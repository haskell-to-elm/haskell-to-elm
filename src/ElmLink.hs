{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module ElmLink where

import Protolude hiding (All, Infix, Type)

import qualified Bound
import qualified Data.Aeson as Aeson
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.String
import Data.Text (Text)
import Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Language.Elm.Definition (Definition)
import qualified Language.Elm.Definition as Definition
import Language.Elm.Expression (Expression)
import qualified Language.Elm.Expression as Expression
import qualified Language.Elm.Name as Name
import qualified Language.Elm.Pattern as Pattern
import Language.Elm.Type (Type)
import qualified Language.Elm.Type as Type

-------------------------------------------------------------------------------
-- * Classes

class HasElmType a where
  elmType :: Type v
  default elmType :: HasElmDefinition a => Type v
  elmType = Type.Global $ Definition.name $ elmDefinition @a

class HasElmDefinition a where
  elmDefinition :: Definition

class HasElmDecoderDefinition value a where
  elmDecoderDefinition :: Definition

class HasElmEncoderDefinition value a where
  elmEncoderDefinition :: Definition

class HasElmType a => HasElmDecoder value a where
  elmDecoder :: Expression v
  default elmDecoder :: HasElmDecoderDefinition value a => Expression v
  elmDecoder = Expression.Global $ Definition.name $ elmDecoderDefinition @value @a

class HasElmType a => HasElmEncoder value a where
  elmEncoder :: Expression v
  default elmEncoder :: HasElmEncoderDefinition value a => Expression v
  elmEncoder = Expression.Global $ Definition.name $ elmEncoderDefinition @value @a

-------------------------------------------------------------------------------
-- * Derivers

data Options = Options
  { fieldLabelModifier :: String -> String
  }

defaultOptions :: Options
defaultOptions =
  Options
    { fieldLabelModifier = identity
    }

deriveElmTypeDefinition :: forall a. (HasDatatypeInfo a, All2 HasElmType (Code a)) => Options -> Name.Qualified -> Definition
deriveElmTypeDefinition options name =
  case datatypeInfo (Proxy @a) of
    ADT _mname _tname (Record _cname fields :* Nil) ->
      Definition.Alias name (Type.Record (recordFields fields))

    ADT _mname _tname cs ->
      Definition.Type name (constructors cs)

    Newtype _mname _tname c ->
      Definition.Type name (constructors (c :* Nil))
  where
    recordFields :: All HasElmType xs => NP FieldInfo xs -> [(Name.Field, Type v)]
    recordFields Nil = []
    recordFields (f :* fs) = field f : recordFields fs

    field :: forall x v. HasElmType x => FieldInfo x -> (Name.Field, Type v)
    field (FieldInfo fname) =
      (fromString $ fieldLabelModifier options fname, elmType @x)

    constructors :: All2 HasElmType xss => NP ConstructorInfo xss -> [(Name.Constructor, [Type v])]
    constructors Nil = []
    constructors (c :* cs) = constructor c : constructors cs

    constructor :: forall xs v. All HasElmType xs => ConstructorInfo xs -> (Name.Constructor, [Type v])
    constructor (Constructor cname) = (fromString cname, constructorFields $ shape @_ @xs)
    constructor (Infix _ _ _) = panic "Infix constructors are not supported"
    constructor (Record cname fs) = (fromString cname, [Type.Record $ recordFields fs])

    constructorFields :: All HasElmType xs => Shape xs -> [Type v]
    constructorFields ShapeNil = []
    constructorFields s@(ShapeCons _) = go s
      where
        go :: forall x xs v. (HasElmType x, All HasElmType xs) => Shape (x ': xs) -> [Type v]
        go (ShapeCons s') = elmType @x : constructorFields s'

-- TODO use options
-- sumEncoding
-- omitNothingFields
-- unwrapUnaryRecords
deriveElmJsonDecoder
  :: forall a
  . (HasDatatypeInfo a, HasElmType a, All2 (HasElmDecoder Aeson.Value) (Code a))
  => Aeson.Options
  -> Name.Qualified
  -> Definition
deriveElmJsonDecoder options decoderName =
  Definition.Constant decoderName (Type.App "Json.Decode.Decoder" $ elmType @a) $
  case datatypeInfo (Proxy @a) of
    ADT _mname _tname (Record _cname fields :* Nil) ->
      recordFields fields $
      Expression.App "Json.Decode.Pipeline.decode" $
      case Type.appsView (elmType @a) of
        (Type.Record fieldTypes, _) ->
          explicitRecordConstructor $ fst <$> fieldTypes

        _ ->
            Expression.Global typeName

    ADT _mname _tname cs ->
      decodeConstructors $ constructors cs

    Newtype _mname _tname c ->
      decodeConstructors $ constructors (c :* Nil)
  where
    typeName@(Name.Qualified moduleName_ _) =
      case Type.appsView (elmType @a) of
        (Type.Global tname, _) -> tname

        _ ->
          panic "Can't automatically derive JSON decoder for an anonymous Elm type"

    explicitRecordConstructor
      :: [Name.Field]
      -> Expression v
    explicitRecordConstructor names =
      go mempty names
      where
        go :: HashMap Name.Field v -> [Name.Field] -> Expression v
        go locals fnames =
          case fnames of
            [] ->
              Expression.Record [(name, Expression.Var $ locals HashMap.! name) | name <- names]

            fname:fnames' ->
              Expression.Lam $ Bound.toScope $ go (HashMap.insert fname (Bound.B ()) $ Bound.F <$> locals) fnames'

    recordFields
      :: All (HasElmDecoder Aeson.Value) xs
      => NP FieldInfo xs
      -> Expression v
      -> Expression v
    recordFields Nil e = e
    recordFields (f :* fs) e =
      recordFields fs $ recordField f e

    recordField
      :: forall x v
      . HasElmDecoder Aeson.Value x
      => FieldInfo x
      -> Expression v
      -> Expression v
    recordField (FieldInfo _) e =
      e Expression.|> Expression.App "Json.Decode.Pipeline.required" (elmDecoder @Aeson.Value @x)

    recordFieldNames
      :: All (HasElmDecoder Aeson.Value) xs
      => NP FieldInfo xs
      -> [Name.Field]
    recordFieldNames Nil = []
    recordFieldNames (FieldInfo fname :* fs) =
      fromString (Aeson.fieldLabelModifier options fname) : recordFieldNames fs

    constructorJSONName :: String -> Text
    constructorJSONName = toS . Aeson.constructorTagModifier options

    decodeConstructors :: [(String, [Expression v])] -> Expression v
    decodeConstructors [(constr, constrFields)]
      | not $ Aeson.tagSingleConstructors options =
        let
          qualifiedConstr =
            Expression.Global $ Name.Qualified moduleName_ $ toS constr
        in
        foldl'
          (Expression.|>)
          (Expression.App "Json.Decode.Pipeline.decode" qualifiedConstr)
          constrFields

    decodeConstructors constrs
      | Aeson.allNullaryToStringTag options && allNullary constrs =
        "Json.Decode.string" Expression.|> Expression.Lam
          (Bound.toScope $ Expression.Case (pure $ Bound.B ()) $
            [ ( Pattern.String $ constructorJSONName constr
              , Bound.toScope $ Expression.App "Json.Decode.succeed" qualifiedConstr
              )
            | (constr, _) <- constrs
            , let
                qualifiedConstr =
                  Expression.Global $ Name.Qualified moduleName_ $ toS constr
            ]
            ++
            [ ( Pattern.Wildcard
              , Bound.toScope $ Expression.App "Json.Decode.fail" $ Expression.String "No matching constructor"
              )
            ]
          )

      | otherwise =
        Expression.App "Json.Decode.field" (Expression.String "tag") Expression.|> Expression.Lam
          (Bound.toScope $ Expression.Case (pure $ Bound.B ()) $
            [ ( Pattern.String $ constructorJSONName constr
              , Bound.toScope $
                case fmap (Bound.F . Bound.F) <$> fields of
                  [field] ->
                    Expression.App "Json.Decode.Pipeline.decode" qualifiedConstr Expression.|>
                      Expression.apps "Json.Decode.Pipeline.required" [Expression.String "contents", field]
                  fields' ->
                    foldl'
                      (Expression.|>)
                      (Expression.App "Json.Decode.Pipeline.decode" qualifiedConstr)
                      [Expression.apps
                        "Json.Decode.Pipeline.required"
                        [ Expression.String "contents"
                        , Expression.apps "Json.Decode.index" [Expression.Int index, field]
                        ]
                      | (index, field) <- zip [0..] fields'
                      ]
              )
            | (constr, fields) <- constrs
            , let
                qualifiedConstr =
                  Expression.Global $ Name.Qualified moduleName_ $ toS constr
            ]
            ++
            [ ( Pattern.Wildcard
              , Bound.toScope $ Expression.App "Json.Decode.fail" $ Expression.String "No matching constructor"
              )
            ]
          )

    allNullary :: forall c f. [(c, [f])] -> Bool
    allNullary = all (null . snd)

    constructors
      :: All2 (HasElmDecoder Aeson.Value) xss
      => NP ConstructorInfo xss
      -> [(String, [Expression v])]
    constructors Nil = []
    constructors (c :* cs) = constructor c : constructors cs

    constructor
      :: forall xs v
      . All (HasElmDecoder Aeson.Value) xs
      => ConstructorInfo xs
      -> (String, [Expression v])
    constructor (Constructor cname) =
      (cname, constructorFields $ shape @_ @xs)
    constructor (Infix _ _ _) =
      panic "Infix constructors are not supported"
    constructor (Record cname fs) =
      (cname, [recordFields fs $ explicitRecordConstructor $ recordFieldNames fs])

    constructorFields
      :: All (HasElmDecoder Aeson.Value) xs
      => Shape xs
      -> [Expression v]
    constructorFields ShapeNil = []
    constructorFields s@(ShapeCons _) = go s
      where
        go
          :: forall x xs v
          . (HasElmDecoder Aeson.Value x, All (HasElmDecoder Aeson.Value) xs)
          => Shape (x ': xs)
          -> [Expression v]
        go (ShapeCons s') = elmDecoder @Aeson.Value @x : constructorFields s'

-------------

instance HasElmType Int where
  elmType =
    "Basics.Int"

instance HasElmType Double where
  elmType =
    "Basics.Float"

instance HasElmType Text where
  elmType =
    "String.String"

instance HasElmEncoder Text Text where
  elmEncoder =
    "Basics.identity"

instance HasElmDecoder Text Text where
  elmDecoder =
    "Basics.identity"

instance HasElmEncoder Aeson.Value Int where
  elmEncoder =
    "Json.Encode.int"

instance HasElmDecoder Aeson.Value Int where
  elmDecoder =
    "Json.Decode.int"

instance HasElmEncoder Aeson.Value Double where
  elmEncoder =
    "Json.Encode.double"

instance HasElmDecoder Aeson.Value Double where
  elmDecoder =
    "Json.Decode.double"

instance HasElmEncoder Aeson.Value Text where
  elmEncoder =
    "Json.Encode.int"

instance HasElmDecoder Aeson.Value Text where
  elmDecoder =
    "Json.Decode.string"

instance HasElmType a => HasElmType (Maybe a) where
  elmType =
    Type.App "Maybe.Maybe" (elmType @a)

instance HasElmEncoder Aeson.Value a => HasElmEncoder Aeson.Value (Maybe a) where
  elmEncoder =
    Expression.apps "Maybe.Extra.unwrap" ["Json.Encode.null", elmEncoder @Aeson.Value @a]

instance HasElmDecoder Aeson.Value a => HasElmDecoder Aeson.Value (Maybe a) where
  elmDecoder =
    Expression.App "Json.Decode.nullable" (elmDecoder @Aeson.Value @a)

instance HasElmType a => HasElmType [a] where
  elmType =
    Type.App "List.List" (elmType @a)

-------------

data Test = A Int Double | B Text
  deriving (GHC.Generic)

instance SOP.Generic Test
instance HasDatatypeInfo Test

instance HasElmDefinition Test where
  elmDefinition = deriveElmTypeDefinition @Test defaultOptions { fieldLabelModifier = drop 1 } "Modul.Test"

instance HasElmDecoderDefinition Aeson.Value Test where
  elmDecoderDefinition = deriveElmJsonDecoder @Test Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "Modul.decodeTest"

instance HasElmType Test where

data Test2 = C | D
  deriving (GHC.Generic)

instance SOP.Generic Test2
instance HasDatatypeInfo Test2

instance HasElmDefinition Test2 where
  elmDefinition = deriveElmTypeDefinition @Test2 defaultOptions { fieldLabelModifier = drop 1 } "Modul.Test2"

instance HasElmDecoderDefinition Aeson.Value Test2 where
  elmDecoderDefinition = deriveElmJsonDecoder @Test2 Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "Modul.decodeTest2"

instance HasElmType Test2 where

data Rec = Rec { _x :: Int, _y :: Maybe Int }
  deriving (GHC.Generic)

instance SOP.Generic Rec
instance HasDatatypeInfo Rec

instance HasElmDefinition Rec where
  elmDefinition = deriveElmTypeDefinition @Rec defaultOptions { fieldLabelModifier = drop 1 } "Modul.Rec"
instance HasElmType Rec where

instance HasElmDecoderDefinition Aeson.Value Rec where
  elmDecoderDefinition = deriveElmJsonDecoder @Rec Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "Modul.decodeRec"

instance HasElmDecoder Aeson.Value Rec

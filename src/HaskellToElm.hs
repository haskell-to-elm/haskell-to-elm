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
module HaskellToElm where

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
import qualified Language.Elm.Pretty as Pretty
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

deriveElmJSONDecoder
  :: forall a
  . (HasDatatypeInfo a, HasElmType a, All2 (HasElmDecoder Aeson.Value) (Code a))
  => Options
  -> Aeson.Options
  -> Name.Qualified
  -> Definition
deriveElmJSONDecoder options aesonOptions decoderName =
  Definition.Constant decoderName (Type.App "Json.Decode.Decoder" $ elmType @a) $
  case datatypeInfo (Proxy @a) of
    ADT _mname _tname (Record _cname fields :* Nil) ->
      decodeRecord fields $
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
      (cname, [decodeRecord fs $ explicitRecordConstructor $ recordFieldNames fs])

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

    decodeRecord
      :: All (HasElmDecoder Aeson.Value) xs
      => NP FieldInfo xs
      -> Expression v
      -> Expression v
    decodeRecord (f :* Nil)
      | Aeson.unwrapUnaryRecords aesonOptions =
        unwrappedRecordField f
    decodeRecord fs =
        recordFields fs

    recordFields
      :: All (HasElmDecoder Aeson.Value) xs
      => NP FieldInfo xs
      -> Expression v
      -> Expression v
    recordFields Nil e = e
    recordFields (f :* fs) e =
      recordFields fs $ recordField f e

    isMaybe :: forall t. HasElmType t => Bool
    isMaybe =
      case Type.appsView $ elmType @t of
        (Type.Global "Maybe.Maybe", _) -> True
        _ -> False

    recordField
      :: forall x v
      . HasElmDecoder Aeson.Value x
      => FieldInfo x
      -> Expression v
      -> Expression v
    recordField (FieldInfo fname) e
      | Aeson.omitNothingFields aesonOptions && isMaybe @x =
        e Expression.|>
          Expression.apps
            "Json.Decode.Pipeline.optional"
            [ jsonFieldName fname
            , elmDecoder @Aeson.Value @x
            , "Maybe.Nothing"
            ]

      | otherwise =
        e Expression.|>
          Expression.apps
            "Json.Decode.Pipeline.required"
            [ jsonFieldName fname
            , elmDecoder @Aeson.Value @x
            ]

    unwrappedRecordField
      :: forall x v
      . HasElmDecoder Aeson.Value x
      => FieldInfo x
      -> Expression v
      -> Expression v
    unwrappedRecordField (FieldInfo _) e =
      e Expression.|> elmDecoder @Aeson.Value @x

    recordFieldNames
      :: All (HasElmDecoder Aeson.Value) xs
      => NP FieldInfo xs
      -> [Name.Field]
    recordFieldNames Nil = []
    recordFieldNames (FieldInfo fname :* fs) =
      elmField fname : recordFieldNames fs

    constructorJSONName :: String -> Text
    constructorJSONName = toS . Aeson.constructorTagModifier aesonOptions

    jsonFieldName :: String -> Expression v
    jsonFieldName = Expression.String . toS . Aeson.fieldLabelModifier aesonOptions

    elmField :: String -> Name.Field
    elmField = fromString . fieldLabelModifier options

    decodeConstructors :: [(String, [Expression v])] -> Expression v
    decodeConstructors [(constr, constrFields)]
      | not $ Aeson.tagSingleConstructors aesonOptions =
        let
          qualifiedConstr =
            Expression.Global $ Name.Qualified moduleName_ $ toS constr
        in
        foldl'
          (Expression.|>)
          (Expression.App "Json.Decode.Pipeline.decode" qualifiedConstr)
          [Expression.apps
            "Json.Decode.Pipeline.index"
            [ Expression.Int index
            , field
            ]
          | (index, field) <- zip [0..] constrFields
          ]

    decodeConstructors constrs
      | Aeson.allNullaryToStringTag aesonOptions && allNullary constrs =
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

    decodeConstructors constrs =
      case Aeson.sumEncoding aesonOptions of
        Aeson.TaggedObject tagName contentsName ->
          Expression.apps "Json.Decode.field" [Expression.String $ toS tagName, "Json.Decode.string"] Expression.|>
            Expression.App "Json.Decode.andThen" (Expression.Lam
              (Bound.toScope $ Expression.Case (pure $ Bound.B ()) $
                [ ( Pattern.String $ constructorJSONName constr
                  , Bound.toScope $
                    case fmap (Bound.F . Bound.F) <$> fields of
                      [field] ->
                        Expression.App "Json.Decode.Pipeline.decode" qualifiedConstr Expression.|>
                          Expression.apps "Json.Decode.Pipeline.required" [Expression.String (toS contentsName), field]
                      fields' ->
                        foldl'
                          (Expression.|>)
                          (Expression.App "Json.Decode.Pipeline.decode" qualifiedConstr)
                          [Expression.apps
                            "Json.Decode.Pipeline.required"
                            [ Expression.String (toS contentsName)
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
            ))

        _ -> panic "Only the DataAeson.TaggedObject sumEncoding is currently supported"

    allNullary :: forall c f. [(c, [f])] -> Bool
    allNullary = all (null . snd)

deriveElmJSONEncoder
  :: forall a
  . (HasDatatypeInfo a, HasElmType a, All2 (HasElmEncoder Aeson.Value) (Code a))
  => Options
  -> Aeson.Options
  -> Name.Qualified
  -> Definition
deriveElmJSONEncoder options aesonOptions encoderName =
  Definition.Constant encoderName (Type.Fun (elmType @a) "Json.Encode.Value") $
  Expression.Lam $ Bound.toScope $
    case datatypeInfo (Proxy @a) of
      ADT _mname _tname (Record _cname fields :* Nil) ->
        encodeRecord fields $ pure $ Bound.B ()

      ADT _mname _tname cs ->
        encodeConstructors (constructors cs) (pure $ Bound.B ())

      Newtype _mname _tname c ->
        encodeConstructors (constructors (c :* Nil)) (pure $ Bound.B ())
  where
    (Name.Qualified moduleName_ _) =
      case Type.appsView (elmType @a) of
        (Type.Global tname, _) -> tname

        _ ->
          panic "Can't automatically derive JSON encoder for an anonymous Elm type"

    constructors
      :: All2 (HasElmEncoder Aeson.Value) xss
      => NP ConstructorInfo xss
      -> [(String, [Expression v])]
    constructors Nil = []
    constructors (c :* cs) = constructor c : constructors cs

    constructor
      :: forall xs v
      . All (HasElmEncoder Aeson.Value) xs
      => ConstructorInfo xs
      -> (String, [Expression v])
    constructor (Constructor cname) =
      (cname, constructorFields $ shape @_ @xs)
    constructor (Infix _ _ _) =
      panic "Infix constructors are not supported"
    constructor (Record cname fs) =
      (cname, [Expression.Lam $ Bound.toScope $ encodeRecord fs (pure $ Bound.B ())])

    constructorFields
      :: All (HasElmEncoder Aeson.Value) xs
      => Shape xs
      -> [Expression v]
    constructorFields ShapeNil = []
    constructorFields s@(ShapeCons _) = go s
      where
        go
          :: forall x xs v
          . (HasElmEncoder Aeson.Value x, All (HasElmEncoder Aeson.Value) xs)
          => Shape (x ': xs)
          -> [Expression v]
        go (ShapeCons s') = elmEncoder @Aeson.Value @x : constructorFields s'

    encodeRecord
      :: All (HasElmEncoder Aeson.Value) xs
      => NP FieldInfo xs
      -> Expression v
      -> Expression v
    encodeRecord (f :* Nil) e
      | Aeson.unwrapUnaryRecords aesonOptions =
        unwrappedRecordField f e
    encodeRecord fs e =
      Expression.App "Json.Encode.object" $
        case recordFields fs e of
          (nonNullable, []) ->
            Expression.List nonNullable

          ([], nullable) ->
            Expression.App "List.concat" $ Expression.List nullable

          (nonNullable, nullable) ->
            Expression.apps "Basics.++"
              [ Expression.List nonNullable
              , Expression.App "List.concat" $ Expression.List nullable
              ]

    recordFields
      :: All (HasElmEncoder Aeson.Value) xs
      => NP FieldInfo xs
      -> Expression v
      -> ([Expression v], [Expression v])
    recordFields Nil _ = mempty
    recordFields (f :* fs) e =
      recordField f e <> recordFields fs e

    isMaybe :: forall t. HasElmType t => Bool
    isMaybe =
      case Type.appsView $ elmType @t of
        (Type.Global "Maybe.Maybe", _) -> True
        _ -> False

    recordField
      :: forall x v
      . HasElmEncoder Aeson.Value x
      => FieldInfo x
      -> Expression v
      -> ([Expression v], [Expression v])
    recordField (FieldInfo fname) e
      | Aeson.omitNothingFields aesonOptions && isMaybe @x =
        ( []
        , [ Expression.Case (Expression.App (Expression.Proj $ elmField fname) e) $
            [ ( Pattern.Con "Maybe.Nothing" []
              , Bound.toScope $ Expression.List []
              )
            , ( Pattern.Con "Maybe.Just" [Pattern.Var 0]
              , Bound.toScope $
                Expression.App
                  (elmEncoder @Aeson.Value @x)
                  (Expression.App (Expression.Proj $ elmField fname) (Bound.F <$> e))
              )
            ]
          ]
        )

      | otherwise =
        ( [ Expression.tuple
            (jsonFieldName fname)
            (Expression.App (elmEncoder @Aeson.Value @x) (Expression.App (Expression.Proj $ elmField fname) e))
          ]
        , []
        )

    unwrappedRecordField
      :: forall x v
      . HasElmEncoder Aeson.Value x
      => FieldInfo x
      -> Expression v
      -> Expression v
    unwrappedRecordField (FieldInfo _) =
      Expression.App (elmEncoder @Aeson.Value @x)

    constructorJSONName :: String -> Text
    constructorJSONName = toS . Aeson.constructorTagModifier aesonOptions

    jsonFieldName :: String -> Expression v
    jsonFieldName = Expression.String . toS . Aeson.fieldLabelModifier aesonOptions

    elmField :: String -> Name.Field
    elmField = fromString . fieldLabelModifier options

    elmConstr :: String -> Name.Qualified
    elmConstr = Name.Qualified moduleName_ . fromString

    encodeConstructors :: [(String, [Expression v])] -> Expression v -> Expression v
    encodeConstructors [(constr, constrFields)] expr
      | not $ Aeson.tagSingleConstructors aesonOptions =
        let
          indexedConstrFields = zip [0..] constrFields
        in
        Expression.Case expr $
          [ ( Pattern.Con (elmConstr constr) (Pattern.Var . fst <$> indexedConstrFields)
            , Bound.toScope $
              Expression.App "Json.Encode.list" $
              Expression.List
                [ Expression.App (Bound.F <$> field) (pure $ Bound.B index)
                | (index, field) <- indexedConstrFields
                ]
            )
          ]

    encodeConstructors constrs expr
      | Aeson.allNullaryToStringTag aesonOptions && allNullary constrs =
        Expression.Case expr $
          [ ( Pattern.Con (elmConstr constr) []
            , Bound.toScope $
              Expression.App "Json.Encode.string" $ Expression.String $ constructorJSONName constr
            )
          | (constr, _) <- constrs
          ]

    encodeConstructors constrs expr =
      case Aeson.sumEncoding aesonOptions of
        Aeson.TaggedObject tagName contentsName ->
          Expression.Case expr $
            [ ( Pattern.Con (elmConstr constr) (Pattern.Var . fst <$> indexedConstrFields)
              , Bound.toScope $
                Expression.App "Json.Encode.object" $
                Expression.List
                  [ Expression.tuple
                    (Expression.String (toS tagName))
                    (Expression.App "Json.Encode.string" $ Expression.String $ constructorJSONName constr)
                  , Expression.tuple
                    (Expression.String (toS contentsName)) $
                      Expression.App "Json.Encode.list" $
                      Expression.List $
                      [ Expression.App (Bound.F <$> field) (pure $ Bound.B index)
                      | (index, field) <- indexedConstrFields
                      ]
                  ]
              )
            | (constr, constrFields) <- constrs
            , let
                indexedConstrFields = zip [0..] constrFields
            ]

        _ -> panic "Only the DataAeson.TaggedObject sumEncoding is currently supported"

    allNullary :: forall c f. [(c, [f])] -> Bool
    allNullary = all (null . snd)

-------------

instance HasElmType Int where
  elmType =
    "Basics.Int"

instance HasElmEncoder Aeson.Value Int where
  elmEncoder =
    "Json.Encode.int"

instance HasElmDecoder Aeson.Value Int where
  elmDecoder =
    "Json.Decode.int"

instance HasElmType Double where
  elmType =
    "Basics.Float"

instance HasElmEncoder Aeson.Value Double where
  elmEncoder =
    "Json.Encode.double"

instance HasElmDecoder Aeson.Value Double where
  elmDecoder =
    "Json.Decode.double"

instance HasElmType Bool where
  elmType =
    "Basics.Bool"

instance HasElmEncoder Aeson.Value Bool where
  elmEncoder =
    "Json.Encode.bool"

instance HasElmDecoder Aeson.Value Bool where
  elmDecoder =
    "Json.Decode.bool"

instance HasElmType Text where
  elmType =
    "String.String"

instance HasElmEncoder Text Text where
  elmEncoder =
    "Basics.identity"

instance HasElmDecoder Text Text where
  elmDecoder =
    "Basics.identity"

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
  elmDefinition = deriveElmTypeDefinition @Test defaultOptions { fieldLabelModifier = drop 1 } "Test.Test"

instance HasElmDecoderDefinition Aeson.Value Test where
  elmDecoderDefinition = deriveElmJSONDecoder @Test defaultOptions Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "Test.decode"

instance HasElmEncoderDefinition Aeson.Value Test where
  elmEncoderDefinition = deriveElmJSONEncoder @Test defaultOptions Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "Test.encode"

instance HasElmType Test where

data Test2 = C | D
  deriving (GHC.Generic)

instance SOP.Generic Test2
instance HasDatatypeInfo Test2

instance HasElmDefinition Test2 where
  elmDefinition = deriveElmTypeDefinition @Test2 defaultOptions { fieldLabelModifier = drop 1 } "Test2.Test2"

instance HasElmDecoderDefinition Aeson.Value Test2 where
  elmDecoderDefinition = deriveElmJSONDecoder @Test2 defaultOptions Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "Test2.decode"

instance HasElmEncoderDefinition Aeson.Value Test2 where
  elmEncoderDefinition = deriveElmJSONEncoder @Test2 defaultOptions Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "Test2.encode"

instance HasElmType Test2 where

data Rec = Rec { _x :: Int, _y :: Maybe Int }
  deriving (GHC.Generic)

instance SOP.Generic Rec
instance HasDatatypeInfo Rec

instance HasElmDefinition Rec where
  elmDefinition = deriveElmTypeDefinition @Rec defaultOptions { fieldLabelModifier = drop 1 } "Rec.Rec"
instance HasElmType Rec where

instance HasElmDecoderDefinition Aeson.Value Rec where
  elmDecoderDefinition = deriveElmJSONDecoder @Rec defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "Rec.decode"

instance HasElmEncoderDefinition Aeson.Value Rec where
  elmEncoderDefinition = deriveElmJSONEncoder @Rec defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "Rec.encode"

instance HasElmDecoder Aeson.Value Rec

data SingleConstructor = SingleConstructor Bool Int
  deriving (GHC.Generic)

instance SOP.Generic SingleConstructor
instance HasDatatypeInfo SingleConstructor

instance Aeson.ToJSON SingleConstructor where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance HasElmDefinition SingleConstructor where
  elmDefinition = deriveElmTypeDefinition @SingleConstructor defaultOptions { fieldLabelModifier = drop 1 } "SingleConstructor.SingleConstructor"

instance HasElmDecoderDefinition Aeson.Value SingleConstructor where
  elmDecoderDefinition = deriveElmJSONDecoder @SingleConstructor defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "SingleConstructor.decode"

instance HasElmEncoderDefinition Aeson.Value SingleConstructor where
  elmEncoderDefinition = deriveElmJSONEncoder @SingleConstructor defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 } "SingleConstructor.encode"

instance HasElmType SingleConstructor where

data SingleFieldRecord = SingleFieldRecord { _singleField :: Int }
  deriving (GHC.Generic)

instance SOP.Generic SingleFieldRecord
instance HasDatatypeInfo SingleFieldRecord

instance Aeson.ToJSON SingleFieldRecord where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1, Aeson.unwrapUnaryRecords = False }
  toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1, Aeson.unwrapUnaryRecords = False }

instance HasElmDefinition SingleFieldRecord where
  elmDefinition = deriveElmTypeDefinition @SingleFieldRecord defaultOptions { fieldLabelModifier = drop 1 } "SingleFieldRecord.SingleFieldRecord"

instance HasElmDecoderDefinition Aeson.Value SingleFieldRecord where
  elmDecoderDefinition = deriveElmJSONDecoder @SingleFieldRecord defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1, Aeson.unwrapUnaryRecords = False } "SingleFieldRecord.decode"

instance HasElmEncoderDefinition Aeson.Value SingleFieldRecord where
  elmEncoderDefinition = deriveElmJSONEncoder @SingleFieldRecord defaultOptions { fieldLabelModifier = drop 1 } Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1, Aeson.unwrapUnaryRecords = False } "SingleFieldRecord.encode"

instance HasElmType SingleFieldRecord where

everything :: forall t. (HasElmDefinition t, HasElmEncoderDefinition Aeson.Value t, HasElmDecoderDefinition Aeson.Value t) => [Definition]
everything =
  [ elmDefinition @t
  , elmEncoderDefinition @Aeson.Value @t
  , elmDecoderDefinition @Aeson.Value @t
  ]

test = Pretty.modules $
  concat $
  [ everything @Test
  , everything @Test2
  , everything @Rec
  , everything @SingleConstructor
  , everything @SingleFieldRecord
  ]

{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language PartialTypeSignatures #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
module Language.Haskell.To.Elm where

import qualified Bound
import qualified Data.Aeson as Aeson
import Data.Foldable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Kind
import Data.Maybe (catMaybes)
import Data.String
import Data.Text (Text)
import Data.Time
import Generics.SOP as SOP
import GHC.TypeLits

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

-- | Represents that the corresponding Elm type for the Haskell type @a@ is @'elmType' \@a@.
class HasElmType a where
  elmType :: Type v
  default elmType :: Type v
  elmType =
    Type.Global $
      maybe
        (error "default-implemented 'elmType' without a definition")
        Definition.name $
          elmDefinition @a

  -- | When 'Just', this represents that we can generate the definition for the
  -- Elm type that corresponds to @a@ using @'elmDefinition' \@a@.
  --
  -- See 'deriveElmTypeDefinition' for a way to automatically derive 'elmDefinition'.
  --
  -- When 'Nothing', it means that the type is an already existing Elm type
  -- that does not need to be generated.
  elmDefinition :: Maybe Definition
  elmDefinition =
    Nothing

  {-# minimal elmType | elmDefinition #-}

-- | Represents that the Elm type that corresponds to @a@ has a decoder from
-- @value@, namely @'elmDecoder' \@value \@a@.
class HasElmType a => HasElmDecoder value a where
  elmDecoder :: Expression v
  default elmDecoder :: Expression v
  elmDecoder =
    Expression.Global $
      maybe
        (error "default-implemented 'elmDecoder' without a definition")
        Definition.name $
          elmDecoderDefinition @value @a

  -- | When 'Just', this represents that we can generate the Elm decoder definition
  -- from @value@ for the Elm type that corresponds to @a@.
  --
  -- See 'deriveElmJSONDecoder' for a way to automatically derive
  -- 'elmDecoderDefinition' when @value = 'Aeson.Value'@.
  elmDecoderDefinition :: Maybe Definition
  elmDecoderDefinition =
    Nothing

  {-# minimal elmDecoder | elmDecoderDefinition #-}

-- | Represents that the Elm type that corresponds to @a@ has an encoder into
-- @value@, namely @'elmEncoder' \@value \@a@.
--
-- This class has a default instance for types that satisfy
-- 'HasElmEncoderDefinition', which refers to the name of that definition.
class HasElmType a => HasElmEncoder value a where
  elmEncoder :: Expression v
  default elmEncoder :: Expression v
  elmEncoder =
    Expression.Global $
      maybe
        (error "default-implemented 'elmEncoder' without a definition")
        Definition.name $
          elmEncoderDefinition @value @a

  -- | When 'Just', this represents that we can generate the Elm encoder
  -- definition into @value@ for the Elm type that corresponds to @a@.
  --
  -- See 'deriveElmJSONEncoder' for a way to automatically derive
  -- 'elmEncoderDefinition' when @value = 'Aeson.Value'@.
  elmEncoderDefinition :: Maybe Definition
  elmEncoderDefinition =
    Nothing

  {-# minimal elmEncoder | elmEncoderDefinition #-}

-------------------------------------------------------------------------------
-- * Derivers

-- | Elm code generation options
newtype Options = Options
  { fieldLabelModifier :: String -> String -- ^ Use this function to go from Haskell record field name to Elm record field name.
  }

defaultOptions :: Options
defaultOptions =
  Options
    { fieldLabelModifier = id
    }

-- ** Type definitions

-- | Automatically create an Elm definition given a Haskell type.
--
-- This is suitable for use as the 'elmDefinition' in a 'HasElmType' instance:
--
-- @
-- instance 'HasElmType' MyType where
--   'elmDefinition' =
--     'Just' $ 'deriveElmTypeDefinition' \@MyType 'defaultOptions' \"Api.MyType.MyType\"
-- @
deriveElmTypeDefinition
  :: forall a
  . DeriveParameterisedElmTypeDefinition 0 a
  => Options
  -> Name.Qualified
  -> Definition
deriveElmTypeDefinition =
  deriveParameterisedElmTypeDefinition @0 @a

class DeriveParameterisedElmTypeDefinition numParams a where
  deriveParameterisedElmTypeDefinition :: Options -> Name.Qualified -> Definition

data Parameter (n :: Nat)

parameterName :: Int -> Name.Qualified
parameterName i =
  Name.Qualified ["Haskell", "To", "Elm"] ("Parameter" <> fromString (show i))

instance KnownNat n => HasElmType (Parameter n) where
  elmType =
    Type.Global $ parameterName $ fromIntegral $ natVal $ Proxy @n

instance (DeriveParameterisedElmTypeDefinition (numParams + 1) (f (Parameter numParams))) => DeriveParameterisedElmTypeDefinition numParams (f :: Data.Kind.Type -> b) where
  deriveParameterisedElmTypeDefinition =
    deriveParameterisedElmTypeDefinition @(numParams + 1) @(f (Parameter numParams))

instance (KnownNat numParams, HasDatatypeInfo a, All2 HasElmType (Code a)) => DeriveParameterisedElmTypeDefinition numParams (a :: Data.Kind.Type) where
  deriveParameterisedElmTypeDefinition options name =
    case constructorInfo $ datatypeInfo (Proxy @a) of
      Record _cname fields :* Nil ->
        Definition.Alias name numParams (bindTypeParameters $ Type.Record (recordFields fields))

      cs ->
        Definition.Type name numParams (fmap (fmap bindTypeParameters) <$> constructors cs)
    where
      typeParameterMap :: HashMap Name.Qualified Int
      typeParameterMap =
        HashMap.fromList [(parameterName i, i) | i <- [0..numParams - 1]]

      bindTypeParameters
        :: Type v
        -> Bound.Scope Int Type v
      bindTypeParameters =
        Bound.Scope .
        Type.bind
          (\n -> maybe (Type.Global n) (pure . Bound.B) (HashMap.lookup n typeParameterMap))
          (pure . pure . pure)

      numParams =
        fromIntegral $ natVal $ Proxy @numParams

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
      constructor (Infix _ _ _) = error "Infix constructors are not supported"
      constructor (Record cname fs) = (fromString cname, [Type.Record $ recordFields fs])

      constructorFields :: All HasElmType xs => Shape xs -> [Type v]
      constructorFields ShapeNil = []
      constructorFields s@(ShapeCons _) = go s
        where
          go :: forall x xs v. (HasElmType x, All HasElmType xs) => Shape (x ': xs) -> [Type v]
          go (ShapeCons s') = elmType @x : constructorFields s'

-- ** JSON decoders

-- | Automatically create an Elm JSON decoder definition given a Haskell type.
--
-- This is suitable for use as the 'elmDecoderDefinition' in a
-- @'HasElmDecoder' 'Aeson.Value'@ instance:
--
-- @
-- instance 'HasElmDecoder' 'Aeson.Value' MyType where
--   'elmDecoderDefinition' =
--     Just $ 'deriveElmJSONDecoder' \@MyType 'defaultOptions' 'Aeson.defaultOptions' "Api.MyType.decoder"
-- @
--
-- Uses the given 'Aeson.Options' to match the JSON format of derived
-- 'Aeson.FromJSON' and 'Aeson.ToJSON' instances.
deriveElmJSONDecoder
  :: forall a
  . DeriveParameterisedElmDecoderDefinition 0 Aeson.Value a
  => Options
  -> Aeson.Options
  -> Name.Qualified
  -> Definition
deriveElmJSONDecoder =
  deriveParameterisedElmDecoderDefinition @0 @Aeson.Value @a

class DeriveParameterisedElmDecoderDefinition numParams value a where
  deriveParameterisedElmDecoderDefinition :: Options -> Aeson.Options -> Name.Qualified -> Definition

instance KnownNat n => HasElmDecoder value (Parameter n) where
  elmDecoder =
    Expression.Global $
      parameterName $ fromIntegral $ natVal $ Proxy @n

instance (DeriveParameterisedElmDecoderDefinition (numParams + 1) value (f (Parameter numParams))) => DeriveParameterisedElmDecoderDefinition numParams value (f :: Data.Kind.Type -> b) where
  deriveParameterisedElmDecoderDefinition =
    deriveParameterisedElmDecoderDefinition @(numParams + 1) @value @(f (Parameter numParams))

instance (HasElmType a, KnownNat numParams, HasDatatypeInfo a, All2 (HasElmDecoder Aeson.Value) (Code a))
  => DeriveParameterisedElmDecoderDefinition numParams Aeson.Value (a :: Data.Kind.Type) where
  deriveParameterisedElmDecoderDefinition options aesonOptions decoderName =
    Definition.Constant decoderName numParams parameterisedType $
      parameteriseBody $
        case constructorInfo $ datatypeInfo (Proxy @a) of
          Record _cname fields :* Nil ->
            decodeRecord fields $
            Expression.App "Json.Decode.succeed" $
            case Type.appsView (elmType @a) of
              (Type.Record fieldTypes, _) ->
                explicitRecordConstructor $ fst <$> fieldTypes

              _ ->
                Expression.Global typeName

          cs ->
            decodeConstructors $ constructors cs
    where
      numParams =
        fromIntegral $ natVal $ Proxy @numParams

      parameterisedType :: Bound.Scope Int Type v
      parameterisedType =
        foldr
          (\i (Bound.Scope rest) ->
            Bound.Scope $ Type.Fun (Type.App "Json.Decode.Decoder" $ pure $ Bound.B i) rest
          )
          (Bound.Scope $ Type.App "Json.Decode.Decoder" $ bindTypeParameters $ elmType @a)
          [0..numParams - 1]

      typeParameterMap :: HashMap Name.Qualified Int
      typeParameterMap =
        HashMap.fromList [(parameterName i, i) | i <- [0..numParams - 1]]

      bindTypeParameters
        :: Type v
        -> Type (Bound.Var Int v)
      bindTypeParameters =
        Type.bind
          (\n -> maybe (Type.Global n) (pure . Bound.B) (HashMap.lookup n typeParameterMap))
          (pure . pure)

      parameteriseBody :: Expression v -> Expression v
      parameteriseBody body =
        foldr
          (\i ->
            Expression.Lam .
              Bound.toScope .
              Expression.bind
                (\global ->
                  if global == parameterName i then
                    pure $ Bound.B ()

                  else
                    Expression.Global global
                )
                (pure . pure)
          )
          body
          [0..numParams - 1]

      typeName@(Name.Qualified moduleName_ _) =
        case Type.appsView (elmType @a) of
          (Type.Global tname, _) -> tname

          _ ->
            error "Can't automatically derive JSON decoder for an anonymous Elm type"

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
        error "Infix constructors are not supported"
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
      constructorJSONName = fromString . Aeson.constructorTagModifier aesonOptions

      jsonFieldName :: String -> Expression v
      jsonFieldName = Expression.String . fromString . Aeson.fieldLabelModifier aesonOptions

      elmField :: String -> Name.Field
      elmField = fromString . fieldLabelModifier options

      decodeConstructor :: String -> Expression v -> [Expression v] -> Expression v
      decodeConstructor _ constr [] =
        Expression.App "Json.Decode.succeed" constr

      decodeConstructor contentsName constr [constrField] =
        Expression.App "Json.Decode.succeed" constr Expression.|>
          Expression.apps "Json.Decode.Pipeline.required" [Expression.String (fromString contentsName), constrField]

      decodeConstructor contentsName constr constrFields =
        Expression.apps
          "Json.Decode.field"
          [ Expression.String (fromString contentsName)
          , foldl'
            (Expression.|>)
            (Expression.App "Json.Decode.succeed" constr)
            [ Expression.App
              "Json.Decode.Pipeline.custom"
              (Expression.apps "Json.Decode.index" [Expression.Int index, field])
            | (index, field) <- zip [0..] constrFields
            ]
          ]

      decodeConstructors :: [(String, [Expression v])] -> Expression v
      decodeConstructors [(constr, constrFields)]
        | not $ Aeson.tagSingleConstructors aesonOptions =
          let
            qualifiedConstr =
              Expression.Global $ Name.Qualified moduleName_ $ fromString constr
          in
          case constrFields of
            [constrField] ->
              Expression.apps "Json.Decode.map" [qualifiedConstr, constrField]

            _ ->
              foldl'
                (Expression.|>)
                (Expression.App "Json.Decode.succeed" qualifiedConstr)
                [Expression.App
                  "Json.Decode.Pipeline.custom"
                  (Expression.apps "Json.Decode.index" [Expression.Int index, field])
                | (index, field) <- zip [0..] constrFields
                ]

      decodeConstructors constrs
        | Aeson.allNullaryToStringTag aesonOptions && allNullary constrs =
          "Json.Decode.string" Expression.|> Expression.App "Json.Decode.andThen" (Expression.Lam
            (Bound.toScope $ Expression.Case (pure $ Bound.B ()) $
              [ ( Pattern.String $ constructorJSONName constr
                , Bound.toScope $ Expression.App "Json.Decode.succeed" qualifiedConstr
                )
              | (constr, _) <- constrs
              , let
                  qualifiedConstr =
                    Expression.Global $ Name.Qualified moduleName_ $ fromString constr
              ]
              ++
              [ ( Pattern.Wildcard
                , Bound.toScope $ Expression.App "Json.Decode.fail" $ Expression.String "No matching constructor"
                )
              ]
            ))

      decodeConstructors constrs =
        case Aeson.sumEncoding aesonOptions of
          Aeson.TaggedObject tagName contentsName ->
            Expression.apps "Json.Decode.field" [Expression.String $ fromString tagName, "Json.Decode.string"] Expression.|>
              Expression.App "Json.Decode.andThen" (Expression.Lam
                (Bound.toScope $ Expression.Case (pure $ Bound.B ()) $
                  [ ( Pattern.String $ constructorJSONName constr
                    , Bound.toScope $ decodeConstructor contentsName qualifiedConstr (fmap (Bound.F . Bound.F) <$> fields)
                    )
                | (constr, fields) <- constrs
                , let
                    qualifiedConstr =
                      Expression.Global $ Name.Qualified moduleName_ $ fromString constr
                ]
                ++
                [ ( Pattern.Wildcard
                  , Bound.toScope $ Expression.App "Json.Decode.fail" $ Expression.String "No matching constructor"
                  )
                ]
              ))

          _ -> error "Only the DataAeson.TaggedObject sumEncoding is currently supported"

      allNullary :: forall c f. [(c, [f])] -> Bool
      allNullary = all (null . snd)

-- ** JSON encoders

-- | Automatically create an Elm JSON encoder definition given a Haskell type.
--
-- This is suitable for use as the 'elmEncoderDefinition' in a @'HasElmEncoder 'Aeson.Value'@ instance:
--
-- @
-- instance 'HasElmEncoder' 'Aeson.Value' MyType where
--   'elmEncoderDefinition' =
--     'Just' $ 'deriveElmJSONEncoder' \@MyType 'defaultOptions' 'Aeson.defaultOptions' "Api.MyType.encoder"
-- @
--
-- Uses the given 'Aeson.Options' to match the JSON format of derived
-- 'Aeson.FromJSON' and 'Aeson.ToJSON' instances.
deriveElmJSONEncoder
  :: forall a
  . DeriveParameterisedElmEncoderDefinition 0 Aeson.Value a
  => Options
  -> Aeson.Options
  -> Name.Qualified
  -> Definition
deriveElmJSONEncoder =
  deriveParameterisedElmEncoderDefinition @0 @Aeson.Value @a

class DeriveParameterisedElmEncoderDefinition numParams value a where
  deriveParameterisedElmEncoderDefinition :: Options -> Aeson.Options -> Name.Qualified -> Definition

instance KnownNat n => HasElmEncoder value (Parameter n) where
  elmEncoder =
    Expression.Global $
      parameterName $ fromIntegral $ natVal $ Proxy @n

instance (DeriveParameterisedElmEncoderDefinition (numParams + 1) value (f (Parameter numParams))) => DeriveParameterisedElmEncoderDefinition numParams value (f :: Data.Kind.Type -> b) where
  deriveParameterisedElmEncoderDefinition =
    deriveParameterisedElmEncoderDefinition @(numParams + 1) @value @(f (Parameter numParams))

instance (HasElmType a, KnownNat numParams, HasDatatypeInfo a, All2 (HasElmEncoder Aeson.Value) (Code a))
  => DeriveParameterisedElmEncoderDefinition numParams Aeson.Value (a :: Data.Kind.Type) where
  deriveParameterisedElmEncoderDefinition options aesonOptions encoderName =
    Definition.Constant encoderName numParams parameterisedType $
      parameteriseBody $
        Expression.Lam $ Bound.toScope $
          case constructorInfo $ datatypeInfo (Proxy @a) of
            Record _cname fields :* Nil ->
              encodeRecord fields $ pure $ Bound.B ()

            cs ->
              encodeConstructors (constructors cs) (pure $ Bound.B ())
    where
      numParams =
        fromIntegral $ natVal $ Proxy @numParams

      parameterisedType :: Bound.Scope Int Type v
      parameterisedType =
        foldr
          (\i (Bound.Scope rest) ->
            Bound.Scope $ Type.Fun (Type.Fun (pure $ Bound.B i) "Json.Encode.Value") rest
          )
          (Bound.Scope $ Type.Fun (bindTypeParameters $ elmType @a) "Json.Encode.Value")
          [0..numParams - 1]

      typeParameterMap :: HashMap Name.Qualified Int
      typeParameterMap =
        HashMap.fromList [(parameterName i, i) | i <- [0..numParams - 1]]

      bindTypeParameters
        :: Type v
        -> Type (Bound.Var Int v)
      bindTypeParameters =
        Type.bind
          (\n -> maybe (Type.Global n) (pure . Bound.B) (HashMap.lookup n typeParameterMap))
          (pure . pure)

      parameteriseBody :: Expression v -> Expression v
      parameteriseBody body =
        foldr
          (\i ->
            Expression.Lam .
              Bound.toScope .
              Expression.bind
                (\global ->
                  if global == parameterName i then
                    pure $ Bound.B ()

                  else
                    Expression.Global global
                )
                (pure . pure)
          )
          body
          [0..numParams - 1]

      (Name.Qualified moduleName_ _) =
        case Type.appsView (elmType @a) of
          (Type.Global tname, _) -> tname

          _ ->
            error "Can't automatically derive JSON encoder for an anonymous Elm type"

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
      constructor Infix {} =
        error "Infix constructors are not supported"
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
          , [ Expression.Case (Expression.App (Expression.Proj $ elmField fname) e)
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
      constructorJSONName = fromString . Aeson.constructorTagModifier aesonOptions

      jsonFieldName :: String -> Expression v
      jsonFieldName = Expression.String . fromString . Aeson.fieldLabelModifier aesonOptions

      elmField :: String -> Name.Field
      elmField = fromString . fieldLabelModifier options

      elmConstr :: String -> Name.Qualified
      elmConstr = Name.Qualified moduleName_ . fromString

      encodeConstructorFields :: [Expression v] -> Expression (Bound.Var Int v)
      encodeConstructorFields [constrField] =
        Expression.App (Bound.F <$> constrField) (pure $ Bound.B 0)

      encodeConstructorFields constrFields =
        Expression.apps
          "Json.Encode.list"
          [ "Basics.identity"
          , Expression.List
            [ Expression.App (Bound.F <$> field) (pure $ Bound.B index)
            | (index, field) <- zip [0..] constrFields
            ]
          ]

      encodeConstructors :: [(String, [Expression v])] -> Expression v -> Expression v
      encodeConstructors [(constr, constrFields)] expr
        | not $ Aeson.tagSingleConstructors aesonOptions =
          Expression.Case expr
            [ ( Pattern.Con (elmConstr constr) (Pattern.Var . fst <$> zip [0..] constrFields)
              , Bound.toScope $ encodeConstructorFields constrFields
              )
            ]

      encodeConstructors constrs expr
        | Aeson.allNullaryToStringTag aesonOptions && allNullary constrs =
          Expression.Case expr
            [ ( Pattern.Con (elmConstr constr) []
              , Bound.toScope $
                Expression.App "Json.Encode.string" $ Expression.String $ constructorJSONName constr
              )
            | (constr, _) <- constrs
            ]

      encodeConstructors constrs expr =
        case Aeson.sumEncoding aesonOptions of
          Aeson.TaggedObject tagName contentsName ->
            Expression.Case expr
              [ ( Pattern.Con (elmConstr constr) (Pattern.Var . fst <$> zip [0..] constrFields)
                , Bound.toScope $
                  Expression.App "Json.Encode.object" $
                  Expression.List $
                    Expression.tuple
                      (Expression.String (fromString tagName))
                      (Expression.App "Json.Encode.string" $ Expression.String $ constructorJSONName constr)
                    :
                    [ Expression.tuple
                      (Expression.String (fromString contentsName))
                      (encodeConstructorFields constrFields)
                    | not $ null constrFields
                    ]
                )
              | (constr, constrFields) <- constrs
              ]

          _ -> error "Only the DataAeson.TaggedObject sumEncoding is currently supported"

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
    "Json.Encode.float"

instance HasElmDecoder Aeson.Value Double where
  elmDecoder =
    "Json.Decode.float"

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

instance HasElmEncoder Text Char where
  elmEncoder =
    "String.fromChar"

instance HasElmEncoder Text Int where
  elmEncoder =
    "String.fromInt"

instance HasElmEncoder Text Double where
  elmEncoder =
    "String.fromFloat"

instance HasElmEncoder Aeson.Value Text where
  elmEncoder =
    "Json.Encode.string"

instance HasElmDecoder Aeson.Value Text where
  elmDecoder =
    "Json.Decode.string"

instance HasElmType Char where
  elmType =
    "Char.Char"

instance HasElmEncoder Aeson.Value Char where
  elmEncoder =
    "Json.Encode.string" Expression.<< "String.fromChar"

instance HasElmDecoder Aeson.Value Char where
  elmDecoder =
    "Json.Decode.string" Expression.|>
      Expression.App "Json.Decode.andThen"
      (Expression.Lam $ Bound.toScope $
        Expression.Case
          (Expression.App "String.uncons" $ Expression.Var $ Bound.B ())
          [ ( Pattern.Con "Maybe.Just" [Pattern.tuple (Pattern.Var 0) (Pattern.String "")]
            , Bound.toScope $ Expression.App "Json.Decode.succeed" $ Expression.Var $ Bound.B 0
            )
          , ( Pattern.Wildcard
            , Bound.toScope $ Expression.App "Json.Decode.fail" $ Expression.String "Not a char"
            )
          ]
      )

instance HasElmType UTCTime where
  elmType =
    "Time.Posix"

instance HasElmEncoder Aeson.Value UTCTime where
  elmEncoder =
    "Iso8601.encode"

instance HasElmDecoder Aeson.Value UTCTime where
  elmDecoder =
    "Iso8601.decoder"

instance HasElmEncoder a b => HasElmEncoder (Maybe a) (Maybe b) where
  elmEncoder = Expression.App "Maybe.map" (elmEncoder @a @b)

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

instance HasElmEncoder Aeson.Value a => HasElmEncoder Aeson.Value [a] where
  elmEncoder =
    Expression.App "Json.Encode.list" (elmEncoder @Aeson.Value @a)

instance HasElmDecoder Aeson.Value a => HasElmDecoder Aeson.Value [a] where
  elmDecoder =
    Expression.App "Json.Decode.list" (elmDecoder @Aeson.Value @a)

instance (HasElmType a, HasElmType b) => HasElmType (a, b) where
  elmType =
    Type.apps "Basics.," [elmType @a, elmType @b]

instance (HasElmEncoder Aeson.Value a, HasElmEncoder Aeson.Value b) => HasElmEncoder Aeson.Value (a, b) where
  elmEncoder =
    Expression.Lam $ Bound.toScope $
      Expression.Case (pure $ Bound.B ())
        [ ( Pattern.tuple (Pattern.Var 0) (Pattern.Var 1)
          , Bound.toScope $
            Expression.apps
              "Json.Encode.list"
              [ "Basics.identity"
              , Expression.List
                  [ Expression.App (elmEncoder @Aeson.Value @a) $ pure $ Bound.B 0
                  , Expression.App (elmEncoder @Aeson.Value @b) $ pure $ Bound.B 1
                  ]
              ]
          )
        ]

instance (HasElmDecoder Aeson.Value a, HasElmDecoder Aeson.Value b) => HasElmDecoder Aeson.Value (a, b) where
  elmDecoder =
    Expression.apps
      "Json.Decode.map2"
      [ "Tuple.pair"
      , Expression.apps "Json.Decode.index" [Expression.Int 0, elmDecoder @Aeson.Value @a]
      , Expression.apps "Json.Decode.index" [Expression.Int 1, elmDecoder @Aeson.Value @b]
      ]

-- | A shorthand for a list of the type definitions for
-- @'jsonDefinitions' \@MyType@ is a shorthand for creating a list of its
-- 'elmDefinition', @'elmEncoderDefinition' \@'Aeson.Value'@, and
-- @'elmDecoderDefinition' \@'Aeson.Value'@.
jsonDefinitions :: forall t. (HasElmEncoder Aeson.Value t, HasElmDecoder Aeson.Value t) => [Definition]
jsonDefinitions =
  catMaybes
  [ elmDefinition @t
  , elmEncoderDefinition @Aeson.Value @t
  , elmDecoderDefinition @Aeson.Value @t
  ]

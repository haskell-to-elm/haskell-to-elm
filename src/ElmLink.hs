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

import qualified Data.Aeson as Aeson
import Data.String
import Data.Text (Text)
import Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Language.Elm.Definition (Definition)
import qualified Language.Elm.Definition as Definition
import Language.Elm.Expression (Expression)
import qualified Language.Elm.Expression as Expression
import qualified Language.Elm.Name as Name
import Language.Elm.Type (Type)
import qualified Language.Elm.Type as Type

class HasElmType a where
  elmType :: Type Name.Qualified
  default elmType :: HasElmDefinition a => Type Name.Qualified
  elmType = pure $ Definition.name $ elmDefinition @a

class HasElmDefinition a where
  elmDefinition :: Definition

deriveElmTypeDefinition :: forall a. (HasDatatypeInfo a, All2 HasElmType (Code a)) => Name.Qualified -> Definition
deriveElmTypeDefinition name =
  case datatypeInfo (Proxy @a) of
    ADT _mname _tname (Record _cname fields :* Nil) ->
      Definition.Alias name (Type.Record (recordFields fields))

    ADT _mname _tname cs ->
      Definition.Type name (constructors cs)

    Newtype _mname _tname c ->
      Definition.Type name (constructors (c :* Nil))
  where
    recordFields :: All HasElmType xs => NP FieldInfo xs -> [(Name.Field, Type Name.Qualified)]
    recordFields Nil = []
    recordFields (f :* fs) = field f : recordFields fs

    field :: forall x. HasElmType x => FieldInfo x -> (Name.Field, Type Name.Qualified)
    field (FieldInfo fname) =
      (fromString fname, elmType @x)

    constructors :: All2 HasElmType xss => NP ConstructorInfo xss -> [(Name.Constructor, [Type Name.Qualified])]
    constructors Nil = []
    constructors (c :* cs) = constructor c : constructors cs

    constructor :: forall xs. All HasElmType xs => ConstructorInfo xs -> (Name.Constructor, [Type Name.Qualified])
    constructor (Constructor cname) = (fromString cname, constructorFields $ shape @_ @xs)
    constructor (Infix _ _ _) = panic "Infix constructors are not supported"
    constructor (Record cname fs) = (fromString cname, [Type.Record $ recordFields fs])

    constructorFields :: All HasElmType xs => Shape xs -> [Type Name.Qualified]
    constructorFields ShapeNil = []
    constructorFields s@(ShapeCons _) = go s
      where
        go :: forall x xs. (HasElmType x, All HasElmType xs) => Shape (x ': xs) -> [Type Name.Qualified]
        go (ShapeCons s') = elmType @x : constructorFields s'

class HasElmDecoderDefinition value a where
  elmDecoderDefinition :: Definition

class HasElmEncoderDefinition value a where
  elmEncoderDefinition :: Definition

class HasElmType a => HasElmDecoder value a where
  elmDecoder :: Expression Name.Qualified
  default elmDecoder :: HasElmDecoderDefinition value a => Expression Name.Qualified
  elmDecoder = pure $ Definition.name $ elmDecoderDefinition @value @a

class HasElmType a => HasElmEncoder value a where
  elmEncoder :: Expression Name.Qualified
  default elmEncoder :: HasElmEncoderDefinition value a => Expression Name.Qualified
  elmEncoder = pure $ Definition.name $ elmEncoderDefinition @value @a

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

instance HasElmEncoder Aeson.Value Text where
  elmEncoder =
    "Json.Encode.string"

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
  elmDefinition = deriveElmTypeDefinition @Test "Modul.Test"

instance HasElmType Test where

data Rec = Rec { x :: Int, y :: Maybe Int }
  deriving (GHC.Generic)

instance SOP.Generic Rec
instance HasDatatypeInfo Rec

instance HasElmDefinition Rec where
  elmDefinition = deriveElmTypeDefinition @Rec "Modul.Rec"
instance HasElmType Rec where

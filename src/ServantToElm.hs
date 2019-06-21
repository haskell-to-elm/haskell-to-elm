{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module ServantToElm where

import Protolude

import qualified Bound
import qualified Data.Aeson as Aeson
import Servant.API.Modifiers
import Servant.Foreign

import HaskellToElm
import Language.Elm.Expression (Expression)
import qualified Language.Elm.Expression as Expression
import qualified Language.Elm.Pattern as Pattern

data Elm

instance HasElmEncoder Aeson.Value a => HasForeignArgument Elm '[ JSON ] (Expression v) a where
  argumentFor Proxy Proxy Proxy Proxy =
    elmEncoder @Aeson.Value @a

instance HasElmEncoder (RequiredArgument mods Text) a => HasForeignArgument Elm '[ Header' mods sym a' ] (Expression v) a where
  argumentFor Proxy Proxy Proxy Proxy =
    elmEncoder @(RequiredArgument mods Text) @a

instance HasElmEncoder (RequiredArgument mods Text) a => HasForeignArgument Elm '[ QueryParam' mods sym a' ] (Expression v) a where
  argumentFor Proxy Proxy Proxy Proxy =
    elmEncoder @(RequiredArgument mods Text) @a

instance HasElmEncoder Text a => HasForeignArgument Elm '[ Capture' mods sym a ] (Expression v) a where
  argumentFor Proxy Proxy Proxy Proxy =
    elmEncoder @Text @a

instance HasElmEncoder Text a => HasForeignArgument Elm '[ CaptureAll sym a ] (Expression v) [a] where
  argumentFor Proxy Proxy Proxy Proxy =
    elmEncoder @Text @a

instance HasElmEncoder Text a => HasForeignArgument Elm '[ QueryParams sym a ] (Expression v) [a] where
  argumentFor Proxy Proxy Proxy Proxy =
    elmEncoder @Text @a

instance HasForeignArgument Elm '[ QueryFlag sym ] (Expression v) Bool where
  argumentFor Proxy Proxy Proxy Proxy =
    "Basics.identity"

instance HasElmDecoder Aeson.Value a => HasForeignResult Elm '[ JSON ] (Expression v) a where
  resultFor Proxy Proxy Proxy Proxy =
    elmDecoder @Aeson.Value @a

instance HasElmType NoContent where
  elmType =
    "NoContent.NoContent"

instance HasElmDecoder Aeson.Value NoContent where
  elmDecoder =
    "Json.Decode.string" Expression.|>
      Expression.App "Json.Decode.andThen"
      (Expression.Lam $ Bound.toScope $
        Expression.Case
          (Expression.Var $ Bound.B ())
          [ ( Pattern.String ""
            , Bound.toScope $ Expression.App "Json.Decode.succeed" "NoContent.NoContent"
            )
          , ( Pattern.Wildcard
            , Bound.toScope $ Expression.App "Json.Decode.fail" $ Expression.String "Expected no content"
            )
          ]
      )

type TestApi
    = "test" :> Header "header" Text :> QueryFlag "flag" :> Get '[JSON] Int
 :<|> "test" :> QueryParam "param" Int :> ReqBody '[JSON] [String] :> Post '[JSON] NoContent
 :<|> "test" :> QueryParams "params" Int :> ReqBody '[JSON] String :> Put '[JSON] NoContent
 :<|> "test" :> Capture "id" Int :> Delete '[JSON] NoContent
 :<|> "test" :> CaptureAll "ids" Int :> Get '[JSON] [Int]
 :<|> "test" :> EmptyAPI

testApi :: [Req (Expression v) (Expression v)]
testApi = listFromAPI (Proxy :: Proxy Elm) (Proxy :: Proxy (Expression v)) (Proxy :: Proxy (Expression v)) (Proxy :: Proxy TestApi)

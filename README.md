# haskell-to-elm [![Build Status](https://travis-ci.com/folq/haskell-to-elm.svg?branch=master)](https://travis-ci.com/folq/haskell-to-elm) [![Hackage](https://img.shields.io/hackage/v/haskell-to-elm.svg)](https://hackage.haskell.org/package/haskell-to-elm)

`haskell-to-elm` is a library that takes Haskell type definitions as input and
generates matching Elm type definitions and JSON encoders and decoders that
match Aeson's format.

## The problem

Let's say we're building a web page with a Haskell backend and an Elm frontend.

We might have a Haskell type like this, that we pass to the frontend encoded as
JSON. The JSON encoder is derived using the Aeson library.

```haskell
data User = User
  { name :: Text
  , age :: Int
  } deriving (Generic, ToJSON)
```

We mirror the type on the Elm side and add a JSON decoder as follows:

```elm
type alias User =
    { name : String
    , age : Int
    }

decoder : Decoder User
decoder =
    Decode.map2 User
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)
```

Now, let's say we want to change a field in the backend:

```haskell
-- Haskell
data User = User
  { name :: Text
--, age :: Int
  , birthday :: Date -- <---- new!
  } deriving (Generic, ToJSON)
```

If we now run the application again, but forget to update the Elm code, the
`User` decoder will fail at runtime in Elm.

## The solution

`haskell-to-elm` solves this problem by letting us _generate_ the Elm `User`
type and `decoder` from the Haskell `User` type.

With `haskell-to-elm` as part of your build pipeline you can make sure that the
frontend is always in sync with your backend, and get type errors in your
frontend code when you change your backend types.

The companion library [servant-to-elm](https://github.com/folq/servant-to-elm) also
lets you generate Elm client libraries for your Servant APIs.

## Basic usage

To generate code for the `User` type above, we first need to derive a bunch of class instances:

```haskell
data User = User
  { name :: Text
  , age :: Int
  } deriving (Generic, Aeson.ToJSON, SOP.Generic, SOP.HasDatatypeInfo)

instance HasElmType User where
  elmDefinition =
    Just $ deriveElmTypeDefinition @User defaultOptions "Api.User.User"

instance HasElmDecoder Aeson.Value User where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @User defaultOptions Aeson.defaultOptions "Api.User.decoder"

instance HasElmEncoder Aeson.Value User where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @User defaultOptions Aeson.defaultOptions "Api.User.encoder"
```

Then we can print the generated Elm code using the following code:

```haskell
main :: IO ()
main = do
  let
    definitions =
      Simplification.simplifyDefinition <$>
        jsonDefinitions @User

    modules =
      Pretty.modules definitions

  forM_ (HashMap.toList modules) $ \(_moduleName, contents) ->
    print contents
```

Running `main` will print the following Elm code:

```elm
module Api.User exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias User =
    { name : String, age : Int }


encoder : User -> Json.Encode.Value
encoder a =
    Json.Encode.object [ ("name" , Json.Encode.string a.name)
    , ("age" , Json.Encode.int a.age) ]


decoder : Json.Decode.Decoder User
decoder =
    Json.Decode.succeed User |>
    Json.Decode.Pipeline.required "name" Json.Decode.string |>
    Json.Decode.Pipeline.required "age" Json.Decode.int
```

In an actual project we would be writing the code to disk instead of printing it.

See [this file](examples/User.hs) for the full code with imports.

## Parameterised types

Since version 0.3.0.0, `haskell-to-elm` supports generating code for types with type parameters.

For example, let's say we have the following Haskell type:

```haskell
data Result e a
  = Err e
  | Ok a
  deriving (Generic, Aeson.ToJSON, SOP.Generic, SOP.HasDatatypeInfo)
```

We can derive the corresponding Elm type and JSON encoders and decoder
definitions with the following code:

```haskell
instance HasElmType Result where
  elmDefinition =
    Just $ deriveElmTypeDefinition @Result defaultOptions "Api.Result.Result"

instance HasElmDecoder Aeson.Value Result where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @Result defaultOptions Aeson.defaultOptions "Api.Result.decoder"

instance HasElmEncoder Aeson.Value Result where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @Result defaultOptions Aeson.defaultOptions "Api.Result.encoder"
```

For parameterised types we also have to add instances for how to handle the
type when it's fully applied to type arguments. Like this:

```haskell
instance (HasElmType a, HasElmType b) => HasElmType (Result a b) where
  elmType =
    Type.apps (elmType @Result) [elmType @a, elmType @b]

instance (HasElmDecoder Aeson.Value a, HasElmDecoder Aeson.Value b) => HasElmDecoder Aeson.Value (Result a b) where
  elmDecoder =
    Expression.apps (elmDecoder @Aeson.Value @Result) [elmDecoder @Aeson.Value @a, elmDecoder @Aeson.Value @b]

instance (HasElmEncoder Aeson.Value a, HasElmDecoder Aeson.Value b) => HasElmEncoder Aeson.Value (Result a b) where
  elmEncoder =
    Expression.apps (elmEncoder @Aeson.Value @Result) [elmEncoder @Aeson.Value @a, elmDecoder @Aeson.Value @b]
```

The rationale for having two instances of the classes for each type is that we
both have to describe how the _type_ is defined (with the unapplied instances),
which generates parameterised definitions, and then we describe how to actually
use those parameterised definitions with the applied instances.

These instances print the following code when run:

```elm
module Api.Result exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type Result a b
    = Err a
    | Ok b


encoder : (a -> Json.Encode.Value) -> (b -> Json.Encode.Value) -> Result a b -> Json.Encode.Value
encoder a b c =
    case c of
        Err d ->
            Json.Encode.object [ ("tag" , Json.Encode.string "Err")
            , ("contents" , a d) ]

        Ok d ->
            Json.Encode.object [ ("tag" , Json.Encode.string "Ok")
            , ("contents" , b d) ]


decoder : Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder (Result a b)
decoder a b =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\c -> case c of
        "Err" ->
            Json.Decode.succeed Err |>
            Json.Decode.Pipeline.required "contents" a

        "Ok" ->
            Json.Decode.succeed Ok |>
            Json.Decode.Pipeline.required "contents" b

        _ ->
            Json.Decode.fail "No matching constructor")
```

Notice that the generated encoder and decoder are parameterised by the encoder
and decoder for the type arguments.

See [this file](examples/Parameterised.hs) for the full code with imports.

## Roadmap

- [x] Derive JSON encoders and generically
  - [ ] Support all Aeson options
- [x] Pretty-print the Elm AST
  - [x] Separate pretty printing from code generation: [elm-syntax](https://github.com/folq/elm-syntax)
- [x] Generate Elm modules
- [x] Servant client library generation: [servant-to-elm](https://github.com/folq/servant-to-elm)
- [x] Test that encoding and decoding round-trip: [haskell-to-elm-test](https://github.com/folq/haskell-to-elm-test)
- [x] Support parameterised types

## Related projects

Libraries that use or are used by haskell-to-elm:
- [elm-syntax](https://github.com/folq/elm-syntax) defines Haskell ASTs for Elm's syntax, and lets us pretty-print it.
- [servant-to-elm](https://github.com/folq/servant-to-elm) can be used to generate Elm client libraries from Servant APIs.
- [haskell-to-elm-test](https://github.com/folq/haskell-to-elm-test) does end-to-end testing of this library.

Others:
- [elm-export](http://hackage.haskell.org/package/elm-export)
- [elm-bridge](http://hackage.haskell.org/package/elm-bridge)
- [elm-street](http://hackage.haskell.org/package/elm-street)
- [elminator](https://github.com/sras/elminator)

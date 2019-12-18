# haskell-to-elm

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

The companion library [servant-elm-bidirectional](https://github.com/folq/servant-elm-bidirectional) also
lets you generate Elm client libraries for your Servant APIs.

## Basic usage

To generate code for the `User` type above, we first need to derive a bunch of class instances:

```haskell
data User = User
  { name :: Text
  , age :: Int
  } deriving
    ( Generic
    , Aeson.ToJSON
    , SOP.Generic
    , SOP.HasDatatypeInfo
    , HasElmDecoder Aeson.Value
    , HasElmEncoder Aeson.Value
    , HasElmType
    )

instance HasElmDefinition User where
  elmDefinition =
    deriveElmTypeDefinition @User defaultOptions "Api.User.User"

instance HasElmDecoderDefinition Aeson.Value User where
  elmDecoderDefinition =
    deriveElmJSONDecoder @User defaultOptions Aeson.defaultOptions "Api.User.decoder"

instance HasElmEncoderDefinition Aeson.Value User where
  elmEncoderDefinition =
    deriveElmJSONEncoder @User defaultOptions Aeson.defaultOptions "Api.User.encoder"
```

Then we can print the generated Elm code using the following code:

```haskell
main :: IO ()
main = do
  let
    definitions =
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

## Roadmap

- [x] Derive JSON encoders and generically
  - [ ] Support all Aeson options
- [x] Pretty-print the Elm AST
  - [x] Separate pretty printing from code generation: [elm-syntax](https://github.com/folq/elm-syntax)
- [x] Generate Elm modules
- [x] Servant client library generation: [servant-elm-bidirectional](https://github.com/folq/servant-elm-bidirectional)
- [x] Test that encoding and decoding round-trip: [elm-to-haskell-test](https://github.com/folq/elm-to-haskell-test)

## Related projects

Libraries that use or are used by haskell-to-elm:
- [elm-syntax](https://github.com/folq/elm-syntax) defines Haskell ASTs for Elm's syntax, and lets us pretty-print it.
- [servant-elm-bidirectional](https://github.com/folq/servant-elm-bidirectional) can be used to generate Elm client libraries from Servant APIs.
- [haskell-to-elm-test](https://github.com/folq/haskell-to-elm-test) does end-to-end testing of this library.

Others:
- [elm-export](http://hackage.haskell.org/package/elm-export)
- [elm-bridge](http://hackage.haskell.org/package/elm-bridge)
- [elm-street](http://hackage.haskell.org/package/elm-street)
- [elminator](https://github.com/sras/elminator)

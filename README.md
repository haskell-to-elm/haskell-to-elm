# haskell-to-elm

Work-in-progress library for generating Elm type definitions, encoders, and
decoders from Haskell types. The goal is to be able to generate nice and
precise types on the Elm side.

## Roadmap

- [x] Derive JSON encoders and generically
  - [ ] Support all Aeson options
- [x] Pretty-print the Elm AST
  - [x] Separate pretty printing from code generation: [elm-syntax](https://github.com/folq/elm-syntax)
- [x] Generate Elm modules
- [x] Servant client library generation: [servant-elm-bidirectional](https://github.com/folq/servant-elm-bidirectional)
- [ ] Test that encoding and decoding round-trip with different Aeson settings

## Related projects

Libraries that use or are used by haskell-to-elm:
- [elm-syntax](https://github.com/folq/elm-syntax)
- [servant-elm-bidirectional](https://github.com/folq/servant-elm-bidirectional)
- [servant-foreign-bidirectional](https://github.com/folq/servant-foreign-bidirectional)

Others:
- [elm-export](http://hackage.haskell.org/package/elm-export)
- [elm-bridge](http://hackage.haskell.org/package/elm-bridge)
- [elm-street](http://hackage.haskell.org/package/elm-street)
- [elminator](https://github.com/sras/elminator)

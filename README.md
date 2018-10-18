# Scotty path normalizer

This library provides a [Scotty] action that normalizes the HTTP request target
as if it were a Unix file path. When the path normalization action detects a
path that can be simplified in one of the following ways, it issues a [redirect]
to a more canonical path.

1. Remove trailing slashes: `https://typeclasses.com/contravariance/`
   becomes `https://typeclasses.com/contravariance`
2. Remove double slashes: `https://typeclasses.com//web-servers////lesson-4`
   becomes `https://typeclasses.com/web-servers/lesson-4`
3. Remove `.` segments, because `.` represents "the current directory":
   `https://typeclasses.com/ghc/./scoped-type-variables` becomes
   `https://typeclasses.com/ghc/scoped-type-variables`
4. Remove segments of the form `xyz/..`, because `..` represents "the parent
   directory": `https://typeclasses.com/python/../javascript/monoidal-folds`
   becomes `https://typeclasses.com/javascript/monoidal-folds`

The typical way to apply this to your Scotty server is to put
`addPathNormalizer` at the top of your `ScottyM` app definition.

```haskell
import qualified Web.Scotty as Scotty
import Web.Scotty.PathNormalizer (addPathNormalizer)

main :: IO ()
main =
    Scotty.scotty 3000 $
      do
        addPathNormalizer

        Scotty.get (Scotty.capture "/:word") $
          do
            beam <- param (Data.Text.Lazy.pack "word")
            Scotty.html $ mconcat
                [ Data.Text.Lazy.pack "<h1>Scotty, "
                , beam
                , Data.Text.Lazy.pack " me up!</h1>"
                ]
```

  [Scotty]: https://hackage.haskell.org/package/scotty

  [redirect]: https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/302

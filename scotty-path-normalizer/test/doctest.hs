import Test.DocTest

main :: IO ()
main =
  doctest
    [ "-isrc"
    , "src/Web/Scotty/PathNormalizer.hs"
    ]

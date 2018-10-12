import Test.DocTest

main :: IO ()
main =
  doctest
    [ "-isrc"
    , "-XOverloadedStrings"
    , "src/Web/Scotty/PathNormalizer.hs"
    ]

{-# LANGUAGE ScopedTypeVariables #-}

module Web.Scotty.PathNormalizer where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Encoding as Text
import Data.Text (Text)
import Network.Wai
import Numeric.Natural
import Web.Scotty

anyPath :: RoutePattern
anyPath = function (const (Just []))

addPathNormalizer :: ScottyM ()
addPathNormalizer = get anyPath pathNormalizerAction

pathNormalizerAction :: ActionM ()
pathNormalizerAction =
  do
    req :: Request <- request

    let
        bs :: ByteString = rawPathInfo req

    txt :: Text <-
        case Text.decodeUtf8' bs of
            Left _ -> next
            Right x -> return x

    when (txt == Text.pack "/") next

    slashRemoved :: Text <-
        case Text.stripPrefix (Text.pack "/") txt of
            Nothing -> next
            Just x -> return x

    let
        (normalizedSegments :: [Text], parents, different) =
            pathNormalize (Text.split (== '/') slashRemoved)

    when (parents > 0) next

    when (not different) next

    queryText :: Text <-
        case (Text.decodeUtf8' (rawQueryString req)) of
            Left _ -> next
            Right x -> return x

    let
        redirectUrl =
            foldMap (\x -> Text.pack "/" <> x) normalizedSegments
            <> queryText

    (redirect . LText.fromStrict) redirectUrl

-- |
-- A path that's already normalized:
--
-- >>> pathNormalize [Text.pack "one", Text.pack "two", Text.pack "three"]
-- (["one","two","three"],0,False)
--
-- A path that contains empty segments:
--
-- >>> pathNormalize [Text.pack "", Text.pack "one", Text.pack ".", Text.pack "two", Text.pack "three"]
-- (["one","two","three"],0,True)
--
-- A path that goes "up" a directory:
--
-- >>> pathNormalize [Text.pack "one", Text.pack "two", Text.pack "three", Text.pack "..", Text.pack "four"]
-- (["one","two","four"],0,True)
--
-- A path that goes up too far:
--
-- >>> pathNormalize [Text.pack "one", Text.pack "..", Text.pack "..", Text.pack "two", Text.pack "three"]
-- (["two","three"],1,True)
--
-- The empty string is a normalized path:
--
-- >>> pathNormalize []
-- ([],0,False)

pathNormalize :: [Text] -> ([Text], Natural, Bool)
pathNormalize =
    foldr f ([], 0 :: Natural, False)
  where
    f x (xs, parents, different)
        | x == Text.pack ""   = (    xs, parents,     True)
        | x == Text.pack "."  = (    xs, parents,     True)
        | x == Text.pack ".." = (    xs, parents + 1, True)
        | parents > 0         = (    xs, parents - 1, True)
        | otherwise           = (x : xs, parents,     different)


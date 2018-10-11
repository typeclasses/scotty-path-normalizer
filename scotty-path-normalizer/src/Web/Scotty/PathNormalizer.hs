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

    normalizedSegments <-
        case (pathNormalize (Text.split (== '/') slashRemoved)) of
            Invalid -> next
            AlreadyNormal -> next
            Normalized xs -> return xs

    queryText :: Text <-
        case (Text.decodeUtf8' (rawQueryString req)) of
            Left _ -> next
            Right x -> return x

    let
        redirectUrl =
            foldMap (\x -> Text.pack "/" <> x) normalizedSegments
            <> queryText

    (redirect . LText.fromStrict) redirectUrl

data Result = Invalid | AlreadyNormal | Normalized [Text]
    deriving Show

-- |
-- A path that's already in normal form:
--
-- >>> pathNormalize [Text.pack "one", Text.pack "two", Text.pack "three"]
-- AlreadyNormal
--
-- A path that contains empty segments:
--
-- >>> pathNormalize [Text.pack "", Text.pack "one", Text.pack ".", Text.pack "two", Text.pack "three"]
-- Normalized ["one","two","three"]
--
-- A path that goes "up" a directory:
--
-- >>> pathNormalize [Text.pack "one", Text.pack "two", Text.pack "three", Text.pack "..", Text.pack "four"]
-- Normalized ["one","two","four"]
--
-- A path that goes up too far:
--
-- >>> pathNormalize [Text.pack "one", Text.pack "..", Text.pack "..", Text.pack "two", Text.pack "three"]
-- Invalid
--
-- The empty string is a normalized path:
--
-- >>> pathNormalize []
-- AlreadyNormal

pathNormalize :: [Text] -> Result
pathNormalize segments =

    case (foldr f ([], 0 :: Natural, False) segments) of
        (_, _, False) -> AlreadyNormal
        (xs, 0, _)    -> Normalized xs
        _             -> Invalid

  where
    f x (xs, parents, different)
        | x == Text.pack ""   = (    xs, parents,     True)
        | x == Text.pack "."  = (    xs, parents,     True)
        | x == Text.pack ".." = (    xs, parents + 1, True)
        | parents > 0         = (    xs, parents - 1, True)
        | otherwise           = (x : xs, parents,     different)

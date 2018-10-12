{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Scotty.PathNormalizer
    ( addPathNormalizer
    , pathNormalizerAction
    , NormalizationResult (..)
    , normalizePath
    , normalizeSegmentList
    ) where

import Control.Monad
import Data.Bool
import Data.Either
import Data.Eq
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Maybe
import Data.Ord
import Network.Wai
import Numeric.Natural
import Text.Show
import Web.Scotty

import Data.ByteString (ByteString)
import Data.Text       (Text)
import Prelude         ((+), (-))

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy     as LT
import qualified Data.Text.Lazy.Encoding     as LT

slash, dot, up :: Text
slash = T.pack "/"
dot   = T.pack "."
up    = T.pack ".."

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

    path :: Text <-
        case T.decodeUtf8' bs of
            Left _  -> next
            Right x -> return x

    path' :: Text <- case (normalizePath path) of
        Invalid       -> next
        AlreadyNormal -> next
        Normalized x  -> return x

    queryText :: Text <-
        case (T.decodeUtf8' (rawQueryString req)) of
            Left _  -> next
            Right x -> return x

    redirect (LT.fromStrict (path' `T.append` queryText))

data NormalizationResult a = Invalid | AlreadyNormal | Normalized a
    deriving (Functor, Show)

-- |
-- A path that's already in normal form:
--
-- >>> normalizePath (T.pack "/one/two/three")
-- AlreadyNormal
--
-- A path that contains empty segments:
--
-- >>> normalizePath (T.pack "//one/./two/three/")
-- Normalized "/one/two/three"
--
-- A path that goes "up" a directory:
--
-- >>> normalizePath (T.pack "/one/two/three/../four")
-- Normalized "/one/two/four"
--
-- A path that goes up too far:
--
-- >>> normalizePath (T.pack "/one/../../two/three")
-- Invalid
--
-- The root path is a normalized path:
--
-- >>> normalizePath (T.pack "/")
-- AlreadyNormal
--
-- The empty string is not a valid path:
--
-- >>> normalizePath (T.pack "")
-- Invalid

normalizePath :: Text -> NormalizationResult Text
normalizePath path
    | path == slash  =  AlreadyNormal
    | otherwise      =
        case T.stripPrefix slash path of
            Nothing           -> Invalid
            Just slashRemoved -> joinSegments <$>
                normalizeSegmentList (T.split (== '/') slashRemoved)
  where
    joinSegments :: [Text] -> Text
    joinSegments [] = slash
    joinSegments xs = foldMap (\x -> slash `T.append` x) xs

-- |
-- A path that's already in normal form:
--
-- >>> normalizeSegmentList [T.pack "one", T.pack "two", T.pack "three"]
-- AlreadyNormal
--
-- A path that contains empty segments:
--
-- >>> normalizeSegmentList [T.pack "", T.pack "one", T.pack ".", T.pack "two", T.pack "three"]
-- Normalized ["one","two","three"]
--
-- A path that goes "up" a directory:
--
-- >>> normalizeSegmentList [T.pack "one", T.pack "two", T.pack "three", T.pack "..", T.pack "four"]
-- Normalized ["one","two","four"]
--
-- A path that goes up too far:
--
-- >>> normalizeSegmentList [T.pack "one", T.pack "..", T.pack "..", T.pack "two", T.pack "three"]
-- Invalid
--
-- The empty string is a normalized path:
--
-- >>> normalizeSegmentList []
-- AlreadyNormal

normalizeSegmentList :: [Text] -> NormalizationResult [Text]
normalizeSegmentList segments =

    case (foldr step init segments) of
        State _  _ False -> AlreadyNormal
        State xs 0 _     -> Normalized xs
        _                -> Invalid

data State = State [Text] Natural Bool

init :: State
init = State [] 0 False

step :: Text -> State -> State
step x (State xs parents different)
    | x == T.empty  =  State       xs    parents       True
    | x == dot      =  State       xs    parents       True
    | x == up       =  State       xs   (parents + 1)  True
    | parents > 0   =  State       xs   (parents - 1)  True
    | otherwise     =  State  (x : xs)   parents       different

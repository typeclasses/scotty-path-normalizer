{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Scotty.PathNormalizer where

import Control.Monad
import Data.Bool
import Data.Either
import Data.Eq
import Data.Foldable
import Data.Function
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

    txt :: Text <-
        case T.decodeUtf8' bs of
            Left _  -> next
            Right x -> return x

    when (txt == slash) next

    slashRemoved :: Text <-
        case T.stripPrefix slash txt of
            Nothing -> next
            Just x  -> return x

    normalizedSegments <-
        case (pathNormalize (T.split (== '/') slashRemoved)) of
            Invalid       -> next
            AlreadyNormal -> next
            Normalized xs -> return xs

    queryText :: Text <-
        case (T.decodeUtf8' (rawQueryString req)) of
            Left _  -> next
            Right x -> return x

    let
        redirectUrl =
            foldMap (\x -> slash `T.append` x) normalizedSegments
            `T.append` queryText

    redirect (LT.fromStrict redirectUrl)

data Result = Invalid | AlreadyNormal | Normalized [Text]
    deriving Show

-- |
-- A path that's already in normal form:
--
-- >>> pathNormalize [T.pack "one", T.pack "two", T.pack "three"]
-- AlreadyNormal
--
-- A path that contains empty segments:
--
-- >>> pathNormalize [T.pack "", T.pack "one", T.pack ".", T.pack "two", T.pack "three"]
-- Normalized ["one","two","three"]
--
-- A path that goes "up" a directory:
--
-- >>> pathNormalize [T.pack "one", T.pack "two", T.pack "three", T.pack "..", T.pack "four"]
-- Normalized ["one","two","four"]
--
-- A path that goes up too far:
--
-- >>> pathNormalize [T.pack "one", T.pack "..", T.pack "..", T.pack "two", T.pack "three"]
-- Invalid
--
-- The empty string is a normalized path:
--
-- >>> pathNormalize []
-- AlreadyNormal

pathNormalize :: [Text] -> Result
pathNormalize segments =

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

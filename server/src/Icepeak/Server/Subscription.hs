{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

module Icepeak.Server.Subscription
(
  SubscriptionTree (..),
  broadcast,
  empty,
  subscribe,
  unsubscribe,
  showTree,
)
where

import Data.Aeson (Value)
import Data.Foldable (for_, traverse_)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import qualified Icepeak.Server.Store as Store

-- Keeps subscriptions in a tree data structure, so we can efficiently determine
-- which clients need to be notified for a given update.
data SubscriptionTree id conn =
  SubscriptionTree (HashMap id conn) (HashMap Text (SubscriptionTree id conn))
  deriving (Eq, Functor, Show)

empty :: SubscriptionTree id conn
empty = SubscriptionTree HashMap.empty HashMap.empty

isEmpty :: SubscriptionTree id conn -> Bool
isEmpty (SubscriptionTree here inner) = HashMap.null here && HashMap.null inner

subscribe
  :: (Eq id, Hashable id)
  => [Text]
  -> id
  -> conn
  -> SubscriptionTree id conn
  -> SubscriptionTree id conn
subscribe path subid subval (SubscriptionTree here inner) =
  case path of
    [] -> SubscriptionTree (HashMap.insert subid subval here) inner
    key : pathTail ->
      let
        subscribeInner = subscribe pathTail subid subval
        newInner = HashMap.alter (Just . subscribeInner . fromMaybe empty) key inner
      in
        SubscriptionTree here newInner

unsubscribe
  :: (Eq id, Hashable id)
  => [Text]
  -> id
  -> SubscriptionTree id conn
  -> SubscriptionTree id conn
unsubscribe path subid (SubscriptionTree here inner) =
  case path of
    [] -> SubscriptionTree (HashMap.delete subid here) inner
    key : pathTail ->
      let
        -- Remove the tail from the inner tree (if it exists). If that left the
        -- inner tree empty, remove the key altogether to keep the tree clean.
        justNotEmpty tree = if isEmpty tree then Nothing else Just tree
        unsubscribeInner = justNotEmpty . unsubscribe pathTail subid
        newInner = HashMap.update unsubscribeInner key inner
      in
        SubscriptionTree here newInner

-- Invoke f for all subscribers to the path. The subscribers get passed the
-- subvalue at the path that they are subscribed to.
broadcast :: (state -> Value -> IO ()) -> [Text] -> Value -> SubscriptionTree id state -> IO ()
broadcast f = loop
 where
  loop path value (SubscriptionTree here inner) = do
    traverse_ (`f` value) here
    case path of
      [] ->
        traverse_
          (\(key, subs) -> loop [] (Store.lookupOrNull [key] value) subs)
          (HashMap.toList inner)
      key : pathTail ->
        for_ (HashMap.lookup key inner) $ \subs ->
          loop pathTail (Store.lookupOrNull [key] value) subs

-- Show subscriptions, for debugging purposes.
showTree :: Show id => SubscriptionTree id conn -> String
showTree tree =
  let
    withPrefix prefix (SubscriptionTree here inner) =
      let
        strHere :: String
        strHere = concatMap (\cid -> " * " <> show cid <> "\n") (HashMap.keys here)
        showInner iPrefix t = iPrefix <> "\n" <> withPrefix iPrefix t
        strInner :: String
        strInner = concat $ HashMap.mapWithKey (\key -> showInner (prefix <> "/" <> Text.unpack key)) inner
      in
        strHere <> strInner
  in
    "/\n" <> withPrefix "" tree

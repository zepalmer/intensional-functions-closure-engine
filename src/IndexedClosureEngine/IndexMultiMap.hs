{-# LANGUAGE GADTs #-}
{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IndexedClosureEngine.IndexMultiMap
( IndexMultiMap
, FMultiMap(..)
, empty
, addIndex
, add
, find
, containsIndex
, fold
) where

import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable, cast, typeOf, eqT, (:~:)(..))

import qualified IndexedClosureEngine.CDMap2 as CDMap2
import IndexedClosureEngine.CDMap2 (CDMap2)
import IndexedClosureEngine.Types

-- ---------- External interface ----------------------------------------------

type MultiMap k v = Map k (Set v)

newtype FMultiMap f k v = FMultiMap (MultiMap k (f v))
    deriving (Eq, Ord, Show)

newtype IndexingFunctionWrapper
        (fact :: Type)
        (key :: Type)
        (derivative :: Type) =
    IndexingFunctionWrapper (IndexingFunction fact key derivative)
    deriving (Eq, Ord)

newtype IndexMultiMap fact f =
    IndexMultiMap (CDMap2 Ord (IndexingFunctionWrapper fact) (FMultiMap f))
    deriving (Eq, Ord)

empty :: IndexMultiMap fact f
empty = IndexMultiMap CDMap2.empty

addIndex :: forall fact f key derivative.
            ( Typeable fact
            , Typeable f
            , Typeable key
            , Typeable derivative
            , Ord key
            , Ord derivative
            , Ord (f derivative)
            )
         => IndexingFunction fact key derivative
         -> IndexMultiMap fact f
         -> IndexMultiMap fact f
addIndex idx (IndexMultiMap m) =
    if CDMap2.contains (IndexingFunctionWrapper idx) m then
        IndexMultiMap m
    else
        IndexMultiMap $
            CDMap2.insert (IndexingFunctionWrapper idx) (FMultiMap Map.empty) m

add :: forall fact f key derivative.
       ( Typeable fact
       , Typeable f
       , Typeable key
       , Typeable derivative
       , Ord key
       , Ord derivative
       , Ord (f derivative)
       )
    => IndexingFunction fact key derivative
    -> key
    -> f derivative
    -> IndexMultiMap fact f
    -> IndexMultiMap fact f
add i k v (IndexMultiMap m) =
    case CDMap2.lookup (IndexingFunctionWrapper i) m of
        Just (FMultiMap d) ->
            let s = case Map.lookup k d of
                        Just vs -> Set.insert v vs
                        Nothing -> Set.singleton v
            in
            IndexMultiMap $
                CDMap2.insert
                    (IndexingFunctionWrapper i)
                    (FMultiMap $ Map.insert k s d)
                    m
        Nothing ->
            IndexMultiMap $
                CDMap2.insert
                    (IndexingFunctionWrapper i)
                    (FMultiMap $ Map.singleton k (Set.singleton v))
                    m

find :: forall fact f key derivative.
        ( Typeable fact
        , Typeable key
        , Typeable derivative
        , Ord key
        , Ord derivative
        , Ord (f derivative)
        )
     => IndexingFunction fact key derivative
     -> key
     -> IndexMultiMap fact f
     -> Set (f derivative)
find i k (IndexMultiMap m) =
    case CDMap2.lookup (IndexingFunctionWrapper i) m of
        Just (FMultiMap d) ->
            case Map.lookup k d of
                Just vs -> vs
                Nothing -> Set.empty
        Nothing -> Set.empty

containsIndex :: forall fact f key derivative.
                 ( Typeable fact, Typeable key, Typeable derivative
                 , Ord key, Ord (f derivative)
                 )
              => IndexingFunction fact key derivative
              -> IndexMultiMap fact f
              -> Bool
containsIndex i (IndexMultiMap m) =
    Maybe.isJust $ CDMap2.lookup (IndexingFunctionWrapper i) m

fold :: forall fact f acc.
        (Typeable fact)
     => (forall key derivative.
            ( Typeable (IndexingFunction fact key derivative)
            , Typeable (FMultiMap f key derivative)
            , Typeable key
            , Typeable derivative
            , Ord key
            , Ord derivative
            )
         => acc
         -> IndexingFunction fact key derivative
         -> FMultiMap f key derivative
         -> acc
        )
     -> acc
     -> IndexMultiMap fact f
     -> acc
fold fn acc (IndexMultiMap m) =
    let fn' :: forall key derivative.
               ( Typeable (IndexingFunctionWrapper fact key derivative)
               , Typeable (FMultiMap f key derivative)
               , Typeable key
               , Typeable derivative
               , Ord key
               , Ord derivative
               )
            => acc
            -> IndexingFunctionWrapper fact key derivative
            -> FMultiMap f key derivative
            -> acc
        fn' a (IndexingFunctionWrapper f) = fn a f
    in
    CDMap2.fold fn' acc m

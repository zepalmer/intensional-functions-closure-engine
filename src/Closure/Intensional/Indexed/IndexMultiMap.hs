{-# LANGUAGE GADTs #-}
{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Closure.Intensional.Indexed.IndexMultiMap
( IndexMultiMap
, FMultiMap(..)
, empty
, addIndex
, add
, find
, contains
, containsIndex
, fold
) where

import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)

import Closure.Intensional.Indexed.Types
import qualified Data.CDDMap as CDDMap
import Data.CDDMap (CDDMap)
import Data.TypeFunctions

-- ---------- External interface ----------------------------------------------

type MultiMap k v = Map k (Set v)

newtype FMultiMap f keyAndDerivative =
    FMultiMap (MultiMap (Fst keyAndDerivative) (f (Snd keyAndDerivative)))
deriving instance
    (Eq k, Eq (f v)) => Eq (FMultiMap f (k, v))
deriving instance
    (Ord k, Ord (f v)) => Ord (FMultiMap f (k, v))

newtype IndexingFunctionWrapper
        (fact :: Type)
        (keyAndDerivative :: Type) =
    IndexingFunctionWrapper
        (IndexingFunction fact (Fst keyAndDerivative) (Snd keyAndDerivative))
deriving instance Eq (IndexingFunctionWrapper fact (k, v))
deriving instance Ord (IndexingFunctionWrapper fact (k, v))

type IndexMultiMapConstraints fact f a k v =
    ( k ~ IndexingFunctionWrapper fact a
    , v ~ FMultiMap f a
    , a ~ (Fst a, Snd a)
    , Typeable a
    , Ord (Fst a)
    , Ord (Snd a)
    )
class (IndexMultiMapConstraints fact f a k v)
    => IndexMultiMapConstraintsClass fact f a k v where
instance (IndexMultiMapConstraints fact f a k v)
    => IndexMultiMapConstraintsClass fact f a k v where

newtype IndexMultiMap fact f =
    IndexMultiMap
        (CDDMap
            (IndexMultiMapConstraintsClass fact f)
            (IndexingFunctionWrapper fact)
            (FMultiMap f))
    deriving (Eq, Ord)

empty :: IndexMultiMap fact f
empty = IndexMultiMap CDDMap.empty

addIndex :: forall fact f key derivative.
            ( Typeable (key, derivative)
            , Typeable (IndexingFunctionWrapper fact)
            , Typeable (FMultiMap f)
            , Ord key
            , Ord derivative
            , Ord (f derivative)
            )
         => IndexingFunction fact key derivative
         -> IndexMultiMap fact f
         -> IndexMultiMap fact f
addIndex idx (IndexMultiMap m) =
    if CDDMap.contains
            @(IndexMultiMapConstraintsClass fact f)
            @(IndexingFunctionWrapper fact)
            @(FMultiMap f)
            @(key,derivative)
            (IndexingFunctionWrapper idx)
            m
    then
        IndexMultiMap m
    else
        IndexMultiMap $
            CDDMap.insert
                @(IndexMultiMapConstraintsClass fact f)
                @(IndexingFunctionWrapper fact)
                @(FMultiMap f)
                @(key,derivative)
                (IndexingFunctionWrapper idx)
                (FMultiMap Map.empty)
                m

add :: forall fact f key derivative.
       ( Typeable (key, derivative)
       , Typeable (IndexingFunctionWrapper fact)
       , Typeable (FMultiMap f)
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
    case CDDMap.lookup
            @(IndexMultiMapConstraintsClass fact f)
            @(IndexingFunctionWrapper fact)
            @(FMultiMap f)
            @(key,derivative)
            (IndexingFunctionWrapper i)
            m
            of
        Just (FMultiMap d) ->
            let s = case Map.lookup k d of
                        Just vs -> Set.insert v vs
                        Nothing -> Set.singleton v
            in
            IndexMultiMap $
                CDDMap.insert
                    @(IndexMultiMapConstraintsClass fact f)
                    @(IndexingFunctionWrapper fact)
                    @(FMultiMap f)
                    @(key,derivative)
                    (IndexingFunctionWrapper i)
                    (FMultiMap $ Map.insert k s d)
                    m
        Nothing ->
            IndexMultiMap $
                CDDMap.insert
                    @(IndexMultiMapConstraintsClass fact f)
                    @(IndexingFunctionWrapper fact)
                    @(FMultiMap f)
                    @(key,derivative)
                    (IndexingFunctionWrapper i)
                    (FMultiMap $ Map.singleton k (Set.singleton v))
                    m

find :: forall fact f key derivative.
        ( Typeable (key, derivative)
        , Typeable (IndexingFunctionWrapper fact)
        , Ord key
        )
     => IndexingFunction fact key derivative
     -> key
     -> IndexMultiMap fact f
     -> Set (f derivative)
find i k (IndexMultiMap m) =
    case CDDMap.lookup
            @(IndexMultiMapConstraintsClass fact f)
            @(IndexingFunctionWrapper fact)
            @(FMultiMap f)
            @(key,derivative)
            (IndexingFunctionWrapper i)
            m
            of
        Just (FMultiMap d) ->
            case Map.lookup k d of
                Just vs -> vs
                Nothing -> Set.empty
        Nothing -> Set.empty

containsIndex :: forall fact f key derivative.
                 ( Typeable (key, derivative)
                 , Typeable (IndexingFunctionWrapper fact)
                 )
              => IndexingFunction fact key derivative
              -> IndexMultiMap fact f
              -> Bool
containsIndex i (IndexMultiMap m) =
    Maybe.isJust $
        CDDMap.lookup
            @(IndexMultiMapConstraintsClass fact f)
            @(IndexingFunctionWrapper fact)
            @(FMultiMap f)
            @(key,derivative)
            (IndexingFunctionWrapper i)
            m

contains :: forall fact f key derivative.
            ( Typeable (key, derivative)
            , Typeable (IndexingFunctionWrapper fact)
            , Ord key
            , Ord (f derivative)
            )
         => IndexingFunction fact key derivative
         -> key
         -> f derivative
         -> IndexMultiMap fact f
         -> Bool
contains i k v (IndexMultiMap m) =
    case CDDMap.lookup
            @(IndexMultiMapConstraintsClass fact f)
            @(IndexingFunctionWrapper fact)
            @(FMultiMap f)
            @(key,derivative)
            (IndexingFunctionWrapper i)
            m
            of
        Nothing -> False
        Just (FMultiMap d) ->
            case Map.lookup k d of
                Nothing -> False
                Just vs -> Set.member v vs

fold :: forall fact f acc.
        ()
     => (forall key derivative.
            ( Typeable (key, derivative)
            , Typeable (IndexingFunctionWrapper fact)
            , Typeable (FMultiMap f)
            , IndexMultiMapConstraintsClass fact f
                    (key, derivative)
                    (IndexingFunctionWrapper fact (key, derivative))
                    (FMultiMap f (key, derivative))
            , Ord (IndexingFunctionWrapper fact (key, derivative))
            , Ord (FMultiMap f (key, derivative))
            )
         => acc
         -> IndexingFunction fact key derivative
         -> FMultiMap f (key, derivative)
         -> acc
        )
     -> acc
     -> IndexMultiMap fact f
     -> acc
fold fn acc imm =
    case imm of
      IndexMultiMap m ->
        let fn' :: forall kv.
                   ( Typeable kv
                   , Typeable (IndexingFunctionWrapper fact)
                   , Typeable (FMultiMap f)
                   , IndexMultiMapConstraintsClass fact f
                           kv (IndexingFunctionWrapper fact kv) (FMultiMap f kv)
                   , Ord (FMultiMap f kv)
                   )
                => acc
                -> IndexingFunctionWrapper fact kv
                -> FMultiMap f kv
                -> acc
            fn' a (IndexingFunctionWrapper f) = fn a f
        in
        CDDMap.fold fn' acc m

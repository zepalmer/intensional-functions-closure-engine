{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Closure.Extensional.Indexed.IndexMultiMap
( IndexMultiMap
, IndexMultiMapValue(..)
, empty
, addIndex
, add
, find
, contains
, containsIndex
, fold
)
where

import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable, eqT, typeOf, (:~:)(..))

import Closure.Extensional.Indexed.Types
import qualified Data.CDDMap as CDDMap
import Data.CDDMap (CDDMap)

-- ---------- External interface ----------------------------------------------

type MultiMap k v = Map k (Set v)

newtype IndexMultiMapValue fact f symbol =
    IndexMultiMapValue (MultiMap
                    (IndexKey fact symbol) (f (IndexDerivative fact symbol)))
deriving instance
    (Eq (IndexKey fact symbol), Eq (f (IndexDerivative fact symbol)))
    => Eq (IndexMultiMapValue fact f symbol)
deriving instance
    (Ord (IndexKey fact symbol), Ord (f (IndexDerivative fact symbol)))
    => Ord (IndexMultiMapValue fact f symbol)

data IndexWrapper fact symbol where
    IndexWrapper :: forall fact symbol.
                    ( Typeable symbol
                    , Eq symbol
                    , Ord symbol
                    , Index fact symbol
                    )
                 => symbol -> IndexWrapper fact symbol
instance Eq (IndexWrapper fact symbol) where
    (IndexWrapper (x :: x)) == (IndexWrapper (y :: y))
        = case eqT of
            Just (Refl :: x :~: y) -> x == y
            Nothing -> False
instance Ord (IndexWrapper fact symbol) where
    compare (IndexWrapper (x :: x)) (IndexWrapper (y :: y))
        = case eqT of
            Just (Refl :: x :~: y) -> x `compare` y
            Nothing -> typeOf x `compare` typeOf y

type IndexMultiMapConstraints fact (f :: Type -> Type) symbol k v =
    ( Index fact symbol
    , IndexWrapper fact symbol ~ k
    , IndexMultiMapValue fact f symbol ~ v
    , Ord (IndexKey fact symbol)
    , Ord (IndexDerivative fact symbol)
    , Ord (f (IndexDerivative fact symbol))
    )
class (IndexMultiMapConstraints fact f symbol k v)
    => IndexMultiMapConstraintsClass fact f symbol k v where
instance (IndexMultiMapConstraints fact f symbol k v)
    => IndexMultiMapConstraintsClass fact f symbol k v where

newtype IndexMultiMap fact (f :: Type -> Type) =
    IndexMultiMap
        (CDDMap
            (IndexMultiMapConstraintsClass fact f)
            (IndexWrapper fact)
            (IndexMultiMapValue fact f))
    deriving (Eq, Ord)

empty :: IndexMultiMap fact f
empty = IndexMultiMap CDDMap.empty

addIndex :: forall fact f symbol.
            ( Typeable symbol
            , Typeable (IndexWrapper fact)
            , Typeable (IndexMultiMapValue fact f)
            , Index fact symbol
            , Ord symbol
            , Ord (IndexKey fact symbol)
            , Ord (IndexDerivative fact symbol)
            , Ord (f (IndexDerivative fact symbol))
            )
         => symbol
         -> IndexMultiMap fact f
         -> IndexMultiMap fact f
addIndex idx (IndexMultiMap m) =
    if CDDMap.contains (IndexWrapper idx) m then
        IndexMultiMap m
    else
        IndexMultiMap $
            CDDMap.insert (IndexWrapper idx) (IndexMultiMapValue Map.empty) m

add :: forall fact f symbol.
       ( Typeable symbol
       , Typeable (IndexWrapper fact)
       , Typeable (IndexMultiMapValue fact f)
       , Index fact symbol
       , Ord symbol
       , Ord (IndexKey fact symbol)
       , Ord (IndexDerivative fact symbol)
       , Ord (f (IndexDerivative fact symbol))
       )
    => symbol
    -> IndexKey fact symbol
    -> f (IndexDerivative fact symbol)
    -> IndexMultiMap fact f
    -> IndexMultiMap fact f
add i k v (IndexMultiMap m) =
    case CDDMap.lookup (IndexWrapper i) m of
        Just (IndexMultiMapValue d) ->
            let s = case Map.lookup k d of
                        Just vs -> Set.insert v vs
                        Nothing -> Set.singleton v
            in
            IndexMultiMap $
                CDDMap.insert
                    (IndexWrapper i)
                    (IndexMultiMapValue $ Map.insert k s d)
                    m
        Nothing ->
            IndexMultiMap $
                CDDMap.insert
                    (IndexWrapper i)
                    (IndexMultiMapValue $ Map.singleton k (Set.singleton v))
                    m

find :: forall fact f symbol.
        ( Typeable symbol
        , Typeable (IndexWrapper fact)
        , Index fact symbol
        , Ord symbol
        , Ord (IndexKey fact symbol)
        )
     => symbol
     -> IndexKey fact symbol
     -> IndexMultiMap fact f
     -> Set (f (IndexDerivative fact symbol))
find i k (IndexMultiMap m) =
    case CDDMap.lookup (IndexWrapper i) m of
        Just (IndexMultiMapValue d) ->
            case Map.lookup k d of
                Just vs -> vs
                Nothing -> Set.empty
        Nothing -> Set.empty

containsIndex :: forall fact f symbol.
                 ( Typeable symbol
                 , Typeable (IndexWrapper fact)
                 , Index fact symbol
                 , Ord symbol
                 )
              => symbol
              -> IndexMultiMap fact f
              -> Bool
containsIndex i (IndexMultiMap m) =
    Maybe.isJust $ CDDMap.lookup (IndexWrapper i) m

contains :: forall fact f symbol.
            ( Typeable symbol
            , Typeable (IndexWrapper fact)
            , Index fact symbol
            , Ord symbol
            , Ord (IndexKey fact symbol)
            , Ord (f (IndexDerivative fact symbol))
            )
         => symbol
         -> IndexKey fact symbol
         -> f (IndexDerivative fact symbol)
         -> IndexMultiMap fact f
         -> Bool
contains i k v (IndexMultiMap m) =
    case CDDMap.lookup (IndexWrapper i) m of
        Nothing -> False
        Just (IndexMultiMapValue d) ->
            case Map.lookup k d of
                Nothing -> False
                Just vs -> Set.member v vs

fold :: forall fact f acc.
        ()
     => (forall symbol.
            ( Typeable symbol
            , Typeable (IndexMultiMapValue fact f symbol)
            , Index fact symbol
            , Ord symbol
            , Ord (IndexKey fact symbol)
            , Ord (IndexDerivative fact symbol)
            , Ord (f (IndexDerivative fact symbol))
            )
         => acc
         -> symbol
         -> IndexMultiMapValue fact f symbol
         -> acc
        )
     -> acc
     -> IndexMultiMap fact f
     -> acc
fold fn acc (IndexMultiMap m) =
    let fn' :: forall symbol.
               ( Typeable symbol
               , Typeable (IndexMultiMapValue fact f)
               , Index fact symbol
               , Ord (IndexKey fact symbol)
               , Ord (IndexDerivative fact symbol)
               , Ord (f (IndexDerivative fact symbol))
               )
            => acc
            -> IndexWrapper fact symbol
            -> IndexMultiMapValue fact f symbol
            -> acc
        fn' a (IndexWrapper idx) = fn a idx
    in
    CDDMap.fold fn' acc m
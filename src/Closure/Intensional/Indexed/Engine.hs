{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Closure.Intensional.Indexed.Engine
( Computation
, ComputationT
, Engine(..)
, emptyEngine
, addIndex
, addComputation
, addFact
, addFacts
, isClosed
, step
, close
, getIndexedFact
, IndexingFunction(..)
) where

import Control.Intensional.Applicative
import Control.Intensional.Functor
import Control.Intensional.Monad
import Control.Intensional.Monad.Trans.Coroutine
import Control.Intensional.Monad.Trans.Coroutine.SuspensionFunctors
import Control.Intensional.Runtime
import Control.Intensional.UtilityFunctions
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Typeable (Typeable, eqT, typeOf, (:~:)(..))

import qualified Closure.Intensional.Indexed.IndexMultiMap as IndexMultiMap
import Closure.Intensional.Indexed.IndexMultiMap (IndexMultiMap)
import Closure.Intensional.Indexed.Types

{-
This module defines an *indexed* coroutine computation engine.  Similar to the
engine provided in the Naive module, this engine contains a set of facts and a
set of computations.  Additionally, the engine contains a series of indexing
functions (in the form of intensional functions) which, given a fact, determine
how (and whether) it should be stored in a given index.  For each fact, one
indexing function may generate a key and a derivative value to be mapped from
that key.

Computations in this indexed engine may access the derivatives for facts by
indexing function and its corresponding key by binding the getIndexedFact
expression provided by this module.  This expression operates much like getFact
from the Naive module in that this bind operation will be executed for each
fact derivative (past and future) which is generated by the corresponding
indexing function.
-}

data EngineSuspensionFunctor fact result where
    EngineSuspensionFunctor :: forall fact key input result.
                               ( Typeable fact
                               , Typeable key
                               , Typeable input
                               , Typeable result
                               , Eq key, Ord key
                               , Ord input
                               )
                            => (IndexingFunction fact key input)
                            -> key
                            -> (input ->%Ord result)
                            -> EngineSuspensionFunctor fact result

instance Eq (EngineSuspensionFunctor fact result) where
    (==)
          (EngineSuspensionFunctor (if1 :: if1) (key1 :: key1) (cont1 :: cont1))
          (EngineSuspensionFunctor (if2 :: if2) (key2 :: key2) (cont2 :: cont2))
      = case eqT of
          Just (Refl :: (if1,key1,cont1) :~: (if2,key2,cont2)) ->
              if1 == if2 && key1 == key2 && cont1 == cont2
          Nothing -> False

instance Ord (EngineSuspensionFunctor fact result) where
    compare
          (EngineSuspensionFunctor (if1 :: if1) (key1 :: key1) (cont1 :: cont1))
          (EngineSuspensionFunctor (if2 :: if2) (key2 :: key2) (cont2 :: cont2))
      = case eqT of
          Just (Refl :: (if1,key1,cont1) :~: (if2,key2,cont2)) ->
            (if1,key1,cont1) `compare` (if2,key2,cont2)
          Nothing ->
            typeOf (if1,key1,cont1) `compare` typeOf (if2,key2,cont2)

instance () => IntensionalFunctor (EngineSuspensionFunctor fact) where
    type IntensionalFunctorCF (EngineSuspensionFunctor fact) = Ord
    type IntensionalFunctorMapC (EngineSuspensionFunctor fact) a b =
        ( Typeable b
        )
    itsFmap = \%%Ord f (EngineSuspensionFunctor idxfn key cont) ->
        EngineSuspensionFunctor idxfn key (itsCompose %@% (f,cont))

type ComputationT m fact =
    CoroutineT Ord (EngineSuspensionFunctor fact) m

type Computation m fact = ComputationT m fact (Set fact)

newtype SuspendedComputation m fact =
    SuspendedComputation (EngineSuspensionFunctor fact (Computation m fact))
    deriving (Eq, Ord)

{- An example:
     For an edge of the form S --push(γ)-> T, we have
       IndexingFunction Edge (Vertex,StackElement) Vertex
     which would, for a given edge, tell us a key (Vertex,StackElement) and a
     value to map from that key (Vertex).  Here, the key would describe the
     source vertex and pushed element mapping to the target vertex.  For an
     index tracking edges in the opposite direction, we would use another
     indexing function.
-}

type family EngineIndexKey fact a :: Type where
    EngineIndexKey fact (key,derivative) = IndexingFunction fact key derivative
type family EngineIndexValue fact a :: Type where
    EngineIndexValue fact (key,derivative) = Map key (Set derivative)

-- |These are the constraints that each WorksetItem must carry.  Because input
--  is existential, they cannot be satisfied in the place that the WorksetItem
--  is used.
type WorksetItemConstraints m fact input = -- FIXME: pick a better name
    ( Typeable input -- TODO: are these two constraints necessary?
    , Ord input
    , IntensionalMonadBindC
        m
        (Either
            (EngineSuspensionFunctor fact (Computation m fact))
            (Set fact))
        (Engine m fact)
    )

data WorksetItem m fact where
    WorksetItem :: forall m fact input.
                   ( WorksetItemConstraints m fact input )
                => input
                -> (input ->%Ord (Computation m fact))
                -> WorksetItem m fact

instance (Typeable m, Typeable fact) => Ord (WorksetItem m fact) where
    compare
            (WorksetItem (input1 :: input1) (comp1 :: comp1))
            (WorksetItem (input2 :: input2) (comp2 :: comp2))
        = case eqT of
            Just (Refl :: (input1, comp1) :~: (input2, comp2)) ->
                (input1, comp1) `compare` (input2, comp2)
            Nothing ->
                typeOf (input1, comp1) `compare` typeOf (input2, comp2)
instance (Ord (WorksetItem m fact)) => Eq (WorksetItem m fact) where
    (==) w1 w2 = w1 `compare` w2 == EQ

newtype FactIndexValue v
    = FactIndexValue v
deriving instance (Eq v) => Eq (FactIndexValue v)
deriving instance (Ord v) => Ord (FactIndexValue v)

data ComputationIndexValue m fact v where
    ComputationIndexValue :: (WorksetItemConstraints m fact v)
                          => (v ->%Ord Computation m fact)
                          -> ComputationIndexValue m fact v
deriving instance Eq (ComputationIndexValue m fact v)
deriving instance Ord (ComputationIndexValue m fact v)

data Engine m fact = Engine
    { facts :: Set fact
    , indexedFacts :: IndexMultiMap fact FactIndexValue
    , indexedComputations :: IndexMultiMap fact (ComputationIndexValue m fact)
    , workset :: Set (WorksetItem m fact)
    }

deriving instance (Typeable m, Typeable fact, Eq fact) => Eq (Engine m fact)
deriving instance (Typeable m, Typeable fact, Ord fact) => Ord (Engine m fact)

emptyEngine :: Engine m fact
emptyEngine = Engine { facts = Set.empty
                     , indexedFacts = IndexMultiMap.empty
                     , indexedComputations = IndexMultiMap.empty
                     , workset = Set.empty
                     }

combineFactWithIndex :: forall fact key derivative m.
                        ( Typeable fact
                        , Typeable (key, derivative)
                        , Typeable m
                        , Ord key
                        , Ord derivative
                        )
                     => fact
                     -> IndexingFunction fact key derivative
                     -> Engine m fact
                     -> Maybe (key, derivative, Set (WorksetItem m fact))
combineFactWithIndex fact idxfn engine =
    case idxfn %@ fact of
        Nothing -> Nothing
        Just (key, derivative) ->
            if IndexMultiMap.contains
                    idxfn key (FactIndexValue derivative) (indexedFacts engine)
            then
                Nothing
            else
                let matchingComputations =
                        IndexMultiMap.find idxfn key
                            (indexedComputations engine)
                in
                let newWorksetItems =
                        matchingComputations
                        & Set.map
                            (\(ComputationIndexValue computation) ->
                                WorksetItem derivative computation)
                in
                Just (key, derivative, newWorksetItems)

addIndex :: forall fact key derivative m.
            ( Typeable fact
            , Typeable key
            , Typeable derivative
            , Typeable m
            , Ord fact
            , Ord key
            , Ord derivative
            , WorksetItemConstraints m fact derivative
            )
         => IndexingFunction fact key derivative
         -> Engine m fact
         -> Engine m fact
addIndex idxfn engine =
    -- Start by adding the index function to the engine's maps.  This is
    -- necessary because we fold over those maps to find indexing functions in
    -- other routines and this routine may not otherwise add the indexing
    -- function here.
    let engine' =
          Engine { facts = facts engine
                 , indexedFacts =
                     IndexMultiMap.addIndex idxfn $ indexedFacts engine
                 , indexedComputations =
                     IndexMultiMap.addIndex idxfn $ indexedComputations engine
                 , workset = workset engine
                 }
    in
    -- Now that we've added the new index, let's compute all of the derivatives
    -- that should exist for it from the set of facts we already have.  Add them
    -- to the indexed fact dictionary.
    let (newFactIndexMap, newWorksetItems) =
          Set.fold
            (\fact (m, s) ->
                case combineFactWithIndex fact idxfn engine' of
                    Nothing -> (m, s)
                    Just (key, derivative, items) ->
                        let m' = IndexMultiMap.add
                                    idxfn key (FactIndexValue derivative) m
                        in
                        (m', Set.union s items)
            )
            (indexedFacts engine', Set.empty)
            (facts engine')
    in
    -- That should do it: the engine is now prepared to act on requests for
    -- facts at that index.
    Engine { facts = facts engine'
           , indexedFacts = newFactIndexMap
           , indexedComputations = indexedComputations engine'
           , workset = Set.union newWorksetItems $ workset engine'
           }

addComputation :: forall m fact.
                  ( Typeable fact
                  , Ord fact
                  , Ord (Engine m fact)
                  , IntensionalFunctorCF m ~ Ord
                  , IntensionalMonad m
                  , IntensionalApplicativePureC m (Engine m fact)
                  , IntensionalMonadBindC
                      m
                      (Either (EngineSuspensionFunctor fact
                                    (Computation m fact))
                              (Set fact))
                      (Engine m fact)
                  )
               => Computation m fact
               -> Engine m fact
               -> m (Engine m fact)
addComputation computation engine =
  intensional Ord do
    result <- resume computation
    itsPure %$ case result of
      Left suspended ->
          addSuspended (SuspendedComputation suspended) engine
      Right factSet ->
          Set.foldr addFact engine factSet

addFact :: forall m fact.
           (Typeable fact, Typeable m, Ord fact)
        => fact
        -> Engine m fact
        -> Engine m fact
addFact fact engine =
    -- For each index, produce derivatives.  Then introduce a new workset item
    -- for each new pairing between the derivatives and the indexed computations
    -- corresponding to them.
    let folder :: forall key derivative.
                  ( Typeable (IndexingFunction fact key derivative)
                  , Typeable
                      (IndexMultiMap.FMultiMap FactIndexValue (key,derivative))
                  , Typeable (key, derivative)
                  , Ord key
                  , Ord derivative
                  )
               => (IndexMultiMap fact FactIndexValue, Set (WorksetItem m fact))
               -> IndexingFunction fact key derivative
               -> IndexMultiMap.FMultiMap FactIndexValue (key, derivative)
               -> (IndexMultiMap fact FactIndexValue, Set (WorksetItem m fact))
        folder (newFactIndexMap, newWorksetItems) idxfn factMappings =
            case combineFactWithIndex fact idxfn engine of
                Nothing -> (newFactIndexMap, newWorksetItems)
                Just (key, derivative, newWorksetItems') ->
                    let newFactIndexMap' =
                          IndexMultiMap.add
                            idxfn key (FactIndexValue derivative)
                            newFactIndexMap
                    in
                    ( newFactIndexMap'
                    , Set.union newWorksetItems newWorksetItems'
                    )
    in
    let (updatedFactIndexMap, newWorksetItems) =
          IndexMultiMap.fold
            folder
            (indexedFacts engine, Set.empty)
            (indexedFacts engine)
    in
    Engine { facts = Set.insert fact $ facts engine
           , indexedFacts = updatedFactIndexMap
           , indexedComputations = indexedComputations engine
           , workset = Set.union newWorksetItems $ workset engine
           }

addFacts :: forall m fact.
            (Typeable fact, Typeable m, Ord fact)
         => [fact]
         -> Engine m fact
         -> Engine m fact
addFacts facts engine = foldr addFact engine facts

addSuspended :: forall m fact.
                ( Typeable m
                , IntensionalMonadBindC
                    m
                    (Either (EngineSuspensionFunctor fact (Computation m fact))
                            (Set fact))
                    (Engine m fact)
                )
             => SuspendedComputation m fact
             -> Engine m fact
             -> Engine m fact
addSuspended suspended engine =
    -- This suspended computation is waiting for results from a specific index
    -- and key.  For each derivative of that index and key, produce a workset
    -- item pairing the derivative with this new computation.
    case suspended of
      SuspendedComputation (EngineSuspensionFunctor idxfn key action) ->
        if IndexMultiMap.contains
                idxfn key (ComputationIndexValue action)
                (indexedComputations engine)
        then
            engine
        else
            let derivatives =
                  engine
                  & indexedFacts
                  & IndexMultiMap.find idxfn key
            in
            let newWorksetItems =
                  Set.map
                    (\(FactIndexValue derivative) ->
                        WorksetItem derivative action)
                    derivatives
            in
            Engine { facts = facts engine
                   , indexedFacts = indexedFacts engine
                   , indexedComputations =
                       indexedComputations engine
                       & IndexMultiMap.add
                            idxfn key (ComputationIndexValue action)
                   , workset = Set.union newWorksetItems $ workset engine
                   }

isClosed :: Engine m fact -> Bool
isClosed engine = Set.null $ workset engine

step :: forall m fact.
        ( Typeable fact
        , Ord fact
        , Ord (Engine m fact)
        , IntensionalMonad m
        , IntensionalFunctorCF m ~ Ord
        , IntensionalApplicativePureC m (Engine m fact)
        )
     => Engine m fact
     -> m (Engine m fact)
step engine =
  case Set.minView $ workset engine of
    Nothing -> itsPure %@ engine
    Just ( WorksetItem
              (arg :: arg)
              (action :: arg ->%Ord (Computation m fact))
         , workset'
         ) ->
      let computation = action %@ arg in
      intensional Ord do
        result <- resume computation
        let engine' = engine { workset = workset' }
        itsPure %$ case result of
          Left suspended ->
            addSuspended (SuspendedComputation suspended) engine'
          Right factSet ->
            Set.foldr addFact engine' factSet

close :: ( Typeable fact
         , Ord fact
         , Ord (Engine m fact)
         , IntensionalMonad m
         , IntensionalFunctorCF m ~ Ord
         , IntensionalApplicativePureC m (Engine m fact)
         , IntensionalMonadBindC m (Engine m fact) (Engine m fact)
         )
      => Engine m fact -> m (Engine m fact)
close engine =
  if isClosed engine then itsPure %@ engine else intensional Ord do
    engine' <- step engine
    close engine'

getIndexedFact :: forall m fact key derivative.
                  ( Typeable fact
                  , Typeable key
                  , Typeable derivative
                  , Ord key
                  , Ord derivative
                  , IntensionalMonad m
                  , IntensionalFunctorCF m ~ Ord
                  , IntensionalApplicativePureC m
                      (CoroutineStepResult
                        Ord
                        (EngineSuspensionFunctor fact)
                        m
                        derivative)
                  )
               => IndexingFunction fact key derivative
               -> key
               -> CoroutineT
                    Ord (EngineSuspensionFunctor fact) m derivative
getIndexedFact idxfn key =
    suspend (EngineSuspensionFunctor idxfn key itsPure)

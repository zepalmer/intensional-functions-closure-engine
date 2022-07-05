{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Closure.Extensional.Indexed.Engine
( Index(..)
, Computation(..)
, ComputationStepResult(..)
, Engine(..)
, emptyEngine
, Closure.Extensional.Indexed.Engine.addIndex
, addFact
, addFacts
, addComputation
, step
, close
, onIndex
, finished
)
where

import Data.Function ((&))
import Data.Kind (Type)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable, (:~:)(..), eqT, typeOf)

import Closure.Extensional.Indexed.IndexMultiMap (IndexMultiMap)
import qualified Closure.Extensional.Indexed.IndexMultiMap as IndexMultiMap
import Closure.Extensional.Indexed.Types

data SuspendedComputation (m :: Type -> Type) fact symbol computation where
    SuspendedComputation :: forall m fact symbol computation.
                            (Index fact symbol, Computation m fact computation)
                         => symbol
                         -> IndexKey fact symbol
                         -> computation (IndexDerivative fact symbol)
                         -> SuspendedComputation m fact symbol computation

data ComputationStepResult m fact where
    ComputationFinished :: forall m fact.
                           Set fact
                        -> ComputationStepResult m fact
    ComputationContinues :: forall m fact symbol computation.
                            ( Typeable (IndexKey fact symbol)
                            , Typeable (IndexDerivative fact symbol)
                            , Typeable symbol
                            , Typeable computation
                            , Ord (IndexKey fact symbol)
                            , Ord (IndexDerivative fact symbol)
                            , Ord (computation (IndexDerivative fact symbol))
                            , Ord symbol
                            )
                         => SuspendedComputation m fact symbol computation
                         -> ComputationStepResult m fact

class (Monad m) =>
        Computation m (fact :: Type) (computation :: Type -> Type) where
    compute :: forall input.
               computation input
            -> input
            -> m (ComputationStepResult m fact)

newtype FactIndexValue v
    = FactIndexValue v
deriving instance (Eq v) => Eq (FactIndexValue v)
deriving instance (Ord v) => Ord (FactIndexValue v)

data ComputationIndexValue m fact v where
    ComputationIndexValue :: forall m fact v computation.
                             ( Computation m fact computation
                             , Typeable computation
                             , Typeable v
                             , Eq (computation v)
                             , Ord (computation v)
                             )
                          => computation v
                          -> ComputationIndexValue m fact v
instance Eq (ComputationIndexValue m fact v) where
    (==)
            (ComputationIndexValue (computation :: a))
            (ComputationIndexValue (computation' :: b))
        = case eqT of
            Just (Refl :: a :~: b) ->
                computation == computation'
            Nothing ->
                False
instance Ord (ComputationIndexValue m fact v) where
    compare
            (ComputationIndexValue (computation :: a))
            (ComputationIndexValue (computation' :: b))
        = case eqT of
            Just (Refl :: a :~: b) ->
                computation `compare` computation'
            Nothing ->
                typeOf computation `compare` typeOf computation'

data WorksetItem m fact where
    WorksetItem :: forall m fact input computation.
                   ( Typeable computation
                   , Typeable input
                   , Computation m fact computation
                   , Eq input, Eq (computation input)
                   , Ord input, Ord (computation input)
                   )
                => input
                -> computation input
                -> WorksetItem m fact

instance (Typeable m, Typeable fact) => Eq (WorksetItem m fact) where
    (==)
            (WorksetItem (input1 :: input1) (comp1 :: comp1))
            (WorksetItem (input2 :: input2) (comp2 :: comp2))
        = case eqT of
            Just (Refl :: (input1, comp1) :~: (input2, comp2)) ->
                (input1, comp1) == (input2, comp2)
            Nothing ->
                False
instance (Typeable m, Typeable fact) => Ord (WorksetItem m fact) where
    compare
            (WorksetItem (input1 :: input1) (comp1 :: comp1))
            (WorksetItem (input2 :: input2) (comp2 :: comp2))
        = case eqT of
            Just (Refl :: (input1, comp1) :~: (input2, comp2)) ->
                (input1, comp1) `compare` (input2, comp2)
            Nothing ->
                typeOf (input1, comp1) `compare` typeOf (input2, comp2)

data Engine m fact =
    Engine { facts :: Set fact
           , indexedFacts :: IndexMultiMap fact FactIndexValue
           , indexedComputations ::
                IndexMultiMap fact (ComputationIndexValue m fact)
           , workset :: Set (WorksetItem m fact)
           }

emptyEngine :: Engine m fact
emptyEngine = Engine { facts = Set.empty
                     , indexedFacts = IndexMultiMap.empty
                     , indexedComputations = IndexMultiMap.empty
                     , workset = Set.empty
                     }

combineFactWithIndex :: forall m fact symbol.
                        ( Typeable m
                        , Typeable fact
                        , Typeable symbol
                        , Index fact symbol
                        , Ord fact
                        , Ord symbol
                        , Ord (IndexKey fact symbol)
                        , Ord (IndexDerivative fact symbol)
                        )
                     => fact
                     -> symbol
                     -> Engine m fact
                     -> Maybe ( IndexKey fact symbol
                              , IndexDerivative fact symbol
                              , Set (WorksetItem m fact)
                              )
combineFactWithIndex fact idx engine =
    case index idx fact of
        Nothing -> Nothing
        Just (key, derivative) ->
            if IndexMultiMap.contains
                    idx key (FactIndexValue derivative) (indexedFacts engine)
            then
                Nothing
            else
                let matchingComputations =
                      IndexMultiMap.find idx key (indexedComputations engine)
                in
                let newWorksetItems =
                      matchingComputations
                      & Set.map
                          (\(ComputationIndexValue computation) ->
                              WorksetItem derivative computation)
                in
                Just (key, derivative, newWorksetItems)

addIndex :: ( Typeable m
            , Typeable fact
            , Typeable symbol
            , Typeable (IndexKey fact symbol)
            , Typeable (IndexDerivative fact symbol)
            , Index fact symbol
            , Ord fact
            , Ord symbol
            , Ord (IndexKey fact symbol)
            , Ord (IndexDerivative fact symbol)
            )
         => symbol -> Engine m fact -> Engine m fact
addIndex idx engine =
    -- Start by adding the index function to the engine's maps.  This is
    -- necessary because we fold over those maps to find indexing functions in
    -- other routines and this routine may not otherwise add the indexing
    -- function here.
    let engine' =
          Engine { facts = facts engine
                 , indexedFacts =
                     IndexMultiMap.addIndex idx $ indexedFacts engine
                 , indexedComputations =
                     IndexMultiMap.addIndex idx $ indexedComputations engine
                 , workset = workset engine
                 }
    in
    -- Now that we've added the new index, let's compute all of the derivatives
    -- that should exist for it from the set of facts we already have.  Add them
    -- to the indexed fact dictionary.
    let (newFactIndexMap, newWorksetItems) =
          Set.fold
            (\fact (m, s) ->
                case combineFactWithIndex fact idx engine' of
                    Nothing -> (m, s)
                    Just (key, derivative, items) ->
                        let m' = IndexMultiMap.add
                                    idx key (FactIndexValue derivative) m
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

addFact :: forall m fact.
           (Typeable m, Typeable fact, Ord fact)
        => fact -> Engine m fact -> Engine m fact
addFact fact engine =
    -- For each index, produce derivatives.  Then introduce a new workset item
    -- for each new pairing between the derivatives and the indexed computations
    -- corresponding to them.
    let folder :: forall symbol.
                  ( Typeable symbol
                  , Index fact symbol
                  , Ord symbol
                  , Ord (IndexKey fact symbol)
                  , Ord (IndexDerivative fact symbol)
                  )
               => (IndexMultiMap fact FactIndexValue, Set (WorksetItem m fact))
               -> symbol
               -> IndexMultiMap.IndexMultiMapValue fact FactIndexValue symbol
               -> ( IndexMultiMap fact FactIndexValue
                  , Set (WorksetItem m fact)
                  )
        folder (newFactIndexMap, newWorksetItems) idx factMappings =
            case combineFactWithIndex fact idx engine of
                Nothing -> (newFactIndexMap, newWorksetItems)
                Just (key, derivative, newWorksetItems') ->
                    let newFactIndexMap' =
                          IndexMultiMap.add
                            idx key (FactIndexValue derivative)
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
            (Typeable m, Typeable fact, Ord fact)
         => [fact] -> Engine m fact -> Engine m fact
addFacts facts engine = foldr addFact engine facts

addComputation :: forall m fact.
                  ( Typeable m
                  , Typeable fact
                  , Monad m
                  , Ord fact
                  )
               => m (ComputationStepResult m fact)
               -> Engine m fact
               -> m (Engine m fact)
addComputation computation engine = do
    result <- computation
    return $ case result of
        ComputationContinues suspended ->
            addSuspended suspended engine
        ComputationFinished factSet ->
            Set.foldr addFact engine factSet

addSuspended :: forall m fact symbol computation.
                ( Typeable m
                , Typeable fact
                , Typeable symbol
                , Typeable computation
                , Typeable (IndexKey fact symbol)
                , Typeable (IndexDerivative fact symbol)
                , Ord symbol
                , Ord (computation (IndexDerivative fact symbol))
                , Ord (IndexKey fact symbol)
                , Ord (IndexDerivative fact symbol)
                )
             => SuspendedComputation m fact symbol computation
             -> Engine m fact
             -> Engine m fact
addSuspended suspended engine =
    -- This suspended computation is waiting for results from a specific index
    -- and key.  For each derivative of that index and key, produce a workset
    -- item pairing the derivative with this new computation.
    case suspended of
      SuspendedComputation idx key computation ->
        if IndexMultiMap.contains
                idx key (ComputationIndexValue computation)
                (indexedComputations engine)
        then
            engine
        else
            let derivatives =
                  engine
                  & indexedFacts
                  & IndexMultiMap.find idx key
            in
            let newWorksetItems =
                  Set.map
                    (\(FactIndexValue derivative) ->
                        WorksetItem derivative computation)
                    derivatives
            in
            Engine { facts = facts engine
                   , indexedFacts = indexedFacts engine
                   , indexedComputations =
                       indexedComputations engine
                       & IndexMultiMap.add
                            idx key (ComputationIndexValue computation)
                   , workset = Set.union newWorksetItems $ workset engine
                   }

step :: (Typeable m, Typeable fact, Monad m, Ord fact)
     => Engine m fact -> m (Engine m fact)
step engine =
  case Set.minView $ workset engine of
    Nothing -> return engine
    Just ( WorksetItem arg action, workset' ) ->
      do
        result <- compute action arg
        let engine' = engine { workset = workset' }
        return $ case result of
          ComputationContinues suspended ->
            addSuspended suspended engine'
          ComputationFinished factSet ->
            Set.foldr addFact engine' factSet

isClosed :: Engine m fact -> Bool
isClosed engine = Set.null $ workset engine

close :: (Typeable m, Typeable fact, Monad m, Ord fact)
      => Engine m fact -> m (Engine m fact)
close engine =
    if isClosed engine then return engine else
        do
            engine' <- step engine
            close engine'

onIndex :: forall m fact symbol computation.
           ( Typeable symbol
           , Typeable computation
           , Typeable (IndexKey fact symbol)
           , Typeable (IndexDerivative fact symbol)
           , Index fact symbol
           , Computation m fact computation
           , Ord symbol
           , Ord (IndexKey fact symbol)
           , Ord (IndexDerivative fact symbol)
           , Ord (computation (IndexDerivative fact symbol))
           )
        => symbol
        -> IndexKey fact symbol
        -> computation (IndexDerivative fact symbol)
        -> ComputationStepResult m fact
onIndex index key computation =
    ComputationContinues $ SuspendedComputation index key computation

finished :: forall m fact. Set fact -> ComputationStepResult m fact
finished facts = ComputationFinished facts

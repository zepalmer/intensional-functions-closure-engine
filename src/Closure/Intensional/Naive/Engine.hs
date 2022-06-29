{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

{-
This module defines a simple coroutine computation engine.  The engine is
provided facts and computations which utilize facts.  Stepping on the engine
causes it to execute computations, executing the coroutines with the various
different facts at its disposal until closure is reached.  Computations are
given as intensional monadic expressions under the Ord constraint function.
Facts are utilized by these computations by binding the getFact expression
provided by this module.
-}

module Closure.Intensional.Naive.Engine
( Computation
, SuspendedComputation
, Engine(..)
, emptyEngine
, addComputation
, addFact
, addFacts
, isFinished
, step
, close
, getFact
) where

import Control.Intensional.Applicative
import Control.Intensional.Functor
import Control.Intensional.Monad
import Control.Intensional.Monad.Trans.Coroutine
import Control.Intensional.Monad.Trans.Coroutine.SuspensionFunctors
import Control.Intensional.Runtime
import qualified Data.Set as Set
import Data.Set (Set)

type Computation m fact = CoroutineT Ord (Await Ord fact) m (Set fact)
type SuspendedComputation m fact = Await Ord fact (Computation m fact)

data Engine m fact = Engine
  { facts :: Set fact
  , computations :: Set (SuspendedComputation m fact)
  , workset :: Set (fact, SuspendedComputation m fact)
  }

deriving instance (Eq (SuspendedComputation m fact))
deriving instance (Ord (SuspendedComputation m fact))

deriving instance (Eq fact) => Eq (Engine m fact)
deriving instance (Ord fact) => Ord (Engine m fact)

emptyEngine :: Engine m fact
emptyEngine = Engine { facts = Set.empty
                     , computations = Set.empty
                     , workset = Set.empty
                     }

addComputation :: ( Typeable fact
                  , Ord fact
                  , IntensionalFunctorCF m ~ Ord
                  , IntensionalMonad m
                  , IntensionalApplicativePureC m (Engine m fact)
                  , IntensionalMonadBindC m
                          (Either (SuspendedComputation m fact) (Set fact))
                          (Engine m fact)
                  )
               => Computation m fact -> Engine m fact -> m (Engine m fact)
addComputation computation engine =
  intensional Ord do
    result <- resume computation
    itsPure %$ case result of
      Left suspended -> addSuspended suspended engine
      Right factSet -> Set.foldr addFact engine factSet

addFact :: (Ord fact) => fact -> Engine m fact -> Engine m fact
addFact fact engine =
  Engine { facts = Set.insert fact $ facts engine
         , computations = computations engine
         , workset =
             Set.union (workset engine) (Set.map (fact,) $ computations engine)
         }

addFacts :: (Ord fact) => [fact] -> Engine m fact -> Engine m fact
addFacts facts engine = foldr addFact engine facts

addSuspended :: ( Ord fact
                , IntensionalFunctorCF m ~ Ord
                )
             => SuspendedComputation m fact
             -> Engine m fact
             -> Engine m fact
addSuspended suspended engine =
  Engine { facts = facts engine
          , computations = Set.insert suspended $ computations engine
          , workset = Set.union
                        (workset engine)
                        (Set.map (,suspended) $ facts engine)
          }

isFinished :: Engine m fact -> Bool
isFinished engine = Set.null $ workset engine

step :: ( Typeable fact
        , Ord fact
        , IntensionalMonad m
        , IntensionalFunctorCF m ~ Ord
        , IntensionalApplicativePureC m (Engine m fact)
        , IntensionalMonadBindC m
                (Either (SuspendedComputation m fact) (Set fact))
                (Engine m fact)
        )
     => Engine m fact -> m (Engine m fact)
step engine =
  case Set.minView $ workset engine of
    Nothing -> itsPure %@ engine
    Just ((fact, Await fn), workset') ->
      let engine' = engine { workset = workset' } in
      intensional Ord do
        result <- resume $ fn %@ fact
        itsPure %$ case result of
          Left suspended -> addSuspended suspended engine'
          Right factSet -> Set.foldr addFact engine' factSet

close :: ( Typeable fact
         , Ord fact
         , IntensionalMonad m
         , IntensionalFunctorCF m ~ Ord
         , IntensionalApplicativePureC m (Engine m fact)
         , IntensionalMonadBindC m
                 (Either (SuspendedComputation m fact) (Set fact))
                 (Engine m fact)
         , IntensionalMonadBindC m (Engine m fact) (Engine m fact)
         )
      => Engine m fact -> m (Engine m fact)
close engine =
  if isFinished engine then itsPure %@ engine else intensional Ord do
    engine' <- step engine
    close engine'

getFact :: ( Typeable fact
           , IntensionalMonad m
           , IntensionalFunctorCF m ~ Ord
           , IntensionalApplicativePureC m
              (Either
                ((Await Ord fact) (CoroutineT Ord (Await Ord fact) m fact))
                fact)
           ) => CoroutineT Ord (Await Ord fact) m fact
getFact = await

{-
  intensional Ord do
    x <- getFact
    y <- getFact
    itsPure %$ x + y
-}

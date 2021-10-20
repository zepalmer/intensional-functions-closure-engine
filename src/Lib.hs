{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
( Computation
, SuspendedComputation
, Engine(..)
, empty
, addComputation
, addFact
, isFinished
, step
, close
, getFact
) where

import Control.Intensional
import Control.Intensional.Applicative
import Control.Intensional.Functor
import Control.Intensional.Monad
import Control.Intensional.Monad.Trans.Coroutine
import Control.Intensional.Monad.Trans.Coroutine.SuspensionFunctors
import qualified Data.Set as Set
import Data.Set (Set)

{-
Coroutine engine:
    * One set of facts (e.g. subtype constraints)
    * One set of computations (e.g. monadic expression to establish transitivity)
      * Monadic expression uses await to obtain new facts
      * Computations both await facts and produce facts (via return)
      * Intensionality specifically is the key ingredient that lets us make this
        a *set* and lets us ask whether a "new" await is in fact novel or has
        already been persued.
    * Engine uses worklist to operate on Cartesian product of these sets
      * Engine stops when worklist is empty
    * Initial sets provided by caller
More sophisticated engine:
    * Yield+await where yield signals filter on set of facts
      * Filter is also intensional?
-}

type Computation m fact = CoroutineT Ord (Await Ord fact) m (Set fact)
type SuspendedComputation m fact = Await Ord fact (Computation m fact)

data Engine m fact = Engine
  { facts :: Set fact
  , computations :: Set (SuspendedComputation m fact)
  , workset :: Set (fact, SuspendedComputation m fact)
  }

{-
intensional Ord do
  x :<: y <- getFact
  y' :<: z <- getFact
  itsGuard $ y == y'
  itsPure $ x :<: z
-}

deriving instance (Eq (SuspendedComputation m fact))
deriving instance (Ord (SuspendedComputation m fact))

deriving instance
  ( Eq fact
  , Eq (m (Either (SuspendedComputation m fact) (Set fact)))
  ) => Eq (Engine m fact)
deriving instance
  ( Ord fact
  , Ord (m (Either (SuspendedComputation m fact) (Set fact)))
  ) => Ord (Engine m fact)

empty :: Engine m fact
empty = Engine { facts = Set.empty
               , computations = Set.empty
               , workset = Set.empty
               }

addComputation :: ( Typeable fact
                  , Ord fact
                  , Ord (m (Either (SuspendedComputation m fact) (Set fact)))
                  , IntensionalFunctorCF m ~ Ord
                  , IntensionalMonad m
                  , IntensionalApplicativePureC m (Engine m fact)
                  , IntensionalMonadBindC m
                          (Either (SuspendedComputation m fact) (Set fact))
                          (Engine m fact)
                  )
               => Computation m fact -> Engine m fact -> m (Engine m fact)
addComputation computation engine = intensional Ord do
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

addSuspended :: ( Ord fact
                , IntensionalFunctorCF m ~ Ord
                )
             => SuspendedComputation m fact
             -> Engine m fact
             -> (Engine m fact)
addSuspended suspended engine =
  Engine { facts = facts engine
          , computations = Set.insert suspended $ computations engine
          , workset = Set.union
                        (workset engine)
                        (Set.map (,suspended) $ facts engine)
          }

isFinished :: Engine m fact -> Bool
isFinished engine = Set.null $ workset engine

{-
type family IntensionalMonadBindC ...

inferrable IntensionalMonadBindC m a b

step :: ( Typeable fact
        , Ord fact
        , Ord (m (Either (SuspendedComputation m fact) (Set fact)))
        , Ord (Computation m fact)
        , IntensionalMonad m
        , IntensionalFunctorCF m ~ Ord
        , ??
        )
     =>
-}

step :: ( Typeable fact
        , Ord fact
        , Ord (m (Either (SuspendedComputation m fact) (Set fact)))
        , Ord (Computation m fact)
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
         , Ord (m (Either (SuspendedComputation m fact) (Set fact)))
         , Ord (Computation m fact)
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

-- TODO: BEGIN: move this to the library package
await :: ( Wrappable c
         , Typeable a
         , IntensionalMonad m
         , IntensionalFunctorCF m ~ c
         , IntensionalApplicativePureC m
            (Either ((Await c a) (CoroutineT c (Await c a) m a)) a)
         )
      => CoroutineT c (Await c a) m a
await = suspend (Await itsPure) -- TODO: fix suspend to be intensional?
-- TODO: END: move this to the library package

getFact :: ( Typeable fact
           , IntensionalMonad m
           , IntensionalFunctorCF m ~ Ord
           , IntensionalApplicativePureC m
              (Either
                ((Await Ord fact) (CoroutineT Ord (Await Ord fact) m fact))
                fact)
           ) => CoroutineT Ord (Await Ord fact) m fact
getFact = await

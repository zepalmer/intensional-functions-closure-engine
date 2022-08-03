{-# LANGUAGE IntensionalFunctions #-}

module Tests.Intensional.Indexed.SubtypeClosure where

import Control.Intensional.Monad
import Control.Intensional.Monad.Identity (IntensionalIdentity(..))
import Control.Intensional.Runtime
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit

import Closure.Intensional.Indexed.Engine

data SubtypeConstraint = (:<:) String String deriving (Eq, Ord, Show)

indexByIdentity :: IndexingFunction SubtypeConstraint () SubtypeConstraint
indexByIdentity = \%Ord a -> Just ((), a)

indexByLowerBound :: IndexingFunction SubtypeConstraint String String
indexByLowerBound = \%Ord (a :<: b) -> Just (a, b)

transitivity :: Computation (IntensionalIdentity Ord) SubtypeConstraint
transitivity = intensional Ord do
    (a :<: b) <- getIndexedFact indexByIdentity ()
    c <- getIndexedFact indexByLowerBound b
    itsReturn %@ Set.singleton (a :<: c)

example :: IntensionalIdentity Ord (Set SubtypeConstraint)
example = intensional Ord do
    initialEngine <-
        addComputation transitivity $
            addIndex indexByLowerBound $
            addIndex indexByIdentity emptyEngine
    let engine :: Engine (IntensionalIdentity Ord) SubtypeConstraint
        engine =
          addFacts [ "poodle" :<: "dog"
                   , "wolfhound" :<: "dog"
                   , "siamese" :<: "cat"
                   , "ragdoll" :<: "cat"
                   , "dog" :<: "mammal"
                   , "cat" :<: "mammal"
                   , "mammal" :<: "animal"
                   , "limestone" :<: "rock"
                   ] initialEngine
    engine' <- close engine
    itsReturn %$ facts engine'

tests :: Test
tests =
  TestLabel "intensional indexed subtype closure" $ TestCase $
    assertEqual "for intensional indexed subtype closure"
        (Set.fromList
            [ "cat" :<: "animal"
            , "cat" :<: "mammal"
            , "dog" :<: "animal"
            , "dog" :<: "mammal"
            , "limestone" :<: "rock"
            , "mammal" :<: "animal"
            , "poodle" :<: "animal"
            , "poodle" :<: "dog"
            , "poodle" :<: "mammal"
            , "ragdoll" :<: "animal"
            , "ragdoll" :<: "cat"
            , "ragdoll" :<: "mammal"
            , "siamese" :<: "animal"
            , "siamese" :<: "cat"
            , "siamese" :<: "mammal"
            , "wolfhound" :<: "animal"
            , "wolfhound" :<: "dog"
            , "wolfhound" :<: "mammal"
            ]
        )
        (let IntensionalIdentity facts = example in facts)

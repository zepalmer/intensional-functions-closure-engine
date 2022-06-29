{-# LANGUAGE IntensionalFunctions #-}

module Main where

import Control.Intensional.Monad
import Control.Intensional.Monad.Identity (IntensionalIdentity(..))
import Control.Intensional.Runtime
import Data.Set (Set)
import qualified Data.Set as Set

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

main :: IO ()
main = let IntensionalIdentity set = example in putStrLn $ show set

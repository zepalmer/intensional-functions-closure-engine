{-# LANGUAGE IntensionalFunctions #-}

module Main where

import Control.Intensional.Monad
import Control.Intensional.Monad.Identity (IntensionalIdentity(..))
import Control.Intensional.Runtime
import Data.Set (Set)
import qualified Data.Set as Set

import qualified IndexedClosureEngine as ICE

data SubtypeConstraint = (:<:) String String deriving (Eq, Ord, Show)

indexByIdentity :: ICE.IndexingFunction SubtypeConstraint () SubtypeConstraint
indexByIdentity = \%Ord a -> Just ((), a)

indexByLowerBound :: ICE.IndexingFunction SubtypeConstraint String String
indexByLowerBound = \%Ord (a :<: b) -> Just (a, b)

transitivity :: ICE.Computation (IntensionalIdentity Ord) SubtypeConstraint
transitivity = intensional Ord do
    (a :<: b) <- ICE.getIndexedFact indexByIdentity ()
    c <- ICE.getIndexedFact indexByLowerBound b
    itsReturn %@ Set.singleton (a :<: c)

example :: IntensionalIdentity Ord (Set SubtypeConstraint)
example = intensional Ord do
    initialEngine <-
        ICE.addComputation transitivity $
            ICE.addIndex indexByLowerBound $
            ICE.addIndex indexByIdentity ICE.empty
    let engine :: ICE.Engine (IntensionalIdentity Ord) SubtypeConstraint
        engine =
          ICE.addFacts [ "poodle" :<: "dog"
                       , "wolfhound" :<: "dog"
                       , "siamese" :<: "cat"
                       , "ragdoll" :<: "cat"
                       , "dog" :<: "mammal"
                       , "cat" :<: "mammal"
                       , "mammal" :<: "animal"
                       , "limestone" :<: "rock"
                       ] initialEngine
    engine' <- ICE.close engine
    itsReturn %$ ICE.facts engine'

main :: IO ()
main = let IntensionalIdentity set = example in putStrLn $ show set

{-# LANGUAGE IntensionalFunctions #-}

module Main where

import Control.Intensional.Monad
import Control.Intensional.Monad.Identity (IntensionalIdentity(..))
import Control.Intensional.Runtime
import Data.Function ((&))
import Data.Set (Set)
import qualified Data.Set as Set

import qualified NaiveClosureEngine as NCE

data SubtypeConstraint = (:<:) String String deriving (Eq, Ord, Show)

transitivity :: NCE.Computation (IntensionalIdentity Ord) SubtypeConstraint
transitivity = intensional Ord do
    (a :<: b) <- NCE.getFact
    (c :<: d) <- NCE.getFact
    if b == c then
        itsReturn %@ Set.singleton (a :<: d)
    else
        itsReturn %@ Set.empty

example :: IntensionalIdentity Ord (Set SubtypeConstraint)
example = intensional Ord do
    initialEngine <- NCE.addComputation transitivity NCE.empty
    let engine :: NCE.Engine (IntensionalIdentity Ord) SubtypeConstraint
        engine =
          NCE.addFacts [ "poodle" :<: "dog"
                       , "wolfhound" :<: "dog"
                       , "siamese" :<: "cat"
                       , "ragdoll" :<: "cat"
                       , "dog" :<: "mammal"
                       , "cat" :<: "mammal"
                       , "mammal" :<: "animal"
                       , "limestone" :<: "rock"
                       ] initialEngine
    engine' <- NCE.close engine
    itsReturn %$ NCE.facts engine'

main :: IO ()
main = do
    let IntensionalIdentity set = example
    putStrLn $ show set
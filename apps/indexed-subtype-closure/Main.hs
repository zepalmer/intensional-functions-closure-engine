{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Intensional.Functor
import Control.Intensional.Applicative

import Control.Intensional.Monad
import Control.Intensional.Monad.Identity (IntensionalIdentity(..))
import Control.Intensional.Runtime
import Data.Function ((&))
import Data.Set (Set)
import qualified Data.Set as Set

import qualified IndexedClosureEngine as ICE

import System.IO.Unsafe (unsafePerformIO)

newtype T = T String
    deriving (Eq, Ord)

poodle :: T
poodle = T "poodle"

wolfhound :: T
wolfhound = T "wolfhound"

dog :: T
dog = T "dog"

siamese :: T
siamese = T "siamese"

ragdoll :: T
ragdoll = T "ragdoll"

cat :: T
cat = T "cat"

mammal :: T
mammal = T "mammal"

animal :: T
animal = T "animal"

limestone :: T
limestone = T "limestone"

rock :: T
rock = T "rock"

data SubtypeConstraint = (:<:) T T
    deriving (Eq, Ord)

instance Show SubtypeConstraint where
    show (T a :<: T b) = a ++ " <: " ++ b

indexByIdentity :: ICE.IndexingFunction SubtypeConstraint () SubtypeConstraint
indexByIdentity = \%Ord a -> Just ((), a)

indexByLowerBound :: ICE.IndexingFunction SubtypeConstraint T T
indexByLowerBound = \%Ord (a :<: b) -> Just (a, b)

transitivity :: ICE.Computation
                    (IntensionalIdentity Ord)
                    SubtypeConstraint
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
          initialEngine
          & ICE.addFact (poodle :<: dog)
          & ICE.addFact (wolfhound :<: dog)
          & ICE.addFact (siamese :<: cat)
          & ICE.addFact (ragdoll :<: cat)
          & ICE.addFact (dog :<: mammal)
          & ICE.addFact (cat :<: mammal)
          & ICE.addFact (mammal :<: animal)
          & ICE.addFact (limestone :<: rock)
    engine' <- ICE.close engine
    itsReturn %$ ICE.facts engine'

main :: IO ()
main = do
    let IntensionalIdentity set = example
    putStrLn $ show set
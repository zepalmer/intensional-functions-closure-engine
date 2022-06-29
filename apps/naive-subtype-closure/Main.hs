{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Intensional.Monad
import Control.Intensional.Monad.Identity (IntensionalIdentity(..))
import Control.Intensional.Runtime
import Data.Function ((&))
import Data.Set (Set)
import qualified Data.Set as Set

import Closure.Intensional.Naive.Engine

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

transitivity :: Computation (IntensionalIdentity Ord) SubtypeConstraint
transitivity = intensional Ord do
    (a :<: b) <- getFact
    (c :<: d) <- getFact
    if b == c then
        itsReturn %@ Set.singleton (a :<: d)
    else
        itsReturn %@ Set.empty

example :: IntensionalIdentity Ord (Set SubtypeConstraint)
example = intensional Ord do
    initialEngine <- addComputation transitivity emptyEngine
    let engine :: Engine (IntensionalIdentity Ord) SubtypeConstraint
        engine =
          addFacts [ poodle :<: dog
                   , wolfhound :<: dog
                   , siamese :<: cat
                   , ragdoll :<: cat
                   , dog :<: mammal
                   , cat :<: mammal
                   , mammal :<: animal
                   , limestone :<: rock
                   ] initialEngine
    engine' <- close engine
    itsReturn %$ facts engine'

main :: IO ()
main = do
    let IntensionalIdentity set = example
    putStrLn $ show set
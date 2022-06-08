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

import qualified NaiveClosureEngine as NCE

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
          initialEngine
          & NCE.addFact (poodle :<: dog)
          & NCE.addFact (wolfhound :<: dog)
          & NCE.addFact (siamese :<: cat)
          & NCE.addFact (ragdoll :<: cat)
          & NCE.addFact (dog :<: mammal)
          & NCE.addFact (cat :<: mammal)
          & NCE.addFact (mammal :<: animal)
          & NCE.addFact (limestone :<: rock)
    engine' <- NCE.close engine
    itsReturn %$ NCE.facts engine'

main :: IO ()
main = do
    let IntensionalIdentity set = example
    putStrLn $ show set
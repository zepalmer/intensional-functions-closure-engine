{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Tests.Extensional.Indexed.SubtypeClosure where

import Data.Functor.Identity (Identity(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit

import Closure.Extensional.Indexed.Engine

data SubtypeConstraint = (:<:) String String deriving (Eq, Ord, Show)

data IdentityIndex = IdentityIndex deriving (Eq, Ord)
instance Index SubtypeConstraint IdentityIndex where
    type IndexKey SubtypeConstraint IdentityIndex = ()
    type IndexDerivative SubtypeConstraint IdentityIndex = SubtypeConstraint
    index IdentityIndex c = Just ((), c)

data LowerBoundIndex = LowerBoundIndex deriving (Eq, Ord)
instance Index SubtypeConstraint LowerBoundIndex where
    type IndexKey SubtypeConstraint LowerBoundIndex = String
    type IndexDerivative SubtypeConstraint LowerBoundIndex = String
    index LowerBoundIndex (a :<: b) = Just (a, b)

data Transitivity input where
    TransitivityStep1 :: Transitivity SubtypeConstraint
    TransitivityStep2 :: String -> Transitivity String
deriving instance (Eq a) => Eq (Transitivity a)
deriving instance (Ord a) => Ord (Transitivity a)

instance Computation Identity SubtypeConstraint Transitivity where
    compute computation input = case computation of
        TransitivityStep1 ->
            let (a :<: b) = input in
            return $ onIndex LowerBoundIndex b $ TransitivityStep2 a
        TransitivityStep2 a ->
            let c = input in
            return $ finished $ Set.singleton $ a :<: c

transitivity :: Identity [ComputationStepResult Identity SubtypeConstraint]
transitivity = return $ onIndex IdentityIndex () TransitivityStep1

example :: Identity (Set SubtypeConstraint)
example = do
    initialEngine <-
        addComputations transitivity $
            addIndex LowerBoundIndex $
            addIndex IdentityIndex $
            (emptyEngine :: Engine Identity SubtypeConstraint)
    let engine :: Engine Identity SubtypeConstraint
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
    return $ facts engine'

tests :: Test
tests =
  TestLabel "extensional indexed subtype closure" $ TestCase $
    assertEqual "for extensional indexed subtype closure"
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
        (let Identity resultFacts = example in resultFacts)

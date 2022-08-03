{-# LANGUAGE IntensionalFunctions #-}

module Tests.Intensional.Naive.CYKClosure where

import Control.Intensional.Monad
import Control.Intensional.Monad.Identity (IntensionalIdentity(..))
import Control.Intensional.Runtime
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit

import Closure.Intensional.Naive.Engine

type Nonterminal = String
data Fact = GrammarRule Nonterminal Nonterminal Nonterminal
          | ParsedSpan Nonterminal Int Int
    deriving (Eq, Ord, Show)

--
-- parsed(X,I,K) :- parsed(Y,I,J), parsed(Z,J,K), grammarNT(X,Y,Z)
--
closure :: Computation (IntensionalIdentity Ord) Fact
closure = intensional Ord do
    f1 <- getFact
    case f1 of
        GrammarRule _ _ _ -> itsReturn %@ Set.empty
        ParsedSpan y i j -> intensional Ord do
            f2 <- getFact
            case f2 of
                GrammarRule _ _ _ -> itsReturn %@ Set.empty
                ParsedSpan z j' k ->
                    if j /= j' then
                        itsReturn %@ Set.empty
                    else
                        intensional Ord do
                            f3 <- getFact
                            case f3 of
                                ParsedSpan _ _ _ -> itsReturn %@ Set.empty
                                GrammarRule x y' z' ->
                                    if (y,z) /= (y',z') then
                                        itsReturn %@ Set.empty
                                    else
                                        itsReturn %@
                                            (Set.singleton $ ParsedSpan x i k)

example :: IntensionalIdentity Ord (Set Fact)
example = intensional Ord do
    initialEngine <- addComputation closure emptyEngine
    let engine :: Engine (IntensionalIdentity Ord) Fact
        engine = addFacts [ GrammarRule "AddL" "int" "+"
                          , GrammarRule "Add" "AddL" "int"
                          , GrammarRule "AddL" "Add" "+"
                          , ParsedSpan "int" 0 1
                          , ParsedSpan "+" 1 2
                          , ParsedSpan "int" 2 3
                          , ParsedSpan "+" 3 4
                          , ParsedSpan "int" 4 5
                          ]
                          initialEngine
    engine' <- close engine
    itsReturn %$ facts engine'

tests :: Test
tests =
  TestLabel "intensional naive CYK closure" $ TestCase $
    assertEqual "for intensional naive CYK closure"
        (Set.fromList
            [ GrammarRule "AddL" "int" "+"
            , GrammarRule "Add" "AddL" "int"
            , GrammarRule "AddL" "Add" "+"
            , ParsedSpan "int" 0 1
            , ParsedSpan "AddL" 0 2
            , ParsedSpan "Add" 0 3
            , ParsedSpan "AddL" 0 4
            , ParsedSpan "Add" 0 5
            , ParsedSpan "+" 1 2
            , ParsedSpan "int" 2 3
            , ParsedSpan "AddL" 2 4
            , ParsedSpan "Add" 2 5
            , ParsedSpan "+" 3 4
            , ParsedSpan "int" 4 5
            ]
        )
        (let IntensionalIdentity facts = example in facts)

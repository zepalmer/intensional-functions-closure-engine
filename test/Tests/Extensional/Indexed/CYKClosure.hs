{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Tests.Extensional.Indexed.CYKClosure where

import Data.Functor.Identity (Identity(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit

import Closure.Extensional.Indexed.Engine

type Nonterminal = String
data Fact = GrammarRule Nonterminal Nonterminal Nonterminal
          | ParsedSpan Nonterminal Int Int
    deriving (Eq, Ord, Show)

data IndexParsedSpans = IndexParsedSpans
    deriving (Eq, Ord)
instance Index Fact IndexParsedSpans where
    type IndexKey Fact IndexParsedSpans = ()
    type IndexDerivative Fact IndexParsedSpans = (Nonterminal,Int,Int)
    index IndexParsedSpans (GrammarRule {}) = Nothing
    index IndexParsedSpans (ParsedSpan t i j) = Just ((), (t,i,j))

data IndexParsedSpansByStartPosition = IndexParsedSpansByStartPosition
    deriving (Eq, Ord)
instance Index Fact IndexParsedSpansByStartPosition where
    type IndexKey Fact IndexParsedSpansByStartPosition = Int
    type IndexDerivative Fact IndexParsedSpansByStartPosition =
        (Nonterminal,Int)
    index IndexParsedSpansByStartPosition (GrammarRule {}) = Nothing
    index IndexParsedSpansByStartPosition (ParsedSpan t i j) = Just (i, (t,j))

data IndexGrammarRulesByProduction = IndexGrammarRulesByProduction
    deriving (Eq, Ord)
instance Index Fact IndexGrammarRulesByProduction where
    type IndexKey Fact IndexGrammarRulesByProduction = (Nonterminal,Nonterminal)
    type IndexDerivative Fact IndexGrammarRulesByProduction = Nonterminal
    index IndexGrammarRulesByProduction (GrammarRule x y z) = Just ((y,z), x)
    index IndexGrammarRulesByProduction (ParsedSpan {}) = Nothing

data ClosureStep input where
    ClosureStep1 :: ClosureStep (Nonterminal, Int, Int)
    ClosureStep2 :: Nonterminal -> Int -> ClosureStep (Nonterminal, Int)
    ClosureStep3 :: Int -> Int -> ClosureStep Nonterminal
deriving instance (Eq input) => (Eq (ClosureStep input))
deriving instance (Ord input) => (Ord (ClosureStep input))

instance Computation Identity Fact ClosureStep where
    compute computation input = case computation of
        ClosureStep1 ->
            let (y, i, j) = input in
            return $ onIndex IndexParsedSpansByStartPosition j $
                ClosureStep2 y i
        ClosureStep2 y i ->
            let (z, k) = input in
            return $ onIndex IndexGrammarRulesByProduction (y,z) $
                ClosureStep3 i k
        ClosureStep3 i k ->
            let x = input in
            return $ finished $ Set.singleton $ ParsedSpan x i k

closure :: Identity (ComputationStepResult Identity Fact)
closure = return $ onIndex IndexParsedSpans () ClosureStep1

example :: Identity (Set Fact)
example = do
    initialEngine <- addComputation closure $
                        addIndex IndexParsedSpans $
                        addIndex IndexParsedSpansByStartPosition $
                        addIndex IndexGrammarRulesByProduction $ emptyEngine
    let engine = addFacts [ GrammarRule "AddL" "int" "+"
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
    return $ facts engine'

tests :: Test
tests =
  TestLabel "extensional indexed CYK closure" $ TestCase $
    assertEqual "for extensional indexed CYK closure"
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
        (let Identity resultFacts = example in resultFacts)

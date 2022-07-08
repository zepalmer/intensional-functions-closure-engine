{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Functor.Identity (Identity(..))
import Data.Set (Set)
import qualified Data.Set as Set

import Closure.Extensional.Indexed.Engine

type Nonterminal = String
data GrammarRule = NonterminalRule Nonterminal Nonterminal Nonterminal
    deriving (Eq, Ord, Show)
data ParsedSpan = ParsedSpan Nonterminal Int Int
    deriving (Eq, Ord, Show)

data Fact = GrammarFact GrammarRule | ParsedFact ParsedSpan
    deriving (Eq, Ord, Show)

data IndexParsedSpans = IndexParsedSpans deriving (Eq, Ord)
instance Index Fact IndexParsedSpans where
    type IndexKey Fact IndexParsedSpans = ()
    type IndexDerivative Fact IndexParsedSpans = ParsedSpan
    index IndexParsedSpans (GrammarFact _) = Nothing
    index IndexParsedSpans (ParsedFact span) = Just ((), span)

data IndexParsedSpansByStartPosition = IndexParsedSpansByStartPosition
    deriving (Eq, Ord)
instance Index Fact IndexParsedSpansByStartPosition where
    type IndexKey Fact IndexParsedSpansByStartPosition = Int
    type IndexDerivative Fact IndexParsedSpansByStartPosition =
        (Nonterminal,Int)
    index IndexParsedSpansByStartPosition (GrammarFact _) = Nothing
    index IndexParsedSpansByStartPosition (ParsedFact (ParsedSpan t i j)) =
        Just (i, (t,j))

data IndexGrammarRulesByProduction = IndexGrammarRulesByProduction
    deriving (Eq, Ord)
instance Index Fact IndexGrammarRulesByProduction where
    type IndexKey Fact IndexGrammarRulesByProduction = (Nonterminal,Nonterminal)
    type IndexDerivative Fact IndexGrammarRulesByProduction = Nonterminal
    index IndexGrammarRulesByProduction (GrammarFact (NonterminalRule x y z)) =
        Just ((y,z), x)
    index IndexGrammarRulesByProduction (ParsedFact _) = Nothing

--
-- parsed(X,I,K) :- parsed(Y,I,J), parsed(Z,J,K), grammarNT(X,Y,Z)
--
data ClosureStep input where
    ClosureStep1 :: ClosureStep ParsedSpan
    ClosureStep2 :: Nonterminal -> Int -> ClosureStep (Nonterminal, Int)
    ClosureStep3 :: Int -> Int -> ClosureStep Nonterminal
deriving instance (Eq input) => (Eq (ClosureStep input))
deriving instance (Ord input) => (Ord (ClosureStep input))

instance Computation Identity Fact ClosureStep where
    compute computation input = case computation of
        ClosureStep1 ->
            let (ParsedSpan y i j) = input in
            return $ onIndex IndexParsedSpansByStartPosition j $
                ClosureStep2 y i
        ClosureStep2 y i ->
            let (z, k) = input in
            return $ onIndex IndexGrammarRulesByProduction (y,z) $
                ClosureStep3 i k
        ClosureStep3 i k ->
            let x = input in
            return $ finished $ Set.singleton $ ParsedFact $ ParsedSpan x i k

closure :: Identity (ComputationStepResult Identity Fact)
closure = return $ onIndex IndexParsedSpans () ClosureStep1

example :: Identity (Set Fact)
example = do
    initialEngine <- addComputation closure $
                        addIndex IndexParsedSpans $
                        addIndex IndexParsedSpansByStartPosition $
                        addIndex IndexGrammarRulesByProduction $ emptyEngine
    let engine :: Engine Identity Fact
        engine =
          addFacts [ GrammarFact $ NonterminalRule "AddL" "int" "+"
                   , GrammarFact $ NonterminalRule "Add" "AddL" "int"
                   , GrammarFact $ NonterminalRule "AddL" "Add" "+"
                   , ParsedFact $ ParsedSpan "int" 0 1
                   , ParsedFact $ ParsedSpan "+" 1 2
                   , ParsedFact $ ParsedSpan "int" 2 3
                   , ParsedFact $ ParsedSpan "+" 3 4
                   , ParsedFact $ ParsedSpan "int" 4 5
                   ]
          initialEngine
    engine' <- close engine
    return $ facts engine'

main :: IO ()
main = let Identity set = example in putStrLn $ show set
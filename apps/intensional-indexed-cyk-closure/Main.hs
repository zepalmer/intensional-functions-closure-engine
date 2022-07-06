{-# LANGUAGE IntensionalFunctions #-}

module Main where

import Control.Intensional.Monad
import Control.Intensional.Monad.Identity (IntensionalIdentity(..))
import Control.Intensional.Runtime
import Data.Set (Set)
import qualified Data.Set as Set

import Closure.Intensional.Indexed.Engine

type Nonterminal = String
data GrammarRule = NonterminalRule Nonterminal Nonterminal Nonterminal
    deriving (Eq, Ord, Show)
data ParsedSpan = ParsedSpan Nonterminal Int Int
    deriving (Eq, Ord, Show)

data Fact = GrammarFact GrammarRule | ParsedFact ParsedSpan
    deriving (Eq, Ord, Show)

indexParsedSpans :: IndexingFunction Fact () ParsedSpan
indexParsedSpans = \%Ord fact -> case fact of
    GrammarFact _ -> Nothing
    ParsedFact parsedSpan -> Just ((), parsedSpan)

indexParsedSpansByStartPosition :: IndexingFunction Fact Int (Nonterminal,Int)
indexParsedSpansByStartPosition = \%Ord fact -> case fact of
    GrammarFact _ -> Nothing
    ParsedFact (ParsedSpan t i j) -> Just (i, (t,j))

indexGrammarRulesByProduction :: IndexingFunction Fact
                                    (Nonterminal,Nonterminal) Nonterminal
indexGrammarRulesByProduction = \%Ord fact -> case fact of
    GrammarFact (NonterminalRule x y z) -> Just ((y, z), x)
    ParsedFact _ -> Nothing

--
-- parsed(X,I,K) :- parsed(Y,I,J), parsed(Z,J,K), grammarNT(X,Y,Z)
--
closure :: Computation (IntensionalIdentity Ord) Fact
closure = intensional Ord do
    ParsedSpan y i j <- getIndexedFact indexParsedSpans ()
    (z, k) <- getIndexedFact indexParsedSpansByStartPosition j
    x <- getIndexedFact indexGrammarRulesByProduction (y,z)
    itsReturn %@ Set.singleton (ParsedFact $ ParsedSpan x i k)

example :: IntensionalIdentity Ord (Set Fact)
example = intensional Ord do
    initialEngine <- addComputation closure $
                        addIndex indexParsedSpans $
                        addIndex indexParsedSpansByStartPosition $
                        addIndex indexGrammarRulesByProduction $ emptyEngine
    let engine :: Engine (IntensionalIdentity Ord) Fact
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
    itsReturn %$ facts engine'

main :: IO ()
main = let IntensionalIdentity set = example in putStrLn $ show set
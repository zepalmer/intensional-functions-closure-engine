{-# LANGUAGE IntensionalFunctions #-}

module Tests.Intensional.Indexed.CYKClosure where

import Control.Intensional.Monad
import Control.Intensional.Monad.Identity (IntensionalIdentity(..))
import Control.Intensional.Runtime
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit

import Closure.Intensional.Indexed.Engine

type Nonterminal = String
data Fact = GrammarRule Nonterminal Nonterminal Nonterminal
          | ParsedSpan Nonterminal Int Int
    deriving (Eq, Ord, Show)

indexParsedSpans :: IndexingFunction Fact () (Nonterminal, Int, Int)
indexParsedSpans = \%Ord fact -> case fact of
    GrammarRule {} -> Nothing
    ParsedSpan t i j -> Just ((), (t,i,j))

indexParsedSpansByStartPosition :: IndexingFunction Fact Int (Nonterminal, Int)
indexParsedSpansByStartPosition = \%Ord fact -> case fact of
    GrammarRule {} -> Nothing
    ParsedSpan t i j -> Just (i, (t,j))

indexGrammarRulesByProduction :: IndexingFunction Fact
                                    (Nonterminal, Nonterminal) Nonterminal
indexGrammarRulesByProduction = \%Ord fact -> case fact of
    GrammarRule x y z -> Just ((y, z), x)
    ParsedSpan {} -> Nothing

closure :: Computation (IntensionalIdentity Ord) Fact
closure = intensional Ord do
    (y, i, j) <- getIndexedFact indexParsedSpans ()
    (z, k) <- getIndexedFact indexParsedSpansByStartPosition j
    x <- getIndexedFact indexGrammarRulesByProduction (y,z)
    itsReturn %@ Set.singleton (ParsedSpan x i k)

example :: IntensionalIdentity Ord (Set Fact)
example = intensional Ord do
    initialEngine <- addComputation closure $
                        addIndex indexParsedSpans $
                        addIndex indexParsedSpansByStartPosition $
                        addIndex indexGrammarRulesByProduction $ emptyEngine
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
    itsReturn %$ facts engine'

tests :: Test
tests =
  TestLabel "intensional indexed CYK closure" $ TestCase $
    assertEqual "for intensional indexed CYK closure"
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
        (let IntensionalIdentity resultFacts = example in resultFacts)

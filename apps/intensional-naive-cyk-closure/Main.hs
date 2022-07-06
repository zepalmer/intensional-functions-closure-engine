{-# LANGUAGE IntensionalFunctions #-}

module Main where

import Control.Intensional.Monad
import Control.Intensional.Monad.Identity (IntensionalIdentity(..))
import Control.Intensional.Runtime
import Data.Set (Set)
import qualified Data.Set as Set

import Closure.Intensional.Naive.Engine

type Nonterminal = String
data GrammarRule = NonterminalRule Nonterminal Nonterminal Nonterminal
    deriving (Eq, Ord, Show)
data ParsedSpan = ParsedSpan Nonterminal Int Int
    deriving (Eq, Ord, Show)

data Fact = GrammarFact GrammarRule | ParsedFact ParsedSpan
    deriving (Eq, Ord, Show)

--
-- parsed(X,I,K) :- parsed(Y,I,J), parsed(Z,J,K), grammarNT(X,Y,Z)
--
closure :: Computation (IntensionalIdentity Ord) Fact
closure = intensional Ord do
    f1 <- getFact
    case f1 of
        GrammarFact _ -> itsReturn %@ Set.empty
        ParsedFact (ParsedSpan y i j) -> intensional Ord do
            f2 <- getFact
            case f2 of
                GrammarFact _ -> itsReturn %@ Set.empty
                ParsedFact (ParsedSpan z j' k) ->
                    if j /= j' then
                        itsReturn %@ Set.empty
                    else
                        intensional Ord do
                            f3 <- getFact
                            case f3 of
                                ParsedFact _ -> itsReturn %@ Set.empty
                                GrammarFact (NonterminalRule x y' z') ->
                                    if (y,z) /= (y',z') then
                                        itsReturn %@ Set.empty
                                    else
                                        itsReturn %@
                                            Set.singleton
                                                (ParsedFact $ ParsedSpan x i k)

example :: IntensionalIdentity Ord (Set Fact)
example = intensional Ord do
    initialEngine <- addComputation closure emptyEngine
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
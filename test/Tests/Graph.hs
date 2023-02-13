{-# LANGUAGE DataKinds #-}
{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Tests.Graph
( tests
) where

import Control.Intensional.Applicative
import Control.Intensional.Monad.Identity
import Control.Intensional.Runtime
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit

import Closure.Intensional.Naive.Engine

data Edge = (:~>) Int Int
  deriving (Eq, Ord, Show)

-- naiveClosure :: Set Edge -> Set Edge
-- naiveClosure edgeSet =
--   let edges = Set.toList edgeSet in
--   let edges' = Set.fromList $ do
--         (a :~> b) <- edges
--         (b' :~> c) <- edges
--         guard $ b == b'
--         pure $ a :~> c
--   in
--   if edgeSet == edges' then edges' else naiveClosure edges'

edgeClosure :: Computation (IntensionalIdentity Ord) Edge
edgeClosure = intensional Ord do
  a :~> b <- getFact
  b' :~> c <- getFact
  if b /= b' then itsPure %@ Set.empty else itsPure %@ Set.singleton (a :~> c)

performGraphClosure :: Set Edge -> Set Edge
performGraphClosure edges =
  let IntensionalIdentity engine =
        intensional Ord do
          engine1 <- addComputation edgeClosure emptyEngine
          let engine1' = foldr addFact engine1 $ Set.toList edges
          close engine1'
  in
  facts engine

graphTest :: Set Edge -> Set Edge -> Test
graphTest start expected =
  TestCase (
    let actual = performGraphClosure start in
    assertEqual "for closure, " expected actual
  )

tests :: Test
tests =
  TestLabel "graph tests" $ TestList $ List.map
    (\(start,expected) ->
      graphTest
        (Set.fromList $ List.map (\(a,b) -> a :~> b) start)
        (Set.fromList $ List.map (\(a,b) -> a :~> b) (start ++ expected))
    )
    [ ( [(1,2), (2,3)]
      , [(1,3)]
      )
    ]
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Test.HUnit

import qualified Tests.Extensional.Indexed.CYKClosure
import qualified Tests.Extensional.Indexed.SubtypeClosure
import qualified Tests.Graph
import qualified Tests.Intensional.Indexed.CYKClosure
import qualified Tests.Intensional.Indexed.SubtypeClosure
import qualified Tests.Intensional.Naive.CYKClosure
import qualified Tests.Intensional.Naive.SubtypeClosure

tests :: Test
tests = TestList
  [ Tests.Extensional.Indexed.CYKClosure.tests
  , Tests.Extensional.Indexed.SubtypeClosure.tests
  , Tests.Graph.tests
  , Tests.Intensional.Indexed.CYKClosure.tests
  , Tests.Intensional.Indexed.SubtypeClosure.tests
  , Tests.Intensional.Naive.CYKClosure.tests
  , Tests.Intensional.Naive.SubtypeClosure.tests
  ]

main :: IO ()
main = runTestTT tests >> pure ()

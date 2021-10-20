{-# LANGUAGE DataKinds #-}
{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import qualified GraphTests
import Test.HUnit

-- FIXME: the Engine apparently has the monad on the *wrong side*: if we make
--        m a nondeterminism monad, we get nondeterminism over coroutines and
--        not coroutines over nondeterminism.  The engine would have to be
--        modified to support providing it a transformer so that it could work
--        with transformed suspended computations.  Note that this can lead to
--        infinite streams of suspended computations being produced in a
--        starvation kind of way: the computations have to be cooperative or
--        they could starve the engine since the engine doesn't even know that
--        the transformer is doing any of this and essentially runs it until
--        all of its current paths of exploration suspend willingly.

tests :: Test
tests = TestList
  [ GraphTests.tests
  ]

main :: IO ()
main = runTestTT tests >> pure ()

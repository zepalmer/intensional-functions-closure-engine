{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
(
) where

import Control.Intensional.Monad.Trans.Coroutine
import Control.Intensional.Monad.Trans.Coroutine.SuspensionFunctors
import qualified Data.Set as Set
import Data.Set (Set)

{-
Coroutine engine:
    * One set of facts (e.g. subtype constraints)
    * One set of computations (e.g. monadic expression to establish transitivity)
      * Monadic expression uses await to obtain new facts
      * Computations both await facts and produce facts (via return)
      * Intensionality specifically is the key ingredient that lets us make this
        a *set* and lets us ask whether a "new" await is in fact novel or has
        already been persued.
    * Engine uses worklist to operate on Cartesian product of these sets
      * Engine stops when worklist is empty
    * Initial sets provided by caller
More sophisticated engine:
    * Yield+await where yield signals filter on set of facts
      * Filter is also intensional?
-}

-- newtype EngineT fact m a =
--   EngineT
--     { unEngineT :: CoroutineT ?? m a }

type Computation m fact = CoroutineT Ord (Await Ord fact) m (Set fact)

data Engine m fact = Engine
  { facts :: Set fact
  , computations :: Set (Computation m fact)
  , workset :: Set (fact, Computation m fact)
  }

deriving instance
  ( Eq fact
  , (Eq (m (Either ((Await Ord fact) (CoroutineT Ord (Await Ord fact) m (Set fact))) (Set fact))))
  ) => Eq (Engine m fact)
deriving instance
  ( Ord fact
  , (Ord (m (Either ((Await Ord fact) (CoroutineT Ord (Await Ord fact) m (Set fact))) (Set fact))))
  ) => Ord (Engine m fact)

initialize :: (Ord fact, Ord (Computation m fact))
           => Set fact -> Set (Computation m fact) -> Engine m fact
initialize = undefined -- TODO
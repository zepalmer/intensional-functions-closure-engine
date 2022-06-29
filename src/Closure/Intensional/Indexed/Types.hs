{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE StandaloneDeriving #-}

module Closure.Intensional.Indexed.Types
( IndexingFunction
) where

import Data.Typeable (Proxy(..), Typeable, eqT, typeRep, (:~:)(..))

type IndexingFunction fact key derivative =
    fact ->%Ord (Maybe (key,derivative))

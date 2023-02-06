{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE StandaloneDeriving #-}

module Closure.Intensional.Indexed.Types
( IndexingFunction
) where

type IndexingFunction fact key derivative =
    fact ->%Ord (Maybe (key,derivative))

{-# LANGUAGE IntensionalFunctions #-}
{-# LANGUAGE StandaloneDeriving #-}

module IndexedClosureEngine.Types
( IndexingFunction
-- , SomeIndexingFunction(..)
) where

import Data.Typeable (Proxy(..), Typeable, eqT, typeRep, (:~:)(..))

type IndexingFunction fact key derivative =
    fact ->%Ord (Maybe (key,derivative))

-- data SomeIndexingFunction fact where
--     SomeIndexingFunction :: forall fact key derivative.
--                             (Typeable (IndexingFunction fact key derivative))
--                          => IndexingFunction fact key derivative
--                          -> SomeIndexingFunction fact

-- instance Eq (SomeIndexingFunction fact) where
--     (==)
--             (SomeIndexingFunction (f :: a))
--             (SomeIndexingFunction (g :: a')) =
--         case eqT of
--             Just (Refl :: (a :~: a')) ->
--                 f == g
--             Nothing ->
--                 False

-- instance Ord (SomeIndexingFunction fact) where
--     compare
--             (SomeIndexingFunction (f :: a))
--             (SomeIndexingFunction (g :: a')) =
--         case eqT of
--             Just (Refl :: (a :~: a')) ->
--                 compare f g
--             Nothing ->
--                 compare
--                     (typeRep (Proxy :: Proxy a))
--                     (typeRep (Proxy :: Proxy a'))

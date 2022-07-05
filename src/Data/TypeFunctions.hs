{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Data.TypeFunctions
( Fst(..)
, Snd(..)
) where

import Data.Kind (Type)

type family Fst (t :: Type) :: Type where
    Fst (a,b) = a
type family Snd (t :: Type) :: Type where
    Snd (a,b) = b

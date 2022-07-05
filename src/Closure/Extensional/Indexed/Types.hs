{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Closure.Extensional.Indexed.Types where

import Data.Kind (Type)

class Index (fact :: Type) symbol where
    type IndexKey fact symbol :: Type
    type IndexDerivative fact symbol :: Type
    index :: symbol -> fact
          -> Maybe (IndexKey fact symbol, IndexDerivative fact symbol)

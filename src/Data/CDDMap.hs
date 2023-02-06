{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
    This module provides a constrained, dynamic dependent map data structure.
    The map is dependent in that each mapping contains a key and value of a
    potentially different type; specifically, the key and value types are the
    result of passing a single argument type to type-level functions and the
    argument type may vary from mapping to mapping.  The map is dynamic in that
    the aforementioned argument type is known at runtime.  The map is
    constrained in that the caller may provide a general type function to
    require constraints related each node's argument, key, and value.

    A similar dependent map structure named DMap can be found in the
    dependent-map Hackage package.  That data structure is neither dynamic nor
    constrained.
-}

module Data.CDDMap
( CDDMap
, empty
, insert
, Data.CDDMap.lookup
, contains
, fold
) where

import qualified Data.List as List
import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.Typeable (Typeable, (:~:)(..), eqT, typeOf)
import GHC.Exts (Constraint)

-- ---------- Interface -------------------------------------------------------

{-|
    The type of a constrained dynamic dependent map.  The first type parameter
    is a type function which, given a node's argument type, key type, and
    value type, produces a constraint which must be proven to create the
    mapping.  This constraint is then available in some cases where mappings
    are retrieved.
-}
newtype CDDMap
            (c :: Type -> Type -> Type -> Constraint)
            (kf :: Type -> Type)
            (vf :: Type -> Type)
    = CDDMap (CDDMapNode c kf vf)

type MappingConstraints c kf vf a =
    ( Typeable a
    , Typeable kf
    , Typeable vf
    , c a (kf a) (vf a)
    , Ord (kf a)
    , Ord (vf a)
    -- NOTE: this "Ord (vf a)" is required by the Ord instance for
    -- CDDMap but not otherwise necessary.  In principle, the Ord here
    -- could be a type parameter and the instance could be
    -- conditional.
    )

empty :: CDDMap c kf vf
empty = CDDMap emptyNode

insert :: forall c kf vf a.
          (MappingConstraints c kf vf a)
       => kf a -> vf a -> CDDMap c kf vf -> CDDMap c kf vf
insert k v (CDDMap node) = CDDMap $ insertNode k v node

lookup :: forall c kf vf a.
          ( Typeable a
          , Typeable kf
          , Ord (kf a)
          )
       => kf a -> CDDMap c kf vf -> Maybe (vf a)
lookup k (CDDMap node) = getFromNode k node

contains :: forall c kf vf a.
            ( Typeable a
            , Typeable kf
            , Ord (kf a)
            )
         => kf a -> CDDMap c kf vf -> Bool
contains k dict = isJust $ Data.CDDMap.lookup k dict

fold :: forall c kf vf r.
        (forall a. (MappingConstraints c kf vf a)
         => r -> kf a -> vf a -> r
        )
     -> r
     -> CDDMap c kf vf
     -> r
fold fn acc (CDDMap node) = foldOnNode fn acc node

instance Eq (CDDMap c kf vf) where
    (==) m1 m2 = m1 `compare` m2 == EQ

-- The arbitrary ordering on CDDMap is defined in terms of the pointwise
-- ordering of a sorted list of key-value pairs in the map.
instance Ord (CDDMap c kf vf) where
    compare (CDDMap r1) (CDDMap r2) =
        loop (iterateNodes r1) (iterateNodes r2)
        where
          loop nodes1 nodes2 =
            case nodes1 of
                [] ->
                    case nodes2 of
                        [] -> EQ
                        Empty : nodes2' -> loop nodes1 nodes2'
                        Node _ _ _ _ : _ -> LT
                Empty : nodes1' -> loop nodes1' nodes2
                Node (k1 :: k1) (v1 :: v1) _ _ : nodes1' ->
                    case nodes2 of
                        [] -> GT
                        Empty : nodes2' -> loop nodes1 nodes2'
                        Node (k2 :: k2) (v2 :: v2) _ _ : nodes2' ->
                            let result =
                                  case eqT of
                                    Just (Refl :: (k1,v1) :~: (k2,v2)) ->
                                        (k1,v1) `compare` (k2,v2)
                                    Nothing ->
                                        typeOf (k1,v1) `compare` typeOf (k2,v2)
                            in
                            if result == EQ then
                                loop nodes1' nodes2'
                            else
                                result

-- ---------- Implementation --------------------------------------------------

data CDDMapNode
        (c :: Type -> Type -> Type -> Constraint)
        (kf :: Type -> Type)
        (vf :: Type -> Type)
        where
    Empty :: CDDMapNode c kf vf
    Node :: forall c kf vf a.
            (MappingConstraints c kf vf a)
         => kf a
         -> vf a
         -> CDDMapNode c kf vf
         -> CDDMapNode c kf vf
         -> CDDMapNode c kf vf

emptyNode :: CDDMapNode c kf vf
emptyNode = Empty

insertNode :: forall c kf vf a.
              (MappingConstraints c kf vf a)
           => kf a -> vf a -> CDDMapNode c kf vf -> CDDMapNode c kf vf
insertNode k v node =
    case node of
        Empty -> Node k v Empty Empty
        Node (k' :: kf a') v' l r ->
            case eqT of
                Just (Refl :: kf a :~: kf a') ->
                    if k < k' then Node k' v' (insertNode k v l) r else
                    if k > k' then Node k' v' l (insertNode k v r) else
                    Node k v l r
                Nothing ->
                    if typeOf k < typeOf k' then
                        Node k' v' (insertNode k v l) r
                    else
                        Node k' v' l (insertNode k v r)

getFromNode :: forall c kf vf a.
               ( Typeable a
               , Typeable kf
               , Ord (kf a)
               )
            => kf a -> CDDMapNode c kf vf -> Maybe (vf a)
getFromNode k node =
    case node of
        Empty -> Nothing
        Node (k' :: kf a') v' l r ->
            case eqT of
                Just (Refl :: kf a :~: kf a') ->
                    if k < k' then getFromNode k l else
                    if k > k' then getFromNode k r else
                    Just v'
                Nothing ->
                    if typeOf k < typeOf k' then
                        getFromNode k l
                    else
                        getFromNode k r

foldOnNode :: forall c kf vf acc.
              (forall a. (MappingConstraints c kf vf a)
               => acc -> kf a -> vf a -> acc
              )
           -> acc
           -> CDDMapNode c kf vf
           -> acc
foldOnNode fn acc node =
    case node of
        Empty -> acc
        Node k v l r ->
            let acc' = foldOnNode fn acc l in
            let acc'' = fn acc' k v in
            let acc''' = foldOnNode fn acc'' r in
            acc'''

iterateNodes :: forall c kf vf. CDDMapNode c kf vf -> [CDDMapNode c kf vf]
iterateNodes root =
    case root of
        Empty -> []
        Node _ _ l r -> List.concat [iterateNodes l, [root], iterateNodes r]

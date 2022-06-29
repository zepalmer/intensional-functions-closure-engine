{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-
  This module provides a constrained, dependent map data structure with each
  mapping providing two type arguments.

  In this structure, the provided key and value types are of kind
  Type -> Type -> Type.  Each mapping contains keys and values which instantiate
  these types with the same two type arguments; however, each mapping may use a
  different pair of type arguments than other mappings to instantiate the types
  of its key and value.

  The structure is constrained in that all type arguments for all mappings are
  subject to a particular constraint function (of kind Type -> Constraint).
  This can be used to bound the polymorphism on the dependent types in this
  structure.

  A similar dependent map structure named DMap can be found in the dependent-map
  Hackage package.  That structure differs from this one in the following ways:

    * DMap has no mechanism for constraining mappings' type arguments.
    * DMap assumes its key and value types have kind Type -> Type.  This does
      not make DMap more or less expressive than CDMap2; in particular, multiple
      type arguments can be encoded through a single type argument with the use
      of type families.  The use case which inspired CDMap2 uses type arguments
      in a way which would make the type family encoding cumbersome, so this
      structure is designed with two type arguments in mind.
-}

-- TODO: simple balancing implementation (e.g. AVL)?  As of this writing
--       (2022-05-10), dependent-map requires base-compat which does not compile
--       with the nearest Stack distribution to the GHC 9.2.2 we're using, so
--       it'd be difficult to use in this case even if we could make it
--       readable.

module CDMap2
( CDMap2
, empty
, insert
, CDMap2.lookup
, contains
, fold
) where

import Data.Kind (Type)
import qualified Data.List as List
import Data.Typeable (Typeable, (:~:)(..), eqT, typeOf)
import GHC.Exts (Constraint)

-- ---------- Interface -------------------------------------------------------

newtype CDMap2
          (c :: Type -> Constraint)
          (kf :: Type -> Type -> Type)
          (vf :: Type -> Type -> Type)
    = CDMap2 (CDMap2Node c kf vf)

empty :: CDMap2 c kf vf
empty = CDMap2 emptyNode

insert :: forall c kf vf a b.
          ( Typeable a
          , Typeable b
          , Typeable (kf a b)
          , Typeable (vf a b)
          , Ord (kf a b)
          , Ord (vf a b)
          , c a
          , c b
          )
       => kf a b
       -> vf a b
       -> CDMap2 c kf vf
       -> CDMap2 c kf vf
insert k v (CDMap2 d) = CDMap2 $ insertNode k v d

lookup :: forall c kf vf a b.
          (Typeable (kf a b), Ord (kf a b))
       => kf a b
       -> CDMap2 c kf vf
       -> Maybe (vf a b)
lookup k (CDMap2 d) = getFromNode k d

contains :: forall c kf vf a b.
            (Typeable (kf a b), Ord (kf a b))
         => kf a b
         -> CDMap2 c kf vf
         -> Bool
contains k (CDMap2 d) =
    case getFromNode k d of
        Nothing -> False
        Just _ -> True

fold :: forall c kf vf r.
        (forall a b.
         ( Typeable (kf a b)
         , Typeable (vf a b)
         , Typeable a
         , Typeable b
         , Ord (kf a b)
         , Ord (vf a b)
         , c a
         , c b
         )
         => r -> kf a b -> vf a b -> r
        )
     -> r
     -> CDMap2 c kf vf
     -> r
fold fn acc (CDMap2 d) =
    foldOnNode fn acc d

-- The arbitrary ordering on DMap2 is defined in terms of the pointwise ordering
-- of a sorted list of key-value pairs in the map.
instance Ord (CDMap2 c kf vf) where
    compare (CDMap2 r1) (CDMap2 r2) =
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

instance Eq (CDMap2 c kf vf) where
    (==) m1 m2 = m1 `compare` m2 == EQ

-- ---------- Implementation --------------------------------------------------

data CDMap2Node
        (c :: Type -> Constraint)
        (kf :: Type -> Type -> Type)
        (vf :: Type -> Type -> Type)
        where
    Empty :: CDMap2Node c kf vf
    Node :: forall c kf vf a b.
            ( Typeable (kf a b)
            , Typeable (vf a b)
            , Typeable a
            , Typeable b
            , Ord (kf a b)
            , Ord (vf a b)
            , c a
            , c b
            )
         => kf a b
         -> vf a b
         -> CDMap2Node c kf vf
         -> CDMap2Node c kf vf
         -> CDMap2Node c kf vf

emptyNode :: CDMap2Node c kf vf
emptyNode = Empty

insertNode :: forall c kf vf a b.
              ( Typeable a
              , Typeable b
              , Typeable (kf a b)
              , Typeable (vf a b)
              , Ord (kf a b)
              , Ord (vf a b)
              , c a
              , c b
              )
           => kf a b
           -> vf a b
           -> CDMap2Node c kf vf
           -> CDMap2Node c kf vf
insertNode k v tree =
    case tree of
        Empty -> Node k v Empty Empty
        Node (k' :: kf a' b') v' l r ->
            case eqT of
                Just (Refl :: kf a b :~: kf a' b') ->
                    if k < k' then Node k' v' (insertNode k v l) r else
                    if k > k' then Node k' v' l (insertNode k v r) else
                    Node k v l r
                Nothing ->
                    if typeOf k < typeOf k' then
                        Node k' v' (insertNode k v l) r
                    else
                        Node k' v' l (insertNode k v r)

getFromNode :: forall c kf vf a b.
               (Typeable (kf a b), Ord (kf a b))
            => kf a b
            -> CDMap2Node c kf vf
            -> Maybe (vf a b)
getFromNode k tree =
    case tree of
        Empty -> Nothing
        Node (k' :: kf a' b') v' l r ->
            case eqT of
                Just (Refl :: kf a b :~: kf a' b') ->
                    if k < k' then getFromNode k l else
                    if k > k' then getFromNode k r else
                    Just v'
                Nothing ->
                    if typeOf k < typeOf k' then
                        getFromNode k l
                    else
                        getFromNode k r

foldOnNode :: forall c kf vf acc.
              (forall a b.
                ( Typeable (kf a b)
                , Typeable (vf a b)
                , Typeable a
                , Typeable b
                , Ord (kf a b)
                , Ord (vf a b)
                , c a
                , c b
                )
                => acc -> kf a b -> vf a b -> acc
              )
           -> acc
           -> CDMap2Node c kf vf
           -> acc
foldOnNode fn acc tree =
    case tree of
        Empty -> acc
        Node k v l r ->
            let acc' = foldOnNode fn acc l in
            let acc'' = fn acc' k v in
            let acc''' = foldOnNode fn acc'' r in
            acc'''

iterateNodes :: forall c kf vf. CDMap2Node c kf vf -> [CDMap2Node c kf vf]
iterateNodes root =
    case root of
        Empty -> []
        Node _ _ l r -> List.concat [iterateNodes l, [root], iterateNodes r]

module Data.CatList
  ( CatList
  , empty
  , null
  , singleton
  , length
  , append
  , cons
  , snoc
  , uncons
  , fromFoldable
  ) where

import Prelude hiding (append)

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus)
import Data.Foldable (class Foldable, foldMapDefaultL)
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse, class Traversable)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)
import Data.Unfoldable1 (class Unfoldable1)

foreign import data CatList :: Type -> Type

foreign import empty :: forall a. CatList a
foreign import null :: forall a. CatList a -> Boolean
foreign import length :: forall a. CatList a -> Int
foreign import append :: forall a. CatList a -> CatList a -> CatList a
foreign import cons :: forall a. a -> CatList a -> CatList a
foreign import singleton :: forall a. a -> CatList a
foreign import snoc :: forall a. CatList a -> a -> CatList a

foreign import unconsImpl :: forall a r. r -> (a -> CatList a -> r) -> CatList a -> r

uncons :: forall a. CatList a -> Maybe (Tuple a (CatList a))
uncons = unconsImpl Nothing (\a rest -> Just (Tuple a rest))

fromFoldable :: forall f. Foldable f => f ~> CatList
fromFoldable f = Foldable.foldMap singleton f

instance semigroupCatList :: Semigroup (CatList a) where
  append = append

instance monoidCatList :: Monoid (CatList a) where
  mempty = empty

instance showCatList :: Show a => Show (CatList a) where
  show c = "(CatList " <> show (length c) <> " items)"

instance foldableCatList :: Foldable CatList where
  foldMap = foldMapDefaultL
  foldr f s l = Foldable.foldrDefault f s l
  foldl f = go
    where
    go acc q = case uncons q of
       Just (Tuple x xs) -> go (f acc x) xs
       Nothing -> acc

instance unfoldableCatList :: Unfoldable CatList where
  unfoldr f b = go b empty
    where
      go source memo = case f source of
        Nothing -> memo
        Just (Tuple one rest) -> go rest (snoc memo one)

instance unfoldable1CatList :: Unfoldable1 CatList where
  unfoldr1 f b = go b empty
    where
      go source memo = case f source of
        Tuple one Nothing -> snoc memo one
        Tuple one (Just rest) -> go rest (snoc memo one)

instance traversableCatList :: Traversable CatList where
  traverse _ c | null c = pure empty
  traverse f c = case uncons c of
    Nothing -> pure empty
    Just (Tuple a d) -> cons <$> f a <*> traverse f d
  sequence c | null c = pure empty
  sequence c = case uncons c of
    Nothing -> pure empty
    Just (Tuple a d) -> cons <$> a <*> sequence d

instance functorCatList :: Functor CatList where
  map _ c | null c = empty
  map f c = case uncons c of
    Nothing -> empty
    Just (Tuple a d) -> f a `cons` map f d

instance applyCatList :: Apply CatList where
  apply = ap

instance applicativeCatList :: Applicative CatList where
  pure = singleton

instance bindCatList :: Bind CatList where
  bind = flip Foldable.foldMap

instance monadCatList :: Monad CatList

instance altCatList :: Alt CatList where
  alt = append

instance plusCatList :: Plus CatList where
  empty = empty

instance alternativeCatList :: Alternative CatList

instance monadPlusCatList :: MonadPlus CatList

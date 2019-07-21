module Typeclasses.Functor () where

import qualified Prelude as P


-- Type class declaration for Functor.
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a        -> f b -> f a
  -- TODO implement const
  -- (<$) a = fmap P.. P.const

data Maybe a = Just a
             | Nothing

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)


data Either a b = Left a
                | Right b

instance Functor (Either a) where
  fmap _ (Left a) = Left a
  fmap f (Right b) = Right (f b)


data Pair a = Pair a a

instance Functor (Pair) where
  fmap f (Pair x y) = Pair (f x) (f y)


data ITree a = Leaf (P.Int -> a)
             | Node [ITree a]

instance Functor ITree where
  fmap f (Leaf g) = Leaf g (f g)
  fmap f (Node x) = fmap f x

-- If f :: a -> b is a functor and g :: b -> c is a functor, then
-- h = (f . g) x = f (g x) is also a functor.

module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Either a b = Left a
                | Right b

data Maybe = Just a
           | Nothing

instance Functor Maybe where
  fmap f (Just a) = f a
  fmap _ Nothing = Nothing

instance Functor (Either a b) where
  fmap _ (Left a) = Left a
  fmap f (Right b) = Right (f b)

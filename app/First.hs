module First where

import Optional
import Test.QuickCheck

newtype First' a = First' { getFirst' :: Optional a }
                 deriving (Eq, Show)

instance Semigroup (First' a) where
  First' Nada <> x = x
  x           <> _ = x

instance Monoid (First' a) where
  mempty = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = do
    x <- arbitrary
    return $ First' x

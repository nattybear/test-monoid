module Main where

import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type S  = String
type B  = Bool
type MA = S -> S -> S -> B

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

mli = monoidLeftIdentity
mri = monoidRightIdentity

main :: IO ()
main = do
  quickCheck (monoidAssoc :: MA)
  quickCheck (mli :: String -> Bool)
  quickCheck (mri :: String -> Bool)

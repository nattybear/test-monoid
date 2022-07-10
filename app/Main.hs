module Main where

import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type S  = String
type B  = Bool
type MA = S -> S -> S -> B

main :: IO ()
main = quickCheck (monoidAssoc :: MA)

> {-# LANGUAGE UnicodeSyntax #-}
>
> module Excercises
> where
> import Prelude hiding (Word)
> import Unicode
> import Data.List
> import Data.Char


3.1 : Write a non-recursive program that computes the word list of a given
text, ordered by frequency of occurrence.

> type Word = String

> lorem ∷ String
> lorem
>   = "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam \
>     \nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam \
>     \erat, sed diam voluptua. At vero eos et accusam et justo duo dolores \
>     \et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est \
>     \Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur \
>     \sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et \
>     \dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam \
>     \et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea \
>     \takimata sanctus est Lorem ipsum dolor sit amet."

> wordList :: String → [(Word, Int)]
> wordList s = let cleanupWord = \s → filter (\c → elem c ['a'..'z']) (map toLower s)
>                  grouped = group ( sort (map cleanupWord (words s)))
>                  unsorted = [ (head xs, length xs) | xs ← grouped ]
>              in sortOn snd unsorted

3.2 : Using the list design pattern discussed in the lectures, give recursive
definitions of
  1. a function allTrue :: [Bool] → Bool that determines whether every
  element of a list of Booleans is true;
  2. a function allFalse that similarly determines whether every element
  of a list of Booleans is false;
  3. a function member :: (Eq a) ⇒ a → [a] → Bool that determines
  whether a specified element is contained in a given list;
  4. a function smallest :: [Int] → Int that calculates the smallest value
  in a list of integers;
  5. a function largest that similarly calculates the largest value in a list
  of integers.

> allTrue :: [Bool] → Bool
> allTrue []      = True
> allTrue (b : bs) = b && allTrue bs

> allFalse :: [Bool] → Bool
> allFalse []      = True
> allFalse (b : bs) = not b && allFalse bs

> member :: (Eq a) ⇒ a → [a] → Bool
> member x []       = False
> member x (y : ys) = if x == y then True else member x ys

> smallest :: [Int] → Int
> smallest (x : [])       = x
> smallest (x1 : x2 : xs) = if x1 < x2 then smallest (x1 : xs) else smallest (x2 : xs)

> largest :: [Int] → Int
> largest (x : [])       = x
> largest (x1 : x2 : xs) = if x1 > x2 then smallest (x1 : xs) else smallest (x2 : xs)

3.3 : A run is a non-empty, non-decreasing sequence of elements. Use the list design pattern to define a function
  runs :: (Ord a) ⇒ [a] → [[a]]
that returns a list of runs such that concat ◦ runs = id

> runs :: (Ord a) ⇒ [a] → [[a]]
> runs []                      = []
> runs (x : [])                = ((x : []) : [])
> runs (x1:x2:xs) | x1 > x2    = (x1 : [] ) : runs (x2:xs)
>                 | otherwise  = ((x1): (head (runs (x2:xs)))) : (tail (runs (x2:xs)))

> {-# LANGUAGE UnicodeSyntax #-}
> module Format
> where
> import Prelude hiding (Word)
> import Unicode
> import WordList (Word, lorem)
> import Data.List

Exercise 3.6 : An important problem in text processing is to format text into
lines of some fixed width, ensuring as many words as possible on each line. If
we assume that adjacent words are separated by one space, a list of words ws
will fit in a line of width n if length (unwords ws) ≤ n. Define a function
  format :: Int → [Word] → [[Word]]
that given a maximal line width and a list of words returns a list of fitting
lines so that concat ◦ format n = id. Is this always possible?
Apply the list design pattern to define format.

> format :: Int → [Word] → [[Word]]
> format n ws = let split = \n ws → ( (take n ws) : [reverse (take (length ws - n) (reverse ws))])  -- split a list at index n
>                   nextsplit = \ws → last [ split m ws | m ← [1..(length ws)], (length (unwords (take m ws)) ≤ n) ] -- takes a list of words and returns the first line and the rest
>                   divide [] = [] -- divide recursively applies the nextsplit function
>                   divide ws = head (nextsplit ws) : divide (last (nextsplit ws))
>               in divide ws

It is not always possible to apply format, since the lenght of individual words may exceed the line lenght.

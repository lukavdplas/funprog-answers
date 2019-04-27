> {-# LANGUAGE UnicodeSyntax #-}
> module QuickTest (Probes, Property, (-->), (==>))
> where
> import Unicode
> import Data.List (sort)

> type Probes a    =  [a]
>
> type Property a  =  a → Bool

> infixr 1  -->, ==>
>
> (-->)   ∷ Probes a → Property b → Property (a → b)
> (==>)   ∷ Probes a → (a → Property b) → Property (a → b)
>
> probes --> prop  =  \ f → and [ prop (f x) | x ← probes ]
> probes ==> prop  =  \ f → and [ prop x (f x) | x ← probes ]

3.5.1 : Define the predicate
  ordered :: (Ord a) ⇒ Property [a]
that checks whether a list is ordered i.e. the sequence of elements
is non-decreasing.

> ordered      ∷ (Ord a) ⇒ Property [a]
> ordered []         = True
> ordered (x:[])     = True
> ordered (x1:x2:xs) = x1 ≤ x2 && ordered (x2:xs)

3.5.2 : Apply the list design pattern to define the generator
  permutations :: [a] → Probes [a]
that produces the list of all permutations of its input list. (How
many permutations of a list of length n are there?)

> permutations ∷ [a] → Probes [a]
> permutations []     = ([]:[])
> permutations (x:[]) = ((x:[]):[])
> permutations (x:xs) = [ take n rest ++ (x : reverse (take (length rest - n) (reverse rest))) | rest ← permutations xs, n ← [0..(length rest)]]

3.5.3 : Use the combinators to define a testing procedure for the function
runs :: (Ord a) ⇒ [a] → [[a]] of Exercise 3.3.

> validruns :: ([Int] → [[Int]]) → Bool
> validruns f = let onlyRuns = \res → and ( map ordered (res))
>                   complete = \inp res → concat (res) == inp
>                   valid = \inp res → (complete inp res) && (onlyRuns res)
>               in ((permutations [1..10]) ==> valid) f

3.5.4 : Harry Hacker has translated a function that calculates the integer
square root from C to Haskell.

> isqrt ∷ Integer → Integer
> isqrt n = loop 0 3 1
>   where loop i k s  | s ≤ n      = loop (i + 1) (k + 2) (s + k)
>                     | otherwise  = i

It is not immediately obvious that this definition is correct. Define
a testing procedure isIntegerSqrt :: Property (Integer → Integer) to
exercise the program. Can you actually figure out how it works?

infixr 4  ⊗
(⊗) ∷ Probes a → Probes b → Probes (a, b)

> niftySort ∷ [a] → [a]
> niftySort _xs  =  []
>
> trustedSort ∷ (Ord a) ⇒ [a] → [a]
> trustedSort  =  sort

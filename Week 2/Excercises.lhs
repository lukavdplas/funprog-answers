> {-# LANGUAGE UnicodeSyntax #-}
>
> module Excercises
> where
> import Unicode
> import Data.Char

2.1.1: How many total functions are there that take one Boolean as an
input and return one Boolean? Or put differently, how many func-
tions are there of type Bool → Bool? Define all of them. Think of
sensible names.

A Bool → Bool function can only have 2 input values, and for each of those input values, it can return two output values. So there are only 2^2 = 4 total functions possible. The functions with somewhat meaningful names:

> identity:: Bool → Bool
> identity x | x == True  = True
>            | x == False = False

> tautology:: Bool → Bool
> tautology x | x == True  = True
>            | x == False = True

> not_:: Bool → Bool
> not_ x | x == True  = False
>            | x == False = True

> falsum:: Bool → Bool
> falsum x | x == True  = False
>            | x == False = False

On top of these, there are also 5 partial functions possible (4 which are only defined for one input value, 1 which is completely undefined.)

> a:: Bool → Bool
> a x | x == True  = True

> b:: Bool → Bool
> b x | x == True  = False

> c:: Bool → Bool
> c x | x == False = False

> d:: Bool → Bool
> d x | x == False = True

It's a bit nonsensical to make a partial function on booleans, so I can't think of meaningful names for these. I'm also not including the undefined function because I don't know how to make it syntactically sound.

2.1.2: How many total functions are there that take two Booleans as an
input and return one Boolean? Or put differently, how many func-
tions are there of type (Bool, Bool) → Bool? Define at least four. Try
to vary the definitional style by using different features of Haskell,
e.g. predefined operators such as || and &&, conditional expres-
sions (if . . then . . else . .), guards, and pattern matching.

There are 2^2 = 4 possible input values for functions of type (Bool, Bool) → Bool, and two possible outcomes for each, so there are a total of 2^4 = 16 total functions possible.
If you allow partial functions, each input value has 3 possible outcomes (True, False, undefined), so there are 3^4 = 81 functions possible.

Some functions (_ is added sometimes to avoid using existing haskell functions):

> and_:: (Bool, Bool) → Bool
> and_(x,y) | (x,y) == (True,True) = True
>           | otherwise            = False

> exor:: (Bool, Bool) → Bool
> exor (x,y) = (x || y) && not (x && y)

> implication:: (Bool, Bool) → Bool
> implication (x,y) = if ((x,y) == (True, False))then False else True

> or_:: (Bool, Bool) → Bool
> or_ (x,y) | x         = True
>           | y         = True
>           | otherwise = False

2.1.3: What about functions of type Bool → Bool → Bool?

These are practically the same as functions of type (Bool, Bool) → Bool: they take two booleans as input and return a single boolean.

2.2.1: Define an equality test for strings that, unlike ==, disregards case,
e.g.g "Ralf" == "raLF" => False but equal "Ralf" "raLF" => True.

> equal :: String → String → Bool
> equal s1 s2 = map toLower s1 == map toLower s2

2.2.2: Define predicates
  isNumeral :: String → Bool
  isBlank   :: String → Bool
that test whether a string consists solely of digits or white space.
You may find the predefined function and :: [Bool] → Bool useful
which conjoins a list of Booleans e.g. and [1 > 2, 2 < 3] => False
and and [1 < 2, 2 < 3] => True.

> isNumeral:: String → Bool
> isNumeral s = let isNumber = \c → elem c ['0'..'9']
>               in and (map isNumber s)

> isBlank:: String → Bool
> isBlank s = let isWhitespace = \c → elem c [' ', '\t', '\n']
>               in and (map isWhitespace s)

2.2.3: Define functions
  fromDigit :: Char → Int
  toDigit :: Int → Char
that convert a digit into an integer and vice versa, e.g. fromDigit ’7’ =>
7 and toDigit 8 => ’8’.

> fromDigit :: Char → Int
> fromDigit c | elem c ['0'..'9']  = ord c - 48 -- trivial solution: use digitToInt

> toDigit :: Int → Char
> toDigit d = chr (d + 48)

2.2.4: Implement the Caesar cipher shift :: Int → Char → Char e.g. shift 3
maps ’A’ to ’D’, ’B’ to ’E’, ..., ’Y’ to ’B’, and ’Z’ to ’C’. Try
to decode the following message (map is your friend).

> msg = "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ JHLJBZ KPJABT HYJUBT LZA ULBAYVU"

> shift :: Int → Char → Char
> shift i c | elem c ['A'..'Z'] = chr ((((ord c - 65) + i) `mod` 26 ) + 65)
>           | otherwise         = c

Generate all 26 options manually to find one that makes sense:

> options = let decode =  \i → (i, map (shift i) msg)
>           in map decode [1..26]

For i = 19 the string decodes into "FABER EST SUAE QUISQUE FORTUNAE APPIUS CLAUDIUS CAECUS DICTUM ARCNUM EST NEUTRON"

2.3: Explore the difference between machine-
integers of type Int and mathematical integers of type Integer. Fire up
GHCi and type:
  product [1..10] :: Int
  product [1..20] :: Int
  product [1..21] :: Int
  product [1..65] :: Int
  product [1..66] :: Int
The expression product [1 . . n] calculates the product of the numbers
from 1 up to n, aka the factorial of n. The type annotation ::Int instructs
the compiler to perform the multiplications using machine-integers. Re-
peat the exercise using the type annotation ::Integer. What do you ob-
serve? Can you explain the differences? On my machine the expression
product [1..66] :: Int yields 0. Why? (Something to keep in mind. Espe-
cially, if you plan to work in finance!)

For the larger factorials, the Int values are sometimes negative or 0 in the case of 66!. The Integer values give the actual numbers. This is because machine integers are bounded, since they are stored using a finite number of bits. I assume that performing operations that exceed their bounds cause roll-over errors, which is why the values start to vary wildly.
The Integer type represents actual integers. I assume that the number of bits used to represent them is changed when needed, which would mean that when the bounds of the current storage space are exceeded, the variable is assigned a longer bitstring, avoiding roll-over errors.

2.4.1: Define a function
  swap :: (Int, Int) → (Int, Int)
that swaps the two components of a pair. Define two other func-
tions of this type (be inventive).

> swap :: (Int, Int) → (Int, Int)
> swap (x, y) = (y,x)

> sort :: (Int, Int) → (Int, Int)
> sort (x, y) | x ≥ y     = (x,y)
>             | otherwise = (y,x)

> complex_conjugate :: (Int, Int) → (Int, Int)
> complex_conjugate (x, y) = (x, 0 - y)

2.4.2. What happens if we change the type to
  swap :: (a, b) → (b, a)
Is your original definition of swap still valid? What about the other
two functions that you have implemented?

The swap function is still valid, the other two are not. There are not many meaningful functions for which that type would apply anyway: the function type requires that we at least always change the order of the two elements (i.e. apply the swap function), but we cannot do much else with the elements because we don't assume type.

2.5.1: Which of the following ex pressions are well-formed and well-typed? Assume that the identifier b has type Bool.
  (+4)
  div
  div 7
  (div 7) 4
  div (7 4)
  7 ‘div‘ 4
  + 37
  (+) 3 7
  (b, ’b’, "b")
  (abs, ’abs’, "abs")
  abs ◦ negate
  (∗3) ◦ (+3)

Well-formed and well-typed expressions:
  (+4)
  div
  div 7
  (div 7) 4
  7 ‘div‘ 4
  (+) 3 7
  (b, ’b’, "b")

  abs ◦ negate
  (∗3) ◦ (+3)

Not:
  div (7 4)
  + 37
  (abs, ’abs’, "abs")

2.5.2: What about these?
  (abs◦) ◦ (◦negate)
  (div◦) ◦ (◦mod)
(They are more tricky—don’t spend too much time on this.)

(abs◦) ◦ (◦negate):
(abs◦) is of type (Num a) ⇒ (a → a) → (a → a)
(◦negate) is of the same type.
So when you concatenate them you still have a well-formed function, of type (Num a) ⇒ (a → a) → (a → a).

(div◦) ◦ (◦mod):
div is of type (Integral a) ⇒ a → (a → a)
So (div◦) is of type (Integral a) ⇒ (b → a) → b → a → a

(◦mod) is also of type (Integral a) ⇒ a → (a → a)
So (◦mod) is of type (Integral a) ⇒ ((a → a) → b) → a → b

So (div◦) ◦ (◦mod) is of type (Integral a) ⇒ ((a → a) → a) → a → a → a
I.e. it is well-typed.

2.5.3: Try to infer the types of the following definitions.
  i x = x
  k (x, y) = x
  b (x, y, z) = (x z) y
  c (x, y, z) = x (y z)
  s (x, y, z) = (x z) (y z)

i :: a → a
k :: (a, b) → a
b :: (a → b → c, a, b) → c
c :: (a → b, c → a, c) → b
s :: (a → b → c, a → b, a) → c

2.6.1: Define total functions of the following types:
(a) Int → Int
(b) a → a
(c) (Int, Int) → Int
(d) (a, a) → a
(e) (a, b) → a
How many total functions are there of type Int → Int? By contrast,
how many total functions are there of type a → a?

> plusone :: Int → Int
> plusone x = x + 1

> copy :: a → a
> copy x = x

> highest :: (Int, Int) → Int
> highest (x,y) | x ≥ y      = x
>               | otherwise  = y

> second :: (a, a) → a
> second (x, y) = y

> first :: (a, b) → a
> first (x, y) = x

There is an uncountable set of total functions of type Integer → Integer, though the set of computable functions is countably infinite. Since the Int type is bounded, the total number of total functions is determined by the fact that there are 2^64 outputs for each of the 2^64 possible inputs, so 2^64^(2^64) functions total.
There is, as far as I know, only one function of type a → a, namely the identity function. Because we cannot assume anything about a, we cannot 'do' anything with it.

2.6.2 Define total functions of the following types:
(a) (a, a) → (a, a)
(b) (a, b) → (b, a)
(c) (a → b) → a → b
(d) (a, x) → a
(e) (x → a → b, a, x) → b
(f) (a → b, x → a, x) → b
(g) (x → a → b, x → a, x) → b

> exc_2_6_2_a :: (a, a) → (a, a)
> exc_2_6_2_a (x, y) = (x, y)

> exc_2_6_2_b ::  (a, b) → (b, a)
> exc_2_6_2_b (x, y) = (y, x)

> exc_2_6_2_c :: (a → b) → a → b
> exc_2_6_2_c f =  f

> exc_2_6_2_d :: (a, x) → a
> exc_2_6_2_d (n, m) = n

> exc_2_6_2_e :: (x → a → b, a, x) → b
> exc_2_6_2_e (p, q, r) = p r q

> exc_2_6_2_f :: (a → b, x → a, x) → b
> exc_2_6_2_f (p, q, r) = p (q r)

> exc_2_6_2_g :: (x → a → b, x → a, x) → b
> exc_2_6_2_g (p, q, r) = p r (q r)

2.6.3
Define total functions of the following types:
  (a) Int → (Int → Int)
  (b) (Int → Int) → Int
  (c) a → (a → a)
  (d) (a → a) → a
How many total functions are there of type (Int → Int) → Int? By
contrast, how many total functions are there of type (a → a) → a?

> plus :: Int → (Int → Int)
> plus x = (+ x)

> bias :: (Int → Int) → Int
> bias f = f 0

> map_to :: a → (a → a)
> map_to x = \y → x

There are 2^64^(2^64) functions of type (Int → Int) (see 2.6.1), which is the number of input values for functions of type (Int → Int) → Int. There are 2^64 possible output values, so 2^64^(2^64^(2^64)) possible total functions of this type.
There are no total functions of type  (a → a) → a.

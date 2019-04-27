> {-# LANGUAGE UnicodeSyntax #-}
> module DNA
> where
> import Prelude hiding (filter)
> import Unicode
> import List
> import Data.List hiding (filter)

Nucleobases or DNA-bases are the basic building blocks of
deoxyribonucleic acid (DNA).

> data Base  =  A | C | G | T
>   deriving (Eq, Ord)

Adenin (A), Cytosin (C), Guanin (G) und Thymin (T).

> instance Show Base where
>   showsPrec _ A  =  showChar 'A'
>   showsPrec _ C  =  showChar 'C'
>   showsPrec _ G  =  showChar 'G'
>   showsPrec _ T  =  showChar 'T'
>
>   showList  =  foldr (.) id . map shows

> base ∷ Char → Maybe Base
> base 'A'  =  Just A
> base 'C'  =  Just C
> base 'G'  =  Just G
> base 'T'  =  Just T
> base _    =  Nothing

> type DNA      =  [Base]
> type Segment  =  [Base]

> dna  ∷  DNA
> dna  =  [A, T, G, T, A, A, A, G, G, G, T, C, C, A, A, T, G, A]

> mm  ∷  DNA
> mm  =  filter base
>    "ATGTAAAGGGTCCAATGACTGGAATTACTTCACAGCCCTGACACTGTGGAGAGATGGATA\
>    \CCAGTTTAATTATGGACAGCTGAGAATAATCTGGGAGTCACCTGCTCTGGATATTTATCA\
>    \TCATTATGAACTTCTCACCAGGCTGTGGCCCGAACACTTTCATAACGTCCCATTTGTGTT\
>    \GGGCAGACATTATGATCTATACAGAACCTGCCTTGTCTGTCACCTTTGAATTTATGGATA\
>    \GACAATTTAATAT\
>    \GTGTTCCTGGCAGCAAAGATAATCATGGAGAGTGGAGAGAAACTAACCTTACCATTGATA\
>    \GGGAAACTCTTGAAGTGTCAACTTCTCCATATTAAATCCAAGGACCAGCAGAGACGAGAA\
>    \AATGAAAAGAAGATGGTAGAAGAAAGAACAAAATCAGAAAAAGACAAAGGAAAAGGGAAG\
>    \TCTCCAAAGGAGAAGAAAGTTGCCAGTGCCAAGCCTGGGAAGGGAAAGAGCAAGGACCAG"

> readDNA ∷ FilePath -> IO [Base]
> readDNA path
>   =  do  x ← readFile path
>          return (filter base x)


3.4.1 : Define a function contains :: Segment → DNA → Bool that checks
whether a specified DNA segment is contained in a DNA strand.
Can you modify the definition so that a list of positions of occur-
rences is returned instead?

> contains ∷ Segment → DNA → Bool
> contains segment dna = let snippets = map (take (length segment)) (tails dna)
>                            matches = [s | s ← snippets, s == segment ]
>                        in length matches > 0

> count ∷ Segment → DNA → Int
> count segment dna = let snippets = map (take (length segment)) (tails dna)
>                         matches = [s | s ← snippets, s == segment ]
>                     in length matches

3.4.2 : Define a function longestOnlyAs :: DNA → Integer that computes
(the length of) the longest segment that contains only the base A.

> longestOnlyAs       ∷ DNA → Integer
> longestOnlyAs dna = let starts = [ segment | segment ← tails dna]
>                         countAs = \s → head [n | n ← [(length s)..1], and (map (== A) (take n s))]
>                     in toInteger (head (sort (map countAs starts)))

3.4.3 : Define a function longestAtMostTenAs :: DNA → Integer that com-
putes (the length of) the longest segment that contains at most ten
occurrences of the base A. (This is more challenging. Don’t spend
too much time on this part.)

> longestAtMostTenAs  ∷ DNA → Integer
> longestAtMostTenAs dna = let starts = [ segment | segment ← tails dna]
>                              countAs = \s → head [n | n ← [(length s)..1], and (map (== A) (take n s))]
>                              sequenceLengths =  [ l | l ← (map countAs starts), l ≤ 10]
>                          in toInteger (head (sort sequenceLengths))

If you want to test your code on a larger example, say within GHCi

dna <- readDNA "mm1.dna"
longestOnlyAs dna

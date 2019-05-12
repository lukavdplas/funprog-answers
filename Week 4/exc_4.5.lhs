> {-# LANGUAGE UnicodeSyntax #-}
> module RedBlackTree
> where
> import Unicode
> import QuickTest

> data RedBlackTree elem
>   =  Leaf
>   |  Red    (RedBlackTree elem) elem (RedBlackTree elem)
>   |  Black  (RedBlackTree elem) elem (RedBlackTree elem)
>   deriving (Show)

> instance Functor RedBlackTree where
>   fmap _f (Leaf)         =  Leaf
>   fmap f  (Red l a r)    =  Red (fmap f l) (f a) (fmap f r)
>   fmap f  (Black l a r)  =  Black (fmap f l) (f a) (fmap f r)

4.5.1 : Adapt the membership test to red-black trees.

> member ∷ (Ord elem) ⇒ elem → RedBlackTree elem → Bool
> member _x Leaf       = False
> member x (Red l a r) = (a == x) || (member x l) || (member x r)
> member x (Black l a r) = (a == x) || (member x l) || (member x r)

2. Adapt the insertion algorithm to red-black trees. (Do not worry
about red- and black-conditions initially.)

> insert ∷ (Ord elem) ⇒ elem → Tree elem → Tree elem
> insert x Leaf =  Red Empty x Empty
> insert x (Red l a r) = Red (insert x l) a r
> insert x (Black l a r) = Black (insert x l) a r

insert ∷ (Ord elem) ⇒ elem → RedBlackTree elem → RedBlackTree elem
black ∷ RedBlackTree elem → elem → RedBlackTree elem → RedBlackTree elem
isRedBlackTree ∷ RedBlackTree elem → Bool
redBlackTrees ∷ [elem] → Probes (RedBlackTree elem)

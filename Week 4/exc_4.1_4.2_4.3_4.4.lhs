> {-# LANGUAGE UnicodeSyntax #-}
> module BinarySearchTree
> where
> import Unicode
> import BinaryTree  -- hiding (member)
> import QuickTest

4.1.1 : Capture the binary tree shown in Figure 1 as a Haskell expression
of type Tree Char.

> tree = Node (Node Empty 'a' (Node Empty 'b' Empty)) 'c' (Node (Node Empty 'd' Empty) 'f' (Node Empty 'g' Empty))

4.1.2 : Conversely, picture the Haskell expressions below.

Node Empty 4711 (Node Empty 0815 (Node Empty 42 Empty))

    4711
   /    \
0815    42
        / \
       e   e

Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty

              "Ralf"
              /    \
        "Peter"    e
        /     \
  "Frits"     e
 /       \
e         e

Node (Node Empty ’a’ Empty) ’k’ (Node Empty ’z’ Empty)

    'k'
   /   \
 'a'   'z'
 / \   / \
e   e e   e


4.1.3 : We have emphasized in the lectures that every datatype comes with
a pattern of definition. Write down the “tree design pattern”. Apply
the design pattern to define a function size :: Tree elem → Int that
calculates the number of elements contained in a given tree. The
size of the tree shown in Figure 1 is 6.

Tree design pattern:
Empty
or
(Node l a r)

> size :: Tree elem → Int
> size Empty        = 0
> size (Node l a r) = 1 + size l + size r

4.1.4 : Define functions minHeight, maxHeight :: Tree elem → Int that cal-
culate the length of the shortest and the length of the longest path
from the root to a leaf. The minimum height of our running exam-
ple in Figure 1 is 2; the maximum height is 3.

> minHeight :: Tree elem → Int
> minHeight Empty        = 0
> minHeight (Node l a r) = 1 + (min (minHeight l) (minHeight r))

> maxHeight :: Tree elem → Int
> maxHeight Empty        = 0
> maxHeight (Node l a r) = 1 + (max (minHeight l) (minHeight r))

4.1.5 : What is the relation between the size, the minimal, and the maximal
height of a tree?

For a tree of size n, the minHeight and maxHeight are both at most n. (In this case, all nodes have only left daughters or they all have only right daughters.) The lower bound for the minHeight or maxHeight of a tree is defined by the tree that is completely branched out. In that case, the maxHeight of the tree is equal to ⌈log(n+1)/log(2)⌉, and the minHeight is equal to ⌊log(n+1)/log(2)⌋.

4.1.6 : Define a function member :: (Eq elem) ⇒ elem → Tree elem → Bool
that determines whether a specified element is contained in a given
binary tree.

> member :: (Eq elem) ⇒ elem → Tree elem → Bool
> member _x Empty       = False
> member x (Node l a r) = (a == x) || (member x l) || (member x r)

4.2.1 : Define functions preorder, inorder, postorder :: Tree elem → [elem]
that return the elements contained in a tree in pre-, in-, and post-
order, respectively.

> preorder :: Tree elem → [elem]
> preorder Empty        = []
> preorder (Node l a r) = [a] ++ (preorder l) ++ (preorder r)

> inorder :: Tree elem → [elem]
> inorder Empty        = []
> inorder (Node l a r) =  (inorder l) ++ [a] ++ (inorder r)

> postorder :: Tree elem → [elem]
> postorder Empty        = []
> postorder (Node l a r) = (postorder l) ++ (postorder r) ++ [a]

4.2.2 : Define a function layout :: (Show elem) ⇒ Tree elem → String that
turns a tree into a string, showing one element per line and empha-
sizing the structure through indentation e.g. putStr (layout abcdfg)
produces (turn your head by 90 ◦ to the left):

    / ’a’
        \ ’b’
- ’c’
        / ’d’
    \ ’f’
        \ ’g’

> layout :: (Show elem) ⇒ Tree elem → String
> layout Empty = ""
> layout (Node l a r) = let line a = " " ++ show a ++ "\n"
>                           ind = "   "
>                           branch prefix edge (Node l a r) = (branch (prefix ++ ind) "/" l) ++ prefix ++ edge ++ line a ++ (branch (prefix ++ ind) "\\" r)
>                           branch _prefix _edge Empty = ""
>                       in (branch ind "/" l) ++ "-" ++ line a ++ (branch ind "\\" r)

4.3.1 : Define a function
build :: [elem] → Tree elem that constructs a binary tree from a
given list of elements such that inorder ◦ build = id. The shape of
the tree does not matter. (Apply the list design pattern.)

> build :: [elem] → Tree elem
> build ([])     = Empty
> build (x : xs) = Node Empty x (build xs)

4.3.2 : Define a function balanced :: [elem] → Tree elem that constructs
a balanced tree from a given list of elements, i.e. for each node
the size of the left and the size of the right sub-tree should differ
by at most one. The order of elements, however, does not matter.
(Again, apply the list design pattern.)

> balanced :: [elem] → Tree elem
> balanced([])      = Empty

4.3.3 : Harry Hacker claims that his function create :: Int → Tree () can
construct a tree of size n in logarithmic time i.e. create n takes
Θ(log n) steps. Genius or quacksalver?

I think that constructing a tree in logarithmic time is definitively impossible. In order for a computer to create a tree in its memory, it would need to calculate or retrieve a value for every node in the tree, and save it in the memory as the value of that node. In other words, there is a procedure that needs to be run for every node, and which will take at least a fixed amount of time. As such, the time complexity of a constructor for a tree of size n would have to be at least Θ(n). So Harry Hacker's claim that his constructor can use fewer steps must be false.

4.4.1 : registry defined below is a binary search tree. Define a function
member :: (Ord elem) ⇒ elem → Tree elem → Bool that determines whether a
 specified element is contained in a given binary search tree. What’s the
 difference to Exercise 4.1.6?

> registry  ∷  Tree String
> registry  =  Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty

The difference is that we can use the ordered structure of the tree to do a faster search:

> member2 ∷ (Ord elem) ⇒ elem → Tree elem → Bool
> member2 _x Empty       = False
> member2 x (Node l a r) | x < a  = (member x l)
>                        | x > a  = (member x r)
>                        | x == a = True

4.4.2 : Define a function insert :: (Ord elem) ⇒ elem → Tree elem →
Tree elem that inserts an element into a search tree.

> insert ∷ (Ord elem) ⇒ elem → Tree elem → Tree elem
> insert x Empty = Node Empty x Empty
> insert x (Node l a r) | x < a     = Node (insert x l) a r
>                       | otherwise = Node l a (insert x r)

4.4.3 : Define a function delete :: (Ord elem) ⇒ elem → Tree elem →
Tree elem that removes an element from a binary search tree.

> delete ∷ (Ord elem) ⇒ elem → Tree elem → Tree elem
> delete x Empty = Empty
> delete x (Node l a r) | x < a  = Node (delete x l) a r
>                       | x > a  = Node l a (delete x r)
>                       | x == a = let insertTree Empty t = t
>                                      insertTree (Node l a r) Empty = Node l a r
>                                      insertTree (Node l a r) (Node l2 a2 r2) | a < a2 = Node (insertTree (Node l a r) l2) a2 r2
>                                                                              | otherwise = Node l2 a2 (insertTree (Node l a r) r2)
>                                  in insertTree r l

4.4.4 : Use the library of Exercise 3.5 to test your code. In particular,
define a function isSearchTree :: (Ord elem) ⇒ Tree elem → Bool
that checks whether a given binary tree satisfies the search tree
property. The most difficult part is to define a function that
generates binary search trees. One approach is to program a
function trees :: [elem] → Probes (Tree elem) that generates all
trees whose inorder traversal yields the given list of elements:
and [inorder t == xs | t ← trees xs]. Applied to an ordered list,
tree will then generate search trees.

> isSearchTree ∷ (Ord elem) ⇒ Tree elem → Bool
> isSearchTree Empty        = True
> isSearchTree (Node l a r) = let isEmpty Empty = True
>                                 isEmpty (Node l a r) = False
>                                 leftconstraint = isEmpty l || (last (inorder l)) ≤ a
>                                 rightconstraint = isEmpty r || (head (inorder r)) ≥ a
>                             in isSearchTree l && isSearchTree r && leftconstraint && rightconstraint

> trees ∷ [elem] → Probes (Tree elem)  -- should be defined in BinaryTree
> trees [] = [Empty]
> trees xs = [ (Node t1 (last (take n xs)) t2 ) | n ← [1..(length xs)],
>                                                 t1 ← trees (take (n - 1) xs),
>                                                 t2 ← trees (reverse (take ((length xs) - n) (reverse xs))) ]

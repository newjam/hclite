filename: AvlTree.lhs
author:  james newman
date: spring 2010, updated Wed Nov 10 12:19:05 PST 2010
desc: This is a haskell implementation of AVL tree algorithms.
this AVL tree has been modified to have a payload to implement an associative array or hash table.

> module AvlTree 
>  (mkTree, mkNode, get, set, BTree, collapse) where

import Keyable

 get :: Keyable a => a -> BTree a -> Maybe a
 set ::  a => a -> BTree a -> BTree a

> set x t = insert (mkNode x) t
> get _ NilT         = Nothing
> get x (Node a l r) = case compare x a of
>  GT -> get x r
>  LT -> get x l
>  EQ -> Just a

> data BTree a = NilT
>              | Node a (BTree a) (BTree a)
>  deriving Show

> instance Functor (BTree) where
>  fmap f NilT         = NilT
>  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

AvlTree> fmap (\x->x^2) $ Node 2 (Node 1 NilT NilT) NilT
Node 4 (Node 1 NilT NilT) NilT


 testTree  = Node 3 (Node 3 NilT NilT) (Node 2 NilT (Node 1 NilT NilT) ) 
 testNode1 = Node 3 NilT NilT
 testNode2 = Node 2 NilT NilT

> height NilT = 0
> height (Node n l r) = 1 + max (height l) (height r)

> insert a b = balance ( insert' a b )
> insert' a NilT = a
> insert' NilT b = b
> insert' a@(Node valA leftA rightA) b@(Node valB leftB rightB) =
>  case compare valA valB of
>   GT -> Node valB leftB (insert a rightB) 
>   LT -> Node valB (insert a leftB) rightB
>   EQ -> insert rightA (insert leftA (Node valA leftB rightB))

this is a policy decision that discards duplicates, NOTE it must a if we want to "update" values in a map

  | valA >= valB  = Node valB leftB (insert a rightB)
  | valA <  valB  = Node valB (insert a leftB) rightB

> rrBalance (Node rootVal rootLeft (Node pivotVal pivotLeft pivotRight)) = 
>  Node pivotVal newLeft pivotRight
>  where newLeft = Node rootVal rootLeft pivotLeft

    3	  2
   /	 / \
  2	1   3
 /	
1	

> llBalance ( Node rootVal (Node pivotVal pivotLeft pivotRight) rootRight ) = 
>  Node pivotVal pivotLeft newRight
>  where newRight = Node rootVal pivotRight rootRight 

> lrBalance root@(Node rootVal rootLeft rootRight) = 
>  llBalance (Node rootVal newLeft rootRight)
>  where newLeft = rrBalance rootLeft

> rlBalance root@(Node rootVal rootLeft rootRight) = 
>  rrBalance (Node rootVal rootLeft newRight)
>  where newRight = llBalance rootRight


> rrTest = Node 1 NilT (Node 2 NilT (Node 3 NilT NilT))
> llTest = Node 3 (Node 2 (Node 1 NilT NilT) NilT ) NilT
> lrTest = Node 4 (Node 2 NilT (Node 3 NilT NilT)) NilT
> rlTest = Node 2 NilT (Node 4 (Node 3 NilT NilT) NilT)
> rTest = Node 2 (Node 1 NilT NilT) NilT

I wish there were some way to memoize the height of the tree so I'm not effectively calculating the height twice.

> balance tree@(Node val left right) = 
>   case balanceFactor tree of
>        2    -> case balanceFactor right of
>                     1    -> rrBalance tree
>                     (-1) -> rlBalance tree
>                     _    -> error "weird tree situation"
>        (-2) -> case balanceFactor left of
>                     1    -> lrBalance tree
>                     (-1) -> llBalance tree
>                     _    -> error "weird tree situation"
>        _    -> tree

> balanceFactor (Node _  left right) = (height right) - (height left)

With balance on:

Node 4 (Node 2 (Node 1 NilT NilT) (Node 3 NilT NilT)) (Node 9 (Node 6 (Node 5 NilT NilT) (Node 8 NilT NilT)) (Node 88 NilT NilT))
Main> treeToList ( binaryTreeSort [5,8,9,3,1,4,6,88,2] )
[1,2,3,4,5,6,8,9,88]

    4
   / \___
  2      9
 / \    / \
1   3  6   88
      / \
     5   8

WITHOUT balance on
Main> :r
Main> binaryTreeSort [5,8,9,3,1,4,6,88,2]
Node 2 (Node 1 NilT NilT) (Node 88 (Node 6 (Node 4 (Node 3 NilT NilT) (Node 5 NilT NilT)) (Node 9 (Node 8 NilT NilT) NilT)) NilT)

  2
 / \__
1     88    
     / 
    6   
   / \___
  4      9
 / \    /
3   5  8


> binaryTreeSort [x]    = Node x NilT NilT
> binaryTreeSort (x:xs) = insert (Node x NilT NilT) (binaryTreeSort xs)

 binaryTreeSort (x:xs) = foldr (\x->insert ()

> mkNode x      = Node x NilT NilT
> mkTree rs = foldr (\x -> insert (mkNode x)) NilT rs

> treeToList NilT           = []
> treeToList (Node val l r) = (treeToList l) ++ [val] ++ (treeToList r)

> collapse = treeToList

Main> binaryTreeSort [5,6,8,2,7]
Node 7 (Node 2 NilT (Node 6 (Node 5 NilT NilT) NilT)) (Node 8 NilT NilT)
Main> treeToList ( binaryTreeSort [5,6,8,2,7] )
[2,5,6,7,8]

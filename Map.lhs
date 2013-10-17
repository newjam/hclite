name: Map.lhs
author: James Newman
date: Wed Nov 10 12:51:13 PST 2010
desc: a map ADT using an avl balanced tree.

> module Map
> (set, get, mkMap, Map, toList, isIn, unsafeGet) where

> import qualified AvlTree

> import Control.Applicative

> toList (Map t) = map toTuple . AvlTree.collapse $ t

> toTuple (KVP (Key k) (Val v)) = (k, v)

> instance (Show a, Show b) => Show (Map a b) where
>   show m = show . toList $ m

> data Map a b = Map (AvlTree.BTree (KeyValuePair a b))

> mkMap ps  = Map (AvlTree.mkTree . map (\(k,v)->KVP (Key k) (Val v)) $ ps)

> set k v (Map t) = Map (AvlTree.set (KVP (Key k) (Val v)) t)
> get k   (Map t) = do KVP k v <- AvlTree.get (KVP (Key k) NoVal    ) t
>                      return (val v) 

Map> get 1 $ mkMap [(1,'a'),(2,'b')]
Just 'a'
Map> get 5 $ mkMap [(1,'a'),(2,'b')]
Nothing
Map> get 3 $ set 3 'c' $ mkMap [(1,'a'),(2,'b')]
Just 'c'

> data KeyValuePair k v = KVP {getKey::(Key k), getVal::(Val v)}
>  deriving Show                      

> instance Eq k => Eq (KeyValuePair k v) where
>  (==)    = (==)    `on` getKey
> instance Ord k => Ord (KeyValuePair k v) where
>  compare = compare `on` getKey

> (*) `on` f = \x y -> f x * f y

> newtype Key k = Key k
>  deriving (Eq, Ord, Show)
> data Val v = Val {val::v}
>            | NoVal
>  deriving (Show)

> instance Functor Val where
>  fmap f NoVal   = NoVal
>  fmap f (Val v) = Val (f v)

 instance Applicative KeyValuePair where
  pure k = KVP (Key k) NoVal

> instance Ord k => Applicative (Map k) where
>  (Map x) <*> m = mkMap [ (k, vf (unsafeGet k m)) | (KVP (Key k) (Val vf)) <- AvlTree.collapse x, k `isIn` m]

> k `isIn` m = case get k m of
>  Nothing -> False
>  Just  _ -> True

> k `unsafeGet` m = case get k m of
>  Nothing -> error "key not present in map"
>  Just  x -> x

Map> get 1 $ (fmap (*) $ mkMap [(1,2),(2,3)]) <*> (mkMap [(1,2),(2,2)])
Just 4
Map> get 2 $ (fmap (*) $ mkMap [(1,2),(2,3)]) <*> (mkMap [(1,2),(2,2)])
Just 6


 testap :: Map Integer Integer
 testap = (fmap (2 *) $ mkMap [(1,2),(2,3)]) <*> mkMap [(1,2),(2,2)]


> instance Ord k => Functor (KeyValuePair k) where
>  fmap f (KVP k v) = KVP k (fmap f v)

Map> fmap (\x->x^2) $ KVP (Key 2) (Val 4.0)
KVP {getKey = Key 2, getVal = Val {val = 16.0}}

> instance Ord k => Functor (Map k) where
>  fmap f (Map t) = Map $ fmap (fmap f) t

the '(fmap f)' is required because we must map the function f over the KeyValuePair context.

Map> get 1 $ fmap (replicate 3) $ mkMap [(1,'a'),(2,'b')]
Just "aaa"
Map> get 2 $ fmap (replicate 3) $ mkMap [(1,'a'),(2,'b')]
Just "bbb"


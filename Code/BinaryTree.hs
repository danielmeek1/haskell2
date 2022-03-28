module BinaryTree where

data BTree a = Leaf
               | Node (BTree a) a (BTree a)
      deriving (Show)

insert :: Ord a => BTree a -> a -> BTree a
insert Leaf a = Node Leaf a Leaf
insert (Node  l v r) a | a == v = Node l a r
                       | a < v = Node (insert l a) v r
                       | a > v = Node r v (insert r a)

getElem :: (Ord a) => BTree a -> a -> Maybe a
getElem Leaf _ = Nothing
getElem (Node l v r) a | a==v= Just v
                       | a< v= getElem l a
                       | a> v= getElem r a


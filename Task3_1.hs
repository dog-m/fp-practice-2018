module Task3_1 where

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

-- это не надо
class Functor f
  where
    fmap :: (a -> b) -> f a -> f b

-- это надо
instance Functor BinaryTree
  where
    fmap f (EmptyBinaryTree) = EmptyBinaryTree
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node e l r) = Node (f e) (fmap f l) (fmap f r)

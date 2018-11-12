module Task3_2 where

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)
import Task2_2

data ReverseList a = RNil | RCons (ReverseList a) a
  deriving (Show, Eq)

rlistToList :: ReverseList a -> [a]
rlistToList lst = reverse $ unfoldr foo lst
  where
    foo list = case list of
      RNil         -> Nothing
      RCons h elem -> Just (elem, h)

listToRList :: [a] -> ReverseList a
listToRList lst = foldl ( \acc elem -> RCons acc elem ) RNil lst

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor


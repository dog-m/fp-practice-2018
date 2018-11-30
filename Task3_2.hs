module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList RNil = []
rlistToList (RCons RNil a) = [a]
rlistToList (RCons head a) = (rlistToList head) ++ [a]

listToRList :: [a] -> ReverseList a
listToRList lst = foldl (\acc elem -> RCons acc elem) RNil lst

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor
instance Eq a => Eq (ReverseList a) where
  (==) RNil RNil = True
  (==) RNil _ = False -- разной длины
  (==) _ RNil = False -- разной длины
  -- одинаковые элементы + одинаковая длина
  (==) (RCons ha a) (RCons hb b) = (a == b) && (ha == hb)

instance Ord a => Ord (ReverseList a) where
  (<=) RNil RNil = True  -- равной дилнны
  (<=) RNil _    = True  -- левый короче = меньше
  (<=) _    RNil = False -- правй короче = больше
   -- каждый элемент меньше или равен + длина
  (<=) (RCons ha a) (RCons hb b) = (a <= b) && (ha <= hb)

instance Show a => Show (ReverseList a) where
  show RNil = "[]"
  show lst = "[" ++ printRList lst ++ "]"
    where
      printRList (RCons RNil a) = show a
      printRList (RCons head a) = printRList head ++ ", " ++ show a

-- зачем эта конструкция здесь?
instance Semigroup (ReverseList a) where
  (<>) a b = mappend a b

instance Monoid (ReverseList a) where
  -- пустой список
  mempty = RNil

  -- добавить один список к другому (превращаем второй список в массив и добавляем в конец)
  mappend RNil RNil = RNil
  mappend RNil a = a
  mappend a RNil = a
  mappend a b = foldr (\elem acc -> RCons acc elem) a (rlistToList b)

instance Functor ReverseList where
  fmap _ RNil = RNil
  fmap f (RCons head a) = RCons (fmap f head) (f a)

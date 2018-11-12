module Task2_2 where

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl func acc [] = acc
foldl func acc (h:t) = foldl func (func acc h) t

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr func acc [] = acc 
foldr func acc (h:t) = func h (foldr func acc t)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr foo b =
  case (foo b) of
    Nothing     -> []
    Just (a, b) -> a : unfoldr foo b

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map func list = foldr ( \x a -> ((func x):a) ) [] list

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product lst = foldl (*) 1 lst

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes lst = foldl f [] lst
  where
    f t (Just a) = a:t
    f t Nothing  = t

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal = todo

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot foo (h:t) =
  case (foo h) of
    True  -> h : filterNot foo t
    False ->     filterNot foo t

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem = todo

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr foo from
  where
    foo acc | acc < to = Just (acc, acc + step)
    foo _ = Nothing

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append a b = foldr( \x a -> x:a ) b a

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = todo

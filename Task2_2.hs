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
unfoldr func a list = 
  case b of
    Nothing    -> list
    Just(a, b) -> a:(unfoldr func b (a:lst))
  where
    b = func a

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
product = foldl (*) 1 lst

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes = foldl f [] lst
  where
    f t (Just a) = a:t
    f t Nothing  = t

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal = todo

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot = todo

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem = todo

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = todo

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append a b = foldr(\x a -> x: a) b a

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = todo

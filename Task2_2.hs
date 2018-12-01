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
map func list = foldr foo [] list
  where
    foo x a = (func x):a

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
diagonal m = fst $ foldl foo ([], 0) m
  where
    foo (result, n) lst = (result ++ [lst !! n], n + 1)

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f lst = foldr foo [] lst
  where
    foo x acc = if (f x) then acc else (x: acc)
    -- не удовлетворяет => должен попасть в результат?

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem x list = foldl foo False list
  where
    foo acc element = acc || (element == x)

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr foo from
  where
    foo acc | acc < to = Just (acc, acc + step)
    foo _              = Nothing

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append a b = foldr( \x a -> x:a ) a b

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups _   n | n < 1 = error "Invalid group length"
groups lst n = reverse $ fst $ foldl foo ([], 0) lst
  where
    foo ( [] , _       ) x = ( [[x]], 1 )
    foo ( h:t, counter ) x
      | counter == n = ( [[x]] ++ (h:t), 1 )
      | otherwise    = ( (h ++ [x]):t  , counter + 1 )

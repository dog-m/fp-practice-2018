module Task1_2 where

import Todo(todo)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = do
  let fact i = if i < 1 then 1 else i * fact(i - 1)
  let sin' n =
        if n < 0
          then 0
          else sin'(n - 1) + (-1)^n * x^(2 * n + 1) / fromIntegral (fact (2 * n + 1))
  sin' 11

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = do
  let fact i = if i < 1 then 1 else i * fact(i - 1)
  let cos' n =
        if n < 0
          then 0
          else cos'(n - 1) + (-1)^n * x^(2 * n) / fromIntegral (fact (2 * n))
  cos' 11

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x y = do
  let gcd_sorted a b = if b == 0 then a else gcd_sorted b (a `mod` b)
  if x > y
    then gcd_sorted y x
    else gcd_sorted x y

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = todo

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = todo

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y = do
  let pow_plus a b =
        if b == 0
          then 1
          else a * (pow_plus a (b - 1))
  if y < 0
    then 0
    else pow_plus x y

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = do
  let checkPrimeRecursive i = 
        if i >= x
          then True
          else
            if (Task1_2.gcd x i) > 1
              then False
              else checkPrimeRecursive (i+1)
  if x < 1
    then False
    else checkPrimeRecursive 2

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть 
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo

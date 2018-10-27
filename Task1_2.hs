module Task1_2 where

import Todo(todo)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = sin' (norm x) 1e-30 1 x
  where
    norm a | a >  2 * pi = norm (a - pi - pi)
    norm a | a < -2 * pi = norm (a + pi + pi)
    norm a = a
    sin' _ eps _ q | abs q < eps = 0
    sin' a eps i q = q + sin' a eps (i + 1) (q * f)
      where
        f = (-1) * a * a / ( (2 * i) * (2 * i + 1) )

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = cos' (norm x) 1e-30 1 1
  where
    norm a | a >  2 * pi = norm (a - pi - pi)
    norm a | a < -2 * pi = norm (a + pi + pi)
    norm a = a
    cos' _ eps _ q | abs q < eps = 0
    cos' a eps i q = q + cos' a eps (i + 1) (q * f)
      where
        f = (-1) * a * a / ( (2 * i) * (2 * i - 1) )

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x y | x > y = Task1_2.gcd y x
gcd x y = gcd_sorted x y
  where
    gcd_sorted a 0 = a
    gcd_sorted a b = gcd_sorted b (a `mod` b)

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
pow _ 0         = 1
pow _ y | y < 0 = 0
pow x y | y `mod` 2 == 1 = x * pow x (y - 1)
pow x y = (pow x (y `div` 2)) * (pow x (y `div` 2))

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x | x < 1 = False
isPrime x = check 2
  where
    check i | i * i > x             = True
    check i | (Task1_2.gcd x i) > 1 = False
    check i = check (i + 1)

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

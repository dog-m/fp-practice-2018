module Main where
import Task1_1
import Task1_2

main = do
  -- тесты для задания №1

  putStrLn "Task1_1:"
  -- (A - 1) * 2 - A * (3 + 4 + 5 - 6 * 7) = (A - 1) * 2 - A * (-30)
  let a = (Variable "A" |-| IntConstant 1) |*| IntConstant 2 |-| Variable "A" |*| (IntConstant 3 |+| IntConstant 4 |+| IntConstant 5 |-| IntConstant 6 |*| IntConstant 7)
  putStrLn "a:";          print $ a;          putStrLn "---"
  putStrLn "evaluate a:"; print $ evaluate a; putStrLn "---"

  -- (0 - 1) * 2 - 0 * (3 + 4 + 5 - 6 * 7) = -2
  let b = replaceVar "A" (IntConstant 0) a
  putStrLn "replaceVar:"; print $ b;          putStrLn "---"
  putStrLn "evaluate b:"; print $ evaluate b; putStrLn "---"
  
  --- 1 * (1 * Z + 0) + 0 = Z
  let c = IntConstant 1 |*| (IntConstant 1 |*| Variable "Z" |+| IntConstant 0) |+| IntConstant 0
  putStrLn "c:";          print $ c;          putStrLn "---"
  putStrLn "evaluate c:"; print $ evaluate c; putStrLn "---"

  -- тесты для задания №2

  putStrLn "Sine tests:"
  putStr "sin( 0   ) = "; print $ Task1_2.sin ( 0   )
  putStr "sin( pi/2) = "; print $ Task1_2.sin ( pi/2)
  putStr "sin(-pi/2) = "; print $ Task1_2.sin (-pi/2)
  putStr "sin( pi  ) = "; print $ Task1_2.sin ( pi  )
  
  putStr "sin( 7*pi) = "; print $ Task1_2.sin ( 7*pi)
  putStr "sin(-9*pi) = "; print $ Task1_2.sin (-9*pi)
  putStrLn ""

  putStrLn "Cosine tests:"
  putStr "cos( 0   ) = "; print $ Task1_2.cos ( 0   )
  putStr "cos( pi/2) = "; print $ Task1_2.cos ( pi/2)
  putStr "cos(-pi/2) = "; print $ Task1_2.cos (-pi/2)
  putStr "cos( pi  ) = "; print $ Task1_2.cos ( pi  )
  
  putStr "cos( 7*pi) = "; print $ Task1_2.cos ( 7*pi)
  putStr "cos(-9*pi) = "; print $ Task1_2.cos (-9*pi)
  putStrLn ""

  putStrLn "GCD tests:"
  putStr "gcd( 27, 45) = "; print $ Task1_2.gcd   27    45
  putStr "gcd( 45, 27) = "; print $ Task1_2.gcd   45    27
  putStr "gcd( -5, 10) = "; print $ Task1_2.gcd  (-5)   10
  putStr "gcd( 10, -5) = "; print $ Task1_2.gcd   10   (-5)
  putStr "gcd(-10, -5) = "; print $ Task1_2.gcd (-10)  (-5)
  putStr "gcd( -5,-10) = "; print $ Task1_2.gcd  (-5) (-10)
  putStrLn ""

  putStrLn "POW tests:"
  putStr "pow(2, 0) = "; print $ Task1_2.pow 2 0
  putStr "pow(2, 1) = "; print $ Task1_2.pow 2 1
  putStr "pow(2, 2) = "; print $ Task1_2.pow 2 2
  putStr "pow(2, 3) = "; print $ Task1_2.pow 2 3
  putStr "pow(2, 4) = "; print $ Task1_2.pow 2 4
  putStr "pow(2, 5) = "; print $ Task1_2.pow 2 5
  putStr "pow(2,10) = "; print $ Task1_2.pow 2 10
  putStr "pow(2,-8) = "; print $ Task1_2.pow 2 (-8)
  putStrLn ""

  putStrLn "isPrime tests:"
  putStr "isPrime(17) = "; print $ Task1_2.isPrime ( 19)
  putStr "isPrime(21) = "; print $ Task1_2.isPrime ( 21)
  putStr "isPrime( 1) = "; print $ Task1_2.isPrime (  1)
  putStr "isPrime( 0) = "; print $ Task1_2.isPrime (  0)
  putStr "isPrime(-1) = "; print $ Task1_2.isPrime ( -1)

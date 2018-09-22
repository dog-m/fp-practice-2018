module Main where
import Task1_1
import Task1_2

main = do
  putStr "Sine tests:\n"
  putStr "sin( 0   ) = "; print $ Task1_2.sin ( 0   )
  putStr "sin( pi/2) = "; print $ Task1_2.sin ( pi/2)
  putStr "sin(-pi/2) = "; print $ Task1_2.sin (-pi/2)
  putStr "sin( pi  ) = "; print $ Task1_2.sin ( pi  )

  putStr "\nCosine tests:\n"
  putStr "cos( 0   ) = "; print $ Task1_2.cos ( 0   )
  putStr "cos( pi/2) = "; print $ Task1_2.cos ( pi/2)
  putStr "cos(-pi/2) = "; print $ Task1_2.cos (-pi/2)
  putStr "cos( pi  ) = "; print $ Task1_2.cos ( pi  )
  
  putStr "\nGCD tests:\n"
  putStr "gcd( 27, 45) = "; print $ Task1_2.gcd   27    45
  putStr "gcd( 45, 27) = "; print $ Task1_2.gcd   45    27
  putStr "gcd( -5, 10) = "; print $ Task1_2.gcd  (-5)   10
  putStr "gcd( 10, -5) = "; print $ Task1_2.gcd   10   (-5)
  putStr "gcd(-10, -5) = "; print $ Task1_2.gcd (-10)  (-5)
  putStr "gcd( -5,-10) = "; print $ Task1_2.gcd  (-5) (-10)
  
  putStr "\nPOW tests:\n"
  putStr "pow(2, 0) = "; print $ Task1_2.pow 2 0
  putStr "pow(2, 1) = "; print $ Task1_2.pow 2 1
  putStr "pow(2, 2) = "; print $ Task1_2.pow 2 2
  putStr "pow(2, 3) = "; print $ Task1_2.pow 2 3
  putStr "pow(2, 4) = "; print $ Task1_2.pow 2 4
  putStr "pow(2, 5) = "; print $ Task1_2.pow 2 5
  putStr "pow(2, 5) = "; print $ Task1_2.pow 2 10
  putStr "pow(2,-8) = "; print $ Task1_2.pow 2 (-8)

  putStr "\nisPrime tests:\n"
  putStr "isPrime(17) = "; print $ Task1_2.isPrime ( 19)
  putStr "isPrime(21) = "; print $ Task1_2.isPrime ( 21)
  putStr "isPrime( 1) = "; print $ Task1_2.isPrime (  1)
  putStr "isPrime( 0) = "; print $ Task1_2.isPrime (  0)
  putStr "isPrime(-1) = "; print $ Task1_2.isPrime ( -1)


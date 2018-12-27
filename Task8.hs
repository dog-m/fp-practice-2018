{-
представлен парсер, разработанный нами на лекции.
Модифицируйте его таким образом, чтобы он поддерживал:
  + Числа с плавающей точкой
  * Операцию унарного минуса
  + Операцию возведения в степень
-}

module Task8 where

import Text.Parsec hiding(digit)

type Parser a = Parsec String () a

digit :: Parser Char
digit = oneOf $ ['0'..'9']

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

fraction :: Parser String
fraction = do
  char '.'
  f <- many1 digit
  return $ "." ++ f

number :: Parser Double
number = do
  a <- many1 digit
  frac <- option "" fraction
  return $ read $ a ++ frac

byNumber ::  Char 
          -> (Double -> Double -> Double) 
          -> Parser Double
          -> Parser (Double -> Double)
byNumber symbol func base =
  do
    char symbol
    spaces
    n <- base
    spaces
    return $ (`func` n)

multNumber :: Parser (Double -> Double)
multNumber = byNumber '*' (*) expr

divNumber :: Parser (Double -> Double)
divNumber = byNumber '/' (/) expr

powNumber :: Parser (Double -> Double)
powNumber = byNumber '^' (**) expr

multiplication :: Parser Double
multiplication =
  do
    x <- expr
    spaces
    ys <- many (multNumber <|> divNumber <|> powNumber)
    return $ foldl (\ x f -> f x) x ys

plusNumber :: Parser (Double -> Double)
plusNumber = byNumber '+' (+) multiplication

minusNumber :: Parser (Double -> Double)
minusNumber = byNumber '-' (-) multiplication

addition :: Parser Double
addition =
  do
    x <- multiplication
    spaces
    ys <- many (plusNumber <|> minusNumber)
    return $ foldl (\ x f -> f x) x ys

expr :: Parser Double
expr =
  number <|> expression

expression :: Parser Double
expression =
  do
    char '('
    spaces
    res <- addition
    char ')'
    spaces
    return $ res

root :: Parser Double
root =
  do
    spaces
    p <- addition
    eof
    return $ p

main =
  do
    s <- getLine
    putStrLn $ show $ parse root "<input>" s
    -- main

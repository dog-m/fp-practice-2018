module Task1_1 where

import Todo(todo)

data Operation = Plus | Minus | Times deriving(Show,Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, rhv :: Term, ac :: Operation } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm l r Plus
(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm l r Minus
(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm l r Times

infixl 1 |+|
infixl 1 |-|
infixl 2 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case expression of
  Variable v | v == varName -> replacement
  BinaryTerm l r a -> BinaryTerm (replaceVar varName replacement l) (replaceVar varName replacement r) a
  _ -> expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = case expression of
  BinaryTerm left right op ->
    let
      l = evaluate left
      r = evaluate right
    in case (l, r, op) of
      -- для упрощения выражения, если не можем посчитать
      (            _, IntConstant 0, Times) -> IntConstant 0
      (            _, IntConstant 0,     _) -> l
      (            _, IntConstant 1, Times) -> l
      (IntConstant 0,             _, Plus ) -> r
      (IntConstant 0,             _, Times) -> IntConstant 0
      (IntConstant 1,             _, Times) -> r
      -- непосредственно вычисление
      (IntConstant a, IntConstant b, Plus ) -> IntConstant $ a + b
      (IntConstant a, IntConstant b, Minus) -> IntConstant $ a - b
      (IntConstant a, IntConstant b, Times) -> IntConstant $ a * b
      -- не можем упростить или вычислить - возвращаем то, что упростилось ранее
      _ -> BinaryTerm l r op
  _ -> expression

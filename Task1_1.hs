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
    in case (l, r) of
      -- для упрощения выражения, если не можем посчитать
      (            _, IntConstant 0) | op /= Times -> l
      (            _, IntConstant 0) | op == Times -> IntConstant 0
      (            _, IntConstant 1) | op == Times -> l
      (IntConstant 0,             _) | op == Plus  -> r
      (IntConstant 0,             _) | op == Times -> IntConstant 0
      (IntConstant 1,             _) | op == Times -> r
      -- непосредственно вычисление
      (IntConstant a, IntConstant b) -> 
        case op of
          Plus  -> IntConstant $ a + b
          Minus -> IntConstant $ a - b
          Times -> IntConstant $ a * b
      -- не можем упростить или вычислить - возвращаем то, что упростилось ранее
      _ -> BinaryTerm l r op
  _ -> expression

module Task4_1 where

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

instance Functor FunMonad where
  -- fmap :: (a -> b) -> FunMonad a -> FunMonad b
  fmap f (FunMonad x) = FunMonad (\s -> f $ x s)

instance Applicative FunMonad where
  -- pure :: a -> FunMonad a
  pure x = return x

  -- (<*>) :: FunMonad (a -> b) -> FunMonad a -> FunMonad b
  -- где a = fun::String -> a, т.е. "по-человечески":
  -- [in ] f   = F (String, a): b, <- функция, принимающая строку и результат старой функции
  -- [in ] x   = fun (String): a,  <- старая ф-ция, принимавшая строку и возвращавшая A
  -- [out] res = fun (String): b   <- новая ф-ция, принимающая строку и возвращающая B
  (<*>) (FunMonad f) (FunMonad x) = FunMonad (\s -> f s $ x s)

instance Monad FunMonad where
  -- (>>=) :: FunMonad a -> (a -> FunMonad b) -> FunMonad b
  -- FM (String -> a) -> (a -> FM (String -> b)) -> FM (String -> b),
  -- при том что на самом деле: fun:: FunMonad a -> String -> a
  (>>=) (FunMonad x) f = FunMonad (\s -> fun (f $ x s) s)

  -- return :: a -> FunMonad a
  return x = FunMonad (\ _ -> x)

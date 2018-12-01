module Task4_2 where

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что 
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor FourOf where
  -- fmap :: (a -> b) -> Functor a -> Functor b
  fmap f (FourOf x1 x2 x3 x4) = FourOf (f x1) (f x2) (f x3) (f x4)

instance Applicative FourOf where
  pure = return

  -- (<*>) :: Functor (a -> b) -> Functor a -> Functor b
  (<*>) (FourOf f1 f2 f3 f4) (FourOf x1 x2 x3 x4) = FourOf (f1 x1) (f2 x2) (f3 x3) (f4 x4)


instance Monad FourOf where
  -- (>>=) :: Monad a -> (a -> Monad b) -> Monad b
  (>>=) (FourOf x1 x2 x3 x4) f = FourOf y1 y2 y3 y4
    where
      FourOf y1 _ _ _ = f x1
      FourOf _ y2 _ _ = f x2
      FourOf _ _ y3 _ = f x3
      FourOf _ _ _ y4 = f x4

  -- return :: a -> m a
  return x = FourOf x x x x

module Task3_1 where

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

instance Show WeirdPeanoNumber where
  show Zero     = "Zero"
  show (Succ x) = "Succ $ " ++ show x
  show (Pred x) = "Pred $ " ++ show x


instance Eq WeirdPeanoNumber where
  (==)  Zero     Zero    = True
  (==) (Succ a) (Succ b) = a == b
  (==) (Pred a) (Pred b) = a == b
  (==)  _        _       = False


evaluateAndCount x = evaluate (0, 0) $ compress x
  where
    evaluate (v, c)  Zero    =          (v    , c    )
    evaluate (v, c) (Succ n) = evaluate (v + 1, c + 1) n
    evaluate (v, c) (Pred n) = evaluate (v - 1, c + 1) n

compress x = compressAll Zero x
  where
    compressAll r Zero            = r

    compressAll (Succ r) (Pred x) = compressAll r x
    compressAll (Pred r) (Succ x) = compressAll r x

    compressAll r (Succ (Pred x)) = compressAll r x
    compressAll r (Pred (Succ x)) = compressAll r x

    compressAll r (Succ x)        = compressAll (Succ r) x
    compressAll r (Pred x)        = compressAll (Pred r) x

instance Ord WeirdPeanoNumber where
  -- проверка только по значению: Succ $ Pred $ Zero == Pred $ Succ $ Zero == Zero
  (<=) a b = checkLE (compress a) (compress b)
    where
      checkLE  Zero     Zero    = True
      checkLE (Pred a) (Succ b) = True
      checkLE (Succ a) (Pred b) = False

      checkLE (Succ a) (Succ b) = checkLE a b
      checkLE (Pred a) (Pred b) = checkLE a b

      checkLE (Pred a)  Zero    = True
      checkLE       a   Zero    = False
      checkLE  Zero    (Pred b) = False
      checkLE  Zero          b  = True


instance Num WeirdPeanoNumber where
  (+) Zero b = b
  (+) a Zero = a
  (+) (Succ a) (Pred b) = a + b
  (+) (Pred a) (Succ b) = a + b
  (+) a (Succ x) = Succ $ a + x
  (+) a (Pred x) = Pred $ a + x

  (*) Zero _ = Zero
  (*) _ Zero = Zero
  (*) a b = if n < Zero then (negate $ res $ negate n) else (res n)
    where
      n = compress b
      res  Zero       = Zero
      res (Succ Zero) = a
      res (Succ i   ) = a + (res i)

  abs Zero = Zero
  abs x = if x < Zero then (negate x) else x

  signum x = case (compress x) of
    Zero    -> Zero
    Succ _ -> Succ Zero
    Pred _ -> Pred Zero

  negate  Zero = Zero
  negate (Succ x) = Pred (negate x)
  negate (Pred x) = Succ (negate x)

  fromInteger x | x == 0 = Zero
                | x <  0 = Pred (fromInteger $ x + 1)
                | x >  0 = Succ (fromInteger $ x - 1)


instance Enum WeirdPeanoNumber where
  toEnum x = fromInteger $ toInteger x
  fromEnum x = fst $ evaluateAndCount x


instance Real WeirdPeanoNumber where
  toRational a = toRational (toInteger a)


instance Integral WeirdPeanoNumber where
  toInteger x = toInteger $ fst $ evaluateAndCount x

  quotRem _ Zero = error "Division by zero!"
  quotRem Zero _ = (Zero    , Zero)
  quotRem a b = case (signum ca, signum cb) of
    (Succ _, Succ _) -> (q, r)
    (Succ _, Pred _) -> (negate q, r)
    (Pred _, Pred _) -> (q, negate r)
    (Pred _, Succ _) -> (negate q, negate r)
    where
      ca = compress a
      cb = compress b
      (q, r) = quotRemEx (abs ca) (abs cb) Zero
      quotRemEx x (Succ Zero) _ = (       x, Zero)
      quotRemEx x (Pred Zero) _ = (negate x, Zero)

      quotRemEx x y i | x < y = (i, x)
      quotRemEx x y i = quotRemEx (x - y) y $ Succ i

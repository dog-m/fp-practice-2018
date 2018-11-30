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


evaluateAndCount x = evaluate (0, 0) x
  where
    evaluate (v, c)  Zero    =          (v    , c    )
    evaluate (v, c) (Succ n) = evaluate (v + 1, c + 1) n
    evaluate (v, c) (Pred n) = evaluate (v - 1, c + 1) n

instance Ord WeirdPeanoNumber where
  (<=) a b = (valueA <= valueB) && (lenA <= lenB)
    where
      (valueA, lenA) = evaluateAndCount a
      (valueB, lenB) = evaluateAndCount b


instance Num WeirdPeanoNumber where
  (+) Zero b = b
  (+) a Zero = a
  (+) a b    = fromInteger $ (toInteger a) + (toInteger b)

  (*) Zero _ = Zero
  (*) _ Zero = Zero
  (*) a b = fromInteger $ (toInteger a) * (toInteger b)

  abs Zero = Zero
  abs x = fromInteger $ abs $ toInteger x

  signum x = case signum $ toInteger x of
    ( 0) -> Zero
    ( 1) -> Succ Zero
    (-1) -> Pred Zero

  negate Zero = Zero
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

  quotRem a b  = (fromInteger div, fromInteger mod)
    where
      (div, mod) = quotRem (toInteger a) (toInteger b)

module Task3_3 where

import Data.Coerce

-- множество = некая функция принадлежности произвольного элемента к воображаемому множеству?
newtype PSet a = PSet{ contains :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

-- зачем объявляли PSet, если не пользуемся?

-- пересечение множеств
newtype PSetIntersect a = PSetIntersect{ containsI :: (a -> Bool) }
instance Semigroup (PSetIntersect a) where (<>) a b = mappend a b
instance Monoid (PSetIntersect a) where
  -- [нейтральное значение] пересечение с универсальным множеством = то же множество
  mempty = PSetIntersect (\ _ -> True)
  -- комбинация пересечений = новое персечение, при котором элемент принадлежит обоим
  mappend (PSetIntersect a) (PSetIntersect b) = PSetIntersect (\x -> (a x) && (b x))

-- объединение множеств
newtype PSetUnion a = PSetUnion{ containsU :: (a -> Bool) }
instance Semigroup (PSetUnion a) where (<>) a b = mappend a b
instance Monoid (PSetUnion a) where
  -- объединение с пустым множеством = то же множество
  mempty = PSetUnion (\ _ -> False)
  -- комбинация объединений = новое объединение, при котором элемент принадлежит хотя бы одному из них
  mappend (PSetUnion a) (PSetUnion b) = PSetUnion (\x -> (a x) || (b x))

-- разность множеств не может быть реализована, т.к. имеет значение порядок вычисления
-- mappend может выглядеть как-то так: PSetSubAB (\x -> (a x) && !(b x))
newtype PSetSubAB a = PSetSubAB{ containsS :: (a -> Bool) }

newtype PSetXor a = PSetXor{ containsX :: (a -> Bool) }
instance Semigroup (PSetXor a) where (<>) a b = mappend a b
instance Monoid (PSetXor a) where
  -- xor с пустым множеством = то же самое множество
  mempty = PSetXor (\ _ -> False)
  -- комбинация = элемент принадлежит только одному множеству
  mappend (PSetXor a) (PSetXor b) = PSetXor (\x -> (a x) /= (b x) )


-- применить функцию к чему? (элементы отсутствуют)
-- если функция = трансформация начального элемента в другой элемент (изменение значения),
-- то применяем трансформацию, а затем проверяем принадлежность
-- instance Functor PSet where
--  fmap f (PSet c) = PSet (\x -> c $ f x)
-- но, не зная самой функции contains мы должны её как-то "изменить", чтобы она принимала тип b вместо a
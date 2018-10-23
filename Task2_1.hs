module Task2_1 where

import Todo(todo)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = Node Integer v (TreeMap v) (TreeMap v)
               | Leaf
     deriving(Show)

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = Leaf

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains t k = conts t k
  where
    conts Leaf _ = False
    conts (Node key _ left right) k
      | key == k = True
      | k < key  = conts left  k
      | k > key  = conts right k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup k t = look k t
  where
    look k (Node key value left right)
      | key == k = value
      | k < key  = look left  k
      | k > key  = look right k
    look _ Leaf  = error "Key dont exist"

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) t = ins k v t
  where
    ins k v (Node key value left right)
      | key == k = Node key v left right -- коллизия, перезаписываем
      | k < key  = Node key value (ins k v left) right
      | k > key  = Node key value left (ins k v right)
    ins k v Leaf = Node k v Leaf Leaf

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove i t = rm i t
  where
    rm _ Leaf = error "Nothing to remove"
    rm k (Node key value left right)
      | k < key = rm k 

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i t = todo

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst =
  foldl insert emptyTree lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree t =
  case t of
    Leaf                = []
    Node k v Leaf Leaf  = [(k, v)]
    Node k v left right = (listFromTree left) ++ [(k, v)] ++ (listFromTree right)

-- Поиск k-той порядковой статистики дерева 
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = todo

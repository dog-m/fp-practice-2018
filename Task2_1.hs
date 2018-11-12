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
contains Leaf _ = False
contains (Node key _ left right) k
  | key == k = True
  | k < key  = contains left  k
  | k > key  = contains right k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup _ Leaf  = error "Key dont exist"
lookup k (Node key value left right)
  | key == k = value
  | k < key  = lookup left  k
  | k > key  = lookup right k

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) Leaf = Node k v Leaf Leaf
insert (k, v) (Node key value left right)
  | key == k = Node key v left right -- коллизия, перезаписываем
  | k < key  = Node key value (insert (k, v) left) right
  | k > key  = Node key value left (insert (k, v) right)

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove _ Leaf = error "Nothing to remove"
remove k (Node key value left right)
  | k < key  = Node key value (remove k left) right
  | k > key  = Node key value left (remove k right)
  | k == key =
    case (left, right) of
      (Leaf, Leaf) -> Leaf
      (_   , Leaf) -> left
      (Leaf, _   ) -> right
      (_   , _   ) -> insertLeft left right
        where
          insertLeft subtree (Node k v l r) = case l of
            Leaf         -> Node k v subtree r
            Node _ _ _ _ -> Node k v (insertLeft subtree l) r

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

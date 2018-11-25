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
  | k < key  = lookup k left
  | k > key  = lookup k right

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) Leaf = Node k v Leaf Leaf
insert p@(k, v) (Node key value left right)
  | key == k = Node key v left right -- коллизия, перезаписываем
  | k < key  = Node key value (insert p left) right
  | k > key  = Node key value left (insert p right)

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
nearestLE _ Leaf = error "Empty tree"
nearestLE i tree@(Node key value left right)
  | i == key  = (key, value)
  | otherwise =
    if distance_to left_value < distance_to right_value
    then (left_key,  left_value)
    else (right_key, right_value)
  where
    (Node left_key  left_value  _ _) = get_min_from left  tree
    (Node right_key right_value _ _) = get_min_from right tree
    distance_to x = if x < i then (i - x) else (x - i)
    get_min_from Leaf               alt = alt
    get_min_from sub@(Node k _ l r) alt
      | k == key = sub
      | k < key  = get_min_from l $ get_alternative sub alt
      | k > key  = get_min_from r $ get_alternative sub alt
    get_alternative subtree@(Node _ subtree_v _ _) alternative@(Node _ alternative_v _ _) = 
      if distance_to subtree_v < distance_to alternative_v
        then subtree
        else alternative

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst =
  foldl insert emptyTree lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree Leaf                = []
listFromTree Node k v Leaf Leaf  = [(k, v)]
listFromTree Node k v left right = (listFromTree left) ++ [(k, v)] ++ (listFromTree right)

-- Поиск k-той порядковой статистики дерева 
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean _ Leaf = error "Empty tree"
-- не очень эффективный вариант: kMean k t = last $ take k $ listFromTree t
kMean k t = todo

module Task6 where

{-
  необходимо реализовать тип данных, представляющий собой бинарное дерево поиска
  (по мотивам соответствующей структуры данных из лекций), но такое, в котором в каждой ячейке
  содержится ссылка на родительскую ячейку (подобно тому, как в типе DList содержится ссылка
  на предыдущий элемент).
-}

import Todo(todo)

data LinkedTree a = ChangeMe

find :: LinkedTree a -> a -> Bool
find = todo

insert :: LinkedTree a -> a -> LinkedTree a
insert = todo

remove :: LinkedTree a -> a -> LinkedTree a
remove = todo

module Task6 where

{-
  необходимо реализовать тип данных, представл€ющий собой бинарное дерево поиска
  (по мотивам соответствующей структуры данных из лекций), но такое, в котором в каждой €чейке
  содержитс€ ссылка на родительскую €чейку (подобно тому, как в типе DList содержитс€ ссылка
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

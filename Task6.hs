module Task6 where

{-
  ���������� ����������� ��� ������, �������������� ����� �������� ������ ������
  (�� ������� ��������������� ��������� ������ �� ������), �� �����, � ������� � ������ ������
  ���������� ������ �� ������������ ������ (������� ����, ��� � ���� DList ���������� ������
  �� ���������� �������).
-}

import Todo(todo)

data LinkedTree a = ChangeMe

find :: LinkedTree a -> a -> Bool
find = todo

insert :: LinkedTree a -> a -> LinkedTree a
insert = todo

remove :: LinkedTree a -> a -> LinkedTree a
remove = todo

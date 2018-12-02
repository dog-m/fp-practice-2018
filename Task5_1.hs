module Task5_1 where

import Todo(todo)

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
    let rec = DCons left h (list2dlist' rec t)
    in rec

index :: DList a -> Int -> a
-- index list i
index  DNil         _ = error "Empty list or wrong index"
index (DCons _ c _) 0 = c
index (DCons l _ _) i | i < 0 = index l $ i + 1
index (DCons _ _ r) i | i > 0 = index r $ i - 1

goLeft      DNil = error
goLeft lst@(DCons DNil _ _) = lst
goLeft      DCons l    _ _  = goLeft l

insertAt :: DList a -> Int -> a -> DList a
-- insertAt list index value
insertAt  DNil         _ _         = error "Empty list or wrong index"
insertAt (DCons l _ _) i v | i < 0 = insertAt l (i + 1) v
insertAt (DCons _ _ r) i v | i > 0 = insertAt r (i - 1) v
insertAt (DCons l c r) 0 v = goLeft DCons left value right
  where
    left  = todo
    right = todo

removeAt :: DList a -> Int -> DList a
-- removeAt list index
removeAt  DNil               _     = error "Empty list"
removeAt (DCons DNil _ r   ) 0     = r
removeAt (DCons l    _ DNil) 0     = l
removeAt (DCons l    _ r   ) index = goLeft todo

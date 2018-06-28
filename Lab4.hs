{-
- Name: Conor John Quin.
- Number: 114400402.
- Assignment: 04.
-}


--algebraic data type called List which behaves very similarily list
data List a = Empty|Occupied a (List a)
                deriving(Eq,Show)

--returns the first element in a List
head' :: List a -> a
head' (Occupied h t) = h
head' Empty    = error "empty list"

--returns the tail of a List
tail'::List a -> List a
tail' (Occupied h t) = t
tail' Empty    = error "empty list"

--this function is the equavalent to foldr on a list
--it takes in a function values and a List and applies
--the function to  every values in the List once at a time
--going from right to left
foldr' :: (a->b->b)->b->List a ->b
foldr' f v Empty = v
foldr' f v vs= f (head' vs) (foldr' f v (tail' vs))

--this function is the equavalent to foldl on a list
--it takes in a function values and a List and applies
--the function to  every values in the List once at a time
--going from left to right
foldl' :: (a->b->a)->a->List b -> a
foldl' f v Empty = v
foldl' f v vs = foldl' f (f v (head' vs)) (tail' vs)

--takes in a List and returns a list version of that List
--eg (Occupied 9 (Occupied 5 Empty)) converts to [9,5]
lister::Show a => List a -> [a]
lister  = foldr' appender []

--takes in a value and a list and appends the value to
--the list
appender:: a -> [a] -> [a]
appender v lst = [v] ++ lst

--takes in a List(l) and prints it out in the form of
--a list
pretty_print :: Show a => List a -> String
pretty_print l = "" ++ show(lister(l))

--takes in a list and creates a List equivaent
----eg [9,5] converts to (Occupied 9 (Occupied 5 Empty))
create_from_list :: Ord a =>[a]->List a
create_from_list = foldr insert Empty

--takes  in a value (v) and List (l) and inserts v in to the List
--in an ordered way (smallest values at front of List)
--eg inserting  (using insert function) 4 in to 
--(Occupied 10 (Occupied 5 (Occupied 3 Empty)) would return
--(Occupied 10 (Occupied 5 (Occupied 4 (Occupied 3 Empty)))
insert::Ord a => a-> List a->List a
insert v Empty = Occupied v Empty
insert v l  | v<(head' l) = Occupied v (Occupied (head' l) (tail' l))
            | otherwise = Occupied (head' l) (insert v (tail' l))



main = putStrLn $ pretty_print $ create_from_list list 
       where list = [1,2,3,5,4]::[Int]
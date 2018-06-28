{-
- Name: Conor John Quin.
- Number: 114400402.
- Assignment: 05.
-}


--PART 2.1

--data type TwoValued has two values, One and Two
--the data type is of the Eq Show and Ord classes
data TwoValued  = One |Two  
    deriving(Eq,Show,Ord)

--The TwoValued data type is made an instance of the Bounded class
--The methods minBound and maxBound are overwritten to min of One and
--max of Two
instance Bounded TwoValued where
    minBound = One
    maxBound = Two

--PART 2.2

--The class Incrementable is created. It has one method next which
--returns the same data type it recieves. This next method will be
--used to get the next highest value of a data type. Eg for an int
--next 1 will return 2
class Ord a => Incrementable a where
    next :: a -> a

--The TwoValued data type is made an instance of the Incrementable class
--the next method is overwritten so that the next highest output of One is Two
--ie when next One is called Two will be returned 
instance Incrementable TwoValued where
    next One = Two


--PART 2.3

--The class Testable is created. It works with values of type Ord
--and Incrementable

--the method check takes in a Boolean expression p and two values 'start'
--and 'end'. These start and end values will determine the range of values 
--which will be checked against the boolean expression 'p'. The method 
--check checks if all the values in this range satisfy the boolean expression. 
class (Ord a, Incrementable a)  => Testable a where
    check :: (Ord a, Incrementable a) => (a->Bool)->a->a->Bool

    check p start end   | (start > end)==True = True
                        | p(start) == False = False
                        | start == end = p(start)
                        | otherwise = check p (next start) end

--TwoValued is made an instance of the Testable class so it can use the check method
instance Testable TwoValued



--part 2.4

--[a] is made an instance of the Testable class and a is are of the Incrementable
--Eq Ord and Bounded classes
instance (Incrementable a, Eq a,Ord a, Bounded a) => Testable [a] 

--[a] is made an instance of the Incrementable class. The method next 
--is overwritten.
--next takes in a list reverses the list, increments the 
--first element in this reversed list that is not the max of that data type and
--reverses the list back
--Eg next (One,One,Two) this would then be reversed to (Two,One,One) then
--the second element in the list is incremented because the first element(Two)
--is at its max (Two,Two,One) this is then reversed back to One,Two,Two
--next' does this incrementing the first non max element
instance (Incrementable a , Eq a , Ord a , Bounded a ) => 
    Incrementable [a] where
    next = reverse.next'.reverse where
        next' (a:as) | a == maxBound = a : (next' as)
                     | otherwise = (next a) : as

--part 2.5
--main method
main = do putStrLn $ show $ test ([One,Two,Two] == )
          putStrLn $ show $ test ([One,Two,Two] /=)
          putStrLn $ show $ test ([One,Two,Two] <)
          putStrLn $ show $ test ([Two,Two,Two] >=)
          putStrLn $ show $ test (>= [Two,Two,Two])
       where test p = [int p bs [Two,Two,Two] | bs <- bss]
             int p a a' = if check p a a' then 1 else 0
             bs = [One,Two]
             bss = [[a,b,c] | a <- bs , b <- bs , c <- bs]
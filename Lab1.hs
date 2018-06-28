{-
- Name: Conor John Quin.
- Number: 114400402.
- Assignment:1
-}

module Main where

main ::IO()
main = do
    putStrLn(show(knight1 False)) 
    putStrLn(show(knave1 True)) 
    putStrLn(show(knight2 False)) 
    putStrLn(show(knave2 True)) 
    putStrLn(show(knave3 False)) 
    putStrLn(show(getInhabitant 2 True)) 
    putStrLn(show(ask 2 False)) 
    putStrLn(show(double_negation True)) 
    putStrLn(show(interrogate1 knight1 False)) 
    putStrLn(show(interrogate2 knave2 True)) 
    putStrLn(show(interrogate3 knave3 False)) 





--This function takes in a Boolean argument and returns
--the argument which was fed in to the function
knight1 :: Bool -> Bool
knight1 a = a

--This function takes in a Boolean argument and returns
--the negated version of the argument
knave1 :: Bool -> Bool
knave1 a = not a

--This function takes in a Boolean argument and uses pattern matching
--to return the argument which was fed in to the function
knight2 :: Bool -> Bool
knight2 True = True
knight2 _ = False

--This function takes in a Boolean argument and uses a lambda-expression
--to return the negated version of the argument
knave2 ::Bool -> Bool
knave2 = \a -> not a

--This function takes in a Boolean argument uses a partial application
--to return the negated version of the argument
knave3::Bool -> Bool
knave3 = not

--This function returns a list consisting of the 
--functions knight1, knight2, knave1, knave2, and knave3.
inhabitants :: [Bool -> Bool]
inhabitants = [knight1, knight2, knave1, knave2, knave3]

-- This function takes in an integer(i) and returns the ith member from the list inhabitants.
getInhabitant :: Int -> Bool -> Bool
getInhabitant i = inhabitants!!i

--This function takes in an integer(i) and a Boolean(b), and returns the value that is returned 
--if you call the ith member of the list inhabitants and pass it the argument b.
ask :: Int -> Bool -> Bool
ask i b = (getInhabitant i) b

--This function uses function composition to call the function knave1 on itself
--The function returns the argument that is fed in to it
double_negation :: Bool -> Bool
double_negation = knave1.knave1

--Is the function double_negation equivalent to knave1?
--It is not equivalent as knave1 is called twice meaning that double_negation will return the Boolean
--value fed in to the function. Knave1 returns the negated value of the Boolean fed in

--This is a function that takes in the name of an inhabitant function and a Boolean and
--returns the Boolean value given by calling the inhabitant function on the Boolean twice
interrogate1 :: (Bool->Bool) -> Bool->Bool 
interrogate1 f x= f(f x)

--This is a function that takes in the name of an inhabitant function and a Boolean and
--uses a lambda-expression to return the Boolean value given by calling the inhabitant 
--function on the Boolean twice
interrogate2 :: (Bool->Bool) -> Bool->Bool
interrogate2 = \f -> (\x -> f(f x))

--This is a function that takes in the name of an inhabitant function and a Boolean and
--uses a partial application to return the Boolean value given by calling the inhabitant 
--function on the Boolean twice
interrogate3::(Bool->Bool)->Bool->Bool
interrogate3 = interrogate2
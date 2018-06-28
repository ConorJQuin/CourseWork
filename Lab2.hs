{-
- Name: Conor John Quin.
- Number: 114400402.
- Assignment: 02.
-}

--this function paints all the walls by calling the other functions
--base case 1 checks if there are no colours fed in to the function
--base case 2 checks if there are no wals
paint_interior_bricks :: Eq a => [a]  -> [[a]] -> [[a]]
paint_interior_bricks  [] walls = walls
paint_interior_bricks  _ [] = []
paint_interior_bricks colours (wall:walls) = painter colours wall : paint_interior_bricks i walls
                                        where i = last_used_paint colours wall


--this function maintains the order of the colours
--it works by moving the colour which has just been used to the back of the
--colours queue
last_used_paint :: Eq a => [a] -> [a]-> [a]
last_used_paint (colour:colours) (brick:bricks)   | ((bricks == [])||(len bricks ==1)) 
                                    && ([brick] /= [])  = (colour:colours)
                                | otherwise = last_used_paint (colours++[colour]) bricks

--this function ensures the first brick is not painted and then calls
--the paint_single_brick function
painter :: Eq a => [a] -> [a] -> [a]
painter colours (brick:bricks) | (bricks == []) && ([brick] /= []) = [brick]
painter colours (brick:bricks) = brick:paint_single_brick colours colours bricks

--this function paints a single wall
--it does this by taking in 2 lists of colours (one which remains unchanged)
--and a wall
--base case 1 checks if all the colours have been used but there are stil 
--bricks to paint
--base case 2 checks if there is only one brick in the wall left
paint_single_brick :: Eq a => [a] -> [a] -> [a] -> [a]
paint_single_brick original (colour:colours) (brick:bricks) | 
                        (colours == []) && (bricks /= []) = 
                         colour:paint_single_brick original original bricks
paint_single_brick original (colour:colours) (brick:bricks) | 
                  (bricks == []) && ([brick] /= []) = [brick]
            | otherwise = colour:paint_single_brick original colours bricks

--gets the length of a list a
len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs





main :: IO ()
main = putStrLn (show (paint_interior_bricks colours walls))
    where   colours = [0,1,2]
            walls = [[3],[3,3],[3,3,3],[3,3,3],[3,3,3,3,3],[3,3,3,3,3,3]]

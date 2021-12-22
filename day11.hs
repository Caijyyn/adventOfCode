
module Day11 where

import Matrix

------------------------------------------------------------------------------------------
-- Day 11: Dumbo Octopus
------------------------------------------------------------------------------------------
getTxtContent :: IO [String] 
getTxtContent = do 
    xs <- lines <$> readFile "day11inp.txt" 
    return xs

stringBreaker :: [String] -> [[Int]] 
stringBreaker strings = [map (read . pure :: Char -> Int) bs | bs <- strings]
------------------------------------------------------------------------------------------
-- Part one
------------------------------------------------------------------------------------------
partOne :: IO [[Int]]
partOne = do 
    strings <- getTxtContent 
    let mx = stringBreaker strings 
    return mx 





burstAdjacent :: (Point, Int) -> [[(Point, Int)]] -> [[(Point, Int)]]
burstAdjacent = undefined

adjacent :: (Point, Int) -> [[(Point, Int)]] -> [[(Point, Int)]]
adjacent = undefined

increase :: [[Int]] -> [[Int]] 
increase mx = increase [map (+1) xs | xs <- mx]

------------------------------------------------------------------------------------------
-- Part two
------------------------------------------------------------------------------------------
partTwo :: IO [[Int]]
partTwo = do 
    strings <- getTxtContent 
    let mx = stringBreaker strings 
    return mx 


------------------------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------------------------
tt = stringBreaker [
    "5483143223",
    "2745854711",
    "5264556173",
    "6141336146",
    "6357385478",
    "4167524645",
    "2176841721",
    "6882881134",
    "4846848554",
    "5283751526"]

burst = stringBreaker [
    "11111",
    "19991",
    "19191",
    "19991",
    "11111"]

t1 = assign2DCoordinates burst

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------


module Day7 where

import Data.List (sort, nub)
import Data.List.Split (splitOn)

------------------------------------------------------------------------------------------
-- Day 7: The Treachery of Whales
------------------------------------------------------------------------------------------
getTxtContent :: IO [Int]
getTxtContent = do 
    string <- readFile "day7inp.txt"
    let xs = sort $ map (read :: String -> Int) $ splitOn "," string
    return xs

------------------------------------------------------------------------------------------
-- Part one
------------------------------------------------------------------------------------------
partOne :: IO (Int, Int) 
partOne = do 
    xs <- getTxtContent 
    let crabsOccupyingTheSamePosition = frequencyInSortedList (nub xs) xs 0
    let costs = calculateFuelCost [0..1952] crabsOccupyingTheSamePosition
    let most = mostEffectiveAlignment costs
    return most

mostEffectiveAlignment :: [(Int,Int)] -> (Int,Int)
mostEffectiveAlignment fuelCosts = optCost
    where optCost = minimum fuelCosts

calculateFuelCost :: [Int] -> [(Int, Int)] -> [(Int,Int)]
calculateFuelCost  []     _     = [] 
calculateFuelCost (x:xs) crabs  = ( fuelCost , x ) : calculateFuelCost xs crabs
    where fuelCost = sum [b * (abs $ x - a) | (a,b) <- crabs]

frequencyInSortedList :: [Int] -> [Int] -> Int -> [(Int, Int)]
frequencyInSortedList [p] [x] n = (p,n+1) : []
frequencyInSortedList (p:ps) (x:xs) n
    | p == x    = frequencyInSortedList (p:ps) xs (n+1)
    | otherwise = (p,n) : frequencyInSortedList ps (x:xs) 0


------------------------------------------------------------------------------------------
-- Part two
------------------------------------------------------------------------------------------
partTwo :: IO (Int, Int) 
partTwo = do 
    xs <- getTxtContent 
    let crabsOccupyingTheSamePosition = frequencyInSortedList (nub xs) xs 0
    let costs = calculateFuelCost' [0..1952] crabsOccupyingTheSamePosition
    let most = mostEffectiveAlignment costs
    return most

calculateFuelCost' :: [Int] -> [(Int, Int)] -> [(Int,Int)]
calculateFuelCost'  []     _     = [] 
calculateFuelCost' (x:xs) crabs  = ( fuelCost , x ) : calculateFuelCost' xs crabs
    where fuelCost = sum [ arithmeticSum x tpl | tpl <- crabs]

arithmeticSum :: Int -> (Int,Int) -> Int 
arithmeticSum n (a,b) = theSum * b
    where 
        theSum = denom `div` 2
        denom = steps * (1 + steps)
        steps = abs (n-a)

------------------------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------------------------
tt = sort [16,1,2,0,4,2,7,1,2,14]

t1 = mostEffectiveAlignment $ calculateFuelCost [0 .. last tt] $ frequencyInSortedList (nub tt) tt 0
t2 = mostEffectiveAlignment $ calculateFuelCost' [0 .. last tt] $ frequencyInSortedList (nub tt) tt 0

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

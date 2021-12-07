
module Day6 where

import Data.List.Split (splitOn)
import Data.List (sort)

------------------------------------------------------------------------------------------
-- Day 6: Lanternfish
------------------------------------------------------------------------------------------
getTxtContent :: IO [Int]
getTxtContent = do 
    string <- readFile "day6inp.txt"
    let xs = map (read :: String -> Int) $ splitOn "," string
    return xs

------------------------------------------------------------------------------------------
-- Part one
------------------------------------------------------------------------------------------
partOne :: IO Int 
partOne = do 
    xs <- getTxtContent 
    let x = length $ spawnMoreFish 80 xs
    return x

spawnMoreFish :: Int -> [Int] -> [Int]
spawnMoreFish 0 fish = fish 
spawnMoreFish n fish 
    | 0 `elem` fish = spawnMoreFish (n-1) (newFish ++ xs)
    | otherwise     = spawnMoreFish (n-1) xs
    where 
        xs = [ if x == 0 then 6 else x - 1 | x <- fish] 
        oldFish = length $ filter (\x -> x == 0) fish 
        newFish = replicate oldFish 8

------------------------------------------------------------------------------------------
-- Part two
------------------------------------------------------------------------------------------
partTwo :: IO Int 
partTwo = do 
    fish <- getTxtContent 
    let xs = assignPopulationToLifeSpan fish
    let x = totalPopulationGrowth 256 xs
    return x

totalPopulationGrowth :: Int -> [(Int,Int)] -> Int 
totalPopulationGrowth n tpls = sum b
    where (a,b) = unzip $ calculatePopulationGrowthByAge n tpls

calculatePopulationGrowthByAge :: Int -> [(Int,Int)] -> [(Int,Int)]
calculatePopulationGrowthByAge n tpls 
    | n <= 1    = xs
    | otherwise = calculatePopulationGrowthByAge (n-1) xs
    where 
        x = snd $ head tpls 
        xs = sort [rotation x tpl | tpl <- tpls]

rotation :: Int -> (Int, Int) -> (Int, Int)
rotation x (a,b)    | a == 0 = (8,b) 
                    | a == 7 = (a-1,b+x)
                    | otherwise = (a-1,b)

assignPopulationToLifeSpan :: [Int] -> [(Int,Int)]
assignPopulationToLifeSpan fish = [0..8] `zip` populationsByDay
    where      populationsByDay = reverse $ map length $ chopByLifeSpan 8 fish

chopByLifeSpan :: Int -> [Int] -> [[Int]]
chopByLifeSpan 0 xs = filter (\x -> x == 0) xs : []
chopByLifeSpan n xs = filter (\x -> x == n) xs : chopByLifeSpan (n-1) xs

------------------------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------------------------
tt :: [Int]
tt = [3,4,3,1,2]
tt18 = totalPopulationGrowth 0 $ assignPopulationToLifeSpan 
    [6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8]

t182 = spawnMoreFish 18 tt

t2 = calculatePopulationGrowthByAge 9 $ assignPopulationToLifeSpan tt

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

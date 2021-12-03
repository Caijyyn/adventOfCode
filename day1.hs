
module Day1 where 

------------------------------------------------------------------------------------------
-- Day 1: Sonar Sweep
------------------------------------------------------------------------------------------

getTxtContent :: IO [Int]
getTxtContent = do
    xs <- map (read :: String -> Int) . words <$> readFile "day1inp.txt"
    return xs

------------------------------------------------------------------------------------------
-- Part one
------------------------------------------------------------------------------------------

partOne :: IO Int
partOne = do 
    depths <- getTxtContent 
    let times = timesIncreasing depths 
    return times


timesIncreasing :: [Int] -> Int
timesIncreasing xs = length $ dropDecreasing xs 
    where 
        dropDecreasing :: [Int] -> [Int]
        dropDecreasing [n]                          = [] 
        dropDecreasing (n:m:ns) | hasIncreased n m  = n : dropDecreasing (m:ns)
                                | otherwise         = dropDecreasing (m:ns)

        hasIncreased :: Int -> Int -> Bool 
        hasIncreased x y = x < y


------------------------------------------------------------------------------------------
-- Part two
------------------------------------------------------------------------------------------

partTwo :: IO Int 
partTwo = do 
    depths <- getTxtContent 
    let xs = sumDepths depths  
    let x = timesIncreasing xs 
    return x

sumDepths :: [Int] -> [Int]
sumDepths xs = addIt
    where 
        trpl  = trupleList xs 
        addIt = addTruple trpl

trupleList :: [Int] -> [(Int,Int,Int)]
trupleList (a:b:c:[]) = (a,b,c) : []
trupleList (a:b:c:xs) = (a,b,c) : trupleList (b:c:xs)

addTruple :: [(Int,Int,Int)] -> [Int]
addTruple truple = [a + b + c | (a,b,c) <- truple]

------------------------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------------------------

ts = [199,200,208,210,200,207,240,269,260,263]
t1 = timesIncreasing ts
t2 = timesIncreasing $ sumDepths ts

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

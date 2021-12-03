
module Day2 where


------------------------------------------------------------------------------------------
-- Day 2: Dive!
------------------------------------------------------------------------------------------

getTxtContent :: IO [(String, Int)]
getTxtContent = do
    xs <- map lines . words <$> readFile "day2inp.txt"
    let str = tupleCommands xs
    return str

tupleCommands :: [[String]] -> [(String, Int)]
tupleCommands []        = [] 
tupleCommands (a:b:str) = (s,x) : tupleCommands str
    where 
        s = head a
        x = read $ head b

------------------------------------------------------------------------------------------
-- Part one
------------------------------------------------------------------------------------------

partOne :: IO Int 
partOne = do 
    commands <- getTxtContent 
    let x = multiplyPositions commands 
    return x

multiplyPositions :: [(String, Int)] -> Int
multiplyPositions list = let (x,y) = unzip $ designation list in sum x * sum y

designation :: [(String, Int)] -> [(Int, Int)]
designation  []                             = []
designation ((a,b) : ls)    | a == "up"     = (0 , b * (-1) )   : designation ls
                            | a == "down"   = (0 , b)           : designation ls
                            | otherwise     = (b , 0)           : designation ls

------------------------------------------------------------------------------------------
-- Part two
------------------------------------------------------------------------------------------

partTwo :: IO Int 
partTwo = do 
    commands <- getTxtContent 
    let x = multiplyDepthWithHorisontal $ addAim (designation commands) 0   
    return x

multiplyDepthWithHorisontal :: [(Int, Int, Int)] -> Int
multiplyDepthWithHorisontal ts = let (a,b,c) = unzip3 ts in sum a * sum b

addAim :: [(Int,Int)] -> Int -> [(Int, Int, Int)]
addAim [] _ = [] 
addAim ((a,b) : ls) x   | a > 0     = (a, a * z, z) : addAim ls z
                        | otherwise = (0, 0, z)     : addAim ls z
    where z = x + b

------------------------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------------------------

tt = [("forward", 5), ("down", 5), ("forward", 8), ("up", 3), ("down", 8), ("forward", 2)]
t1 = multiplyPositions tt
t2 = multiplyDepthWithHorisontal $ addAim (designation tt) 0

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

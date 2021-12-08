
module Day5 where

import Data.List.Split (splitOn, chunksOf)
import Data.List (intersect, sort, nub, transpose)

type Vector = (Int, Int)
type Force  = (Vector, Vector)
type Matrix = [[Int]]
data Vectors = V (Int,Int) deriving (Eq, Ord, Show)

------------------------------------------------------------------------------------------
-- Day 5: Hydrothermal Venture
------------------------------------------------------------------------------------------
getTxtContent :: IO [Vector] 
getTxtContent = do
    strs <- map lines . words <$> readFile "day5inp.txt"
    let str = stringBreaker strs
    return str

stringBreaker :: [[String]] -> [Vector]
stringBreaker strings = map (\[x,y] -> (x,y)) xss
    where 
        xss = [map (read :: String -> Int) s | s <- st]
        st  = map (\x -> splitOn "," x) str
        str = filter (\x -> x /= "->") $ map head strings

------------------------------------------------------------------------------------------
-- Part one  -- FUNGERAR INTE, FYLLER UPP MINNE OCH KLARAR INTE AV ATT EXEKVERA GILTIG LÖSNING
------------------------------------------------------------------------------------------
partOne :: IO Int {- [Vectors] -}
partOne = do 
    tpls <- getTxtContent 
    let vectors = horisontalAndVertical tpls 
    let matrix = drawLines vectors $ buildMatrix 1000
    let x = countOverlapping matrix 
    return x

-- Dessa tre fungerar, men extremt långsamma ----------------
findIntersections :: [Vectors] -> [Vectors]
findIntersections [] = []
findIntersections (ve:ctors)    | ve `elem` ctors   = ve : findIntersections ctors
                                | otherwise         = findIntersections ctors

countOverlapping' :: [Vectors] -> Integer 
countOverlapping' [] = 0
countOverlapping' (v:ector) | v `elem` ector = 1 + countOverlapping' ector 
                            | otherwise      = countOverlapping' ector

countOverlapping :: Matrix -> Int
countOverlapping matrix = sum $ map length [filter (\x -> x > 1) rows | rows <- matrix]
--------------------------------------------------------------

drawLines :: [Vector] -> Matrix -> Matrix
drawLines []                matrix = matrix
drawLines ((xs,ys):vectors) matrix = drawLines vectors mmx
    where 
        mmx = [[e | (y,e) <- l] | (x,l) <- mx]
        mx = [if y == ys then (y,[if x == xs then (x,el + 1) else e 
                | e@(x,el) <- ls]) else l | l@(y, ls) <- numbered]
        numbered = id `zip` [ id `zip` list | list <- matrix]
        id = [0..]

horisontalAndVertical :: [Vector] -> [Vector]
horisontalAndVertical vector = sort $ concat $ pairIt list
    where 
        list = (xs `zip` ys)
        (xs, ys) = (createLines x, createLines y)
        (x, y) = unzip vector

        pairIt :: [([Int], [Int])] -> [[Vector]]
        pairIt [] = []
        pairIt ((vx,vy):ector) 
            | x == 1    = (replicate y $ head vx) `zip` vy : pairIt ector
            | y == 1    = vx `zip` (replicate x $ head vy) : pairIt ector
            | otherwise = pairIt ector
            where (x,y) = (length vx, length vy) 

createLines :: [Int] -> [[Int]]
createLines [] = [] 
createLines (x:y:xs) 
    | x > y     = [y..x] : createLines xs
    | x < y     = [x..y] : createLines xs
    | otherwise = [x] : createLines xs

buildMatrix :: Int -> Matrix
buildMatrix n = replicate n $ replicate n 0

plotMatrix :: Matrix -> IO ()
plotMatrix matrix = putStr $ concat $ map (\x -> x ++ "\n") str
    where 
        str = [[if s == '0' then '.' else s | s <- st, unClutter s] | st <- strings]
        strings = map show matrix

        unClutter :: Char -> Bool 
        unClutter c = (c /= ',') && (c /= '[') && (c /= ']')

------------------------------------------------------------------------------------------
-- Part two
------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------------------------
tt = [  ["0,9"], ["->"], ["5,9"],
        ["8,0"], ["->"], ["0,8"],
        ["9,4"], ["->"], ["3,4"],
        ["2,2"], ["->"], ["2,1"],
        ["7,0"], ["->"], ["7,4"],
        ["6,4"], ["->"], ["2,0"],
        ["0,9"], ["->"], ["2,9"],
        ["3,4"], ["->"], ["1,4"],
        ["0,0"], ["->"], ["8,8"],
        ["5,5"], ["->"], ["8,2"]]

t1 = findIntersections $ map (\v -> V v) $ horisontalAndVertical $ stringBreaker tt
t2 = plotMatrix $ drawLines (horisontalAndVertical $ stringBreaker tt) $ buildMatrix 10


------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

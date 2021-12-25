
module Day11 where

import Matrix

type Grid = [[(Point, Int)]]
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
partOne :: IO Int
partOne = do 
    strings <- getTxtContent 
    let mx = (assign2DCoordinates.stringBreaker) strings 
    let x  = fst $ chargeThem 100 (0,mx)
    return x 

chargeThem :: Int -> (Int, Grid) -> (Int, Grid)
chargeThem 0 tpl = tpl 
chargeThem n (x , grid) = chargeThem (n-1) (x + y , newgrid)
    where 
        y = countZeroes newgrid 
        newgrid = (reverse . iterateGrid) incGrid 
        incGrid = increase grid

iterateGrid :: Grid -> Grid 
iterateGrid grid = remodeled grid []
    where 
        remodeled :: Grid -> Grid -> Grid
        remodeled [] new = new
        remodeled (m:mx) new
            | snd l > 9 = iterateGrid newGrid 
            | otherwise = remodeled mx (m:new)
            where 
                newGrid = injectAdjacent adj grid
                adj     = burstAdjacent l $ adjacent l grid
                l       = if ls == [] then ((0,0),0) else head ls
                ls      = filter (\(p,v) -> v > 9) m


countZeroes :: Grid -> Int 
countZeroes = sum.map (length.filter (\(p,x) -> x == 0))

injectAdjacent :: Grid -> Grid -> Grid
injectAdjacent adj mx = p1 ++ putIn adj middle ++ p2
    where 
        middle = take lenA $ drop f1 mx
        spl@(p1, p2) = (take f1 mx, drop (f1 + lenA) mx)
        (f1,lenA) = ((fst.fst.head.head) adj ,  length adj)

        putIn :: Grid -> Grid -> Grid
        putIn (a:dj) (m:mx) = (p1 ++ a ++ p2) : putIn dj mx
            where 
                (p1, p2)  = (take f2 m, drop (f2 + lenA) m)
                (f2,lenA) = ((snd.fst.head) a , length a)
        putIn  _      _   = []

burstAdjacent :: (Point, Int) -> Grid -> Grid
burstAdjacent (point,val) adj = 
    [[if p == point then (p,0) else if v == 0 then (p,v) else (p, v+1) 
    | (p,v) <- row] | row <- adj]

adjacent :: (Point, Int) -> Grid -> Grid
adjacent ((row,col), _) mx = filter (\xs -> length xs > 0) extracted 
    where 
        extracted    = [[ c | c@(p,v) <- rows, p `elem` points ] | rows <- mx]
        points       = [(r,c) | r <- rows, c <- cols ]
        (rows, cols) = (getInterval row rLen , getInterval col cLen)
        (rLen, cLen) = get2DMatrixSize mx

        getInterval :: Int -> Int -> [Int]
        getInterval p max
            | (p-1) < 0   = [0 .. (p+1)] 
            | (p+1) > max = [(p-1) .. p]
            | otherwise   = [(p-1) .. (p+1)]

increase :: Grid -> Grid 
increase mx = [[ (p, v+1) | (p,v) <- line] | line <- mx]

------------------------------------------------------------------------------------------
-- Part two
------------------------------------------------------------------------------------------
partTwo :: IO Int
partTwo = do 
    strings <- getTxtContent 
    let mx = (assign2DCoordinates.stringBreaker) strings 
    let x  = fst $ simultaneousCharge (0,mx)
    return x 

simultaneousCharge :: (Int, Grid) -> (Int, Grid)
simultaneousCharge (x , grid)
    | allZero   = (x , grid)
    | otherwise = simultaneousCharge  (x + 1 , newgrid)
    where 
        allZero = all (==[]) [filter (\(p,v) -> v /= 0) line | line <- grid ]
        newgrid = (reverse . iterateGrid) incGrid 
        incGrid = increase grid


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

b1 = ((0,0),1)
b2 = ((1,1),9)
b3 = ((2,2),1)
b4 = ((4,4),1)

t1 = assign2DCoordinates burst
t2 = burstAdjacent b3 $ adjacent b3 t1
t3 = injectAdjacent t2 t1
t4 = iterateGrid t1
t5 = chargeThem 5 (0,t1)
t6 = iterateGrid $ increase t1
t7 = length t6
t8 = chargeThem 195 (0, assign2DCoordinates tt)
t9 = iterateGrid $ increase $ snd $ chargeThem 5 (0,t1)
t10 = simultaneousCharge (0, assign2DCoordinates tt)
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

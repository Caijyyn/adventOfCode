
module Matrix 
    ( Point
    , assign2DCoordinates 
    , get2Delem
    , remove2DPoint
    , get2DMatrixSize) where 


------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
type Point =  (Int, Int)

-- | assign2DCoordinates: Every element in the matrix get a tuple (row,col), 
-- | that shows its position
assign2DCoordinates :: [[a]] -> [[(Point, a)]]
assign2DCoordinates mx = remodeled (0,0) mx 
    where
        remodeled :: Point -> [[a]] -> [[(Point, a)]]
        remodeled _     []      = []
        remodeled (x,y) (m:mx)  = lines (x,y) m : remodeled (x+1, 0) mx

        lines :: Point -> [a] -> [(Point, a)] 
        lines _     []      = [] 
        lines (x,y) (l:ls)  = ((x,y), l) : lines (x, y+1) ls

-- | Removes the two-dimensional coordinate from the matrix.
remove2DPoint :: [[(Point, a)]] -> [[a]]
remove2DPoint = map (snd.unzip)

-- | Fetches an element from a 2D-matrix
get2Delem :: Point -> [[a]] -> a
get2Delem p@(row,col) matrix | sizeInvariant p matrix = (matrix !! row) !! col

-- | Returns the size of the matrix in a tuple. 
-- | WARNING: It needs to be symmetric for this function to work.
get2DMatrixSize :: [[a]] -> (Int , Int)
get2DMatrixSize mx = (length mx , (length.head) mx)

-- | Will make sure the arguments stays within the borders of any given matrix
sizeInvariant :: Point -> [[a]] -> Bool 
sizeInvariant (row,col) mx =
    (0 <= row && row < length mx) && (0 <= col && col < length (mx !! row)) 




------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------





------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

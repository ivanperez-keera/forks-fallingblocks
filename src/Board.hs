module Board where
{- Really only want to export

Board, newBoard, copyBlockToBoard, removeCompleteRows, isMoveValid
-}

import Helpers
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, isNothing)
import Shape

-- A board has a map of Position to Color
data Board = Bd { m :: M.Map Position Color, ncol:: Int, nrow:: Int}
           deriving (Show, Eq)


newBoard :: Int -> Int -> Board
newBoard x y = Bd M.empty x y

copyBlockToBoard :: Block -> Board -> Board
copyBlockToBoard block
    = copySquares (blockPositions block) (typeColor (blockType block))

--See about changing this to a foldl
copySquares :: [Position] -> Color -> Board -> Board
copySquares [] _ b = b
copySquares (p:ps) c b = copySquares ps c (insert p c b)

insert :: Position -> Color -> Board -> Board
insert p c (Bd m x y) = Bd (M.insert p c m) x y

--Returns board and number of lines cleared
removeCompleteRows :: Board -> (Board, Int)
removeCompleteRows board = foldl removeFullRow (board, 0) [0 .. (nrow board)]

removeFullRow :: (Board, Int) -> Int -> (Board, Int)
removeFullRow (b, ls) row = if rowIsFull b row
                            then removeFullRow (moveEverythingDown, ls + 1) row
                            else (b, ls)
    where moveEverythingDown = foldl (moveColumnDown row) b [0 .. (ncol b)]
          
moveColumnDown :: Int -> Board -> Int -> Board
moveColumnDown row board col 
    | row <= (nrow board) = moveColumnDown (row + 1) (moveSquare col board row) col
    | otherwise = board

moveSquare :: Int -> Board -> Int -> Board
moveSquare x b y = mdelete (x, y + 1) newboard
    where abvSquare = doLookup (x, y + 1) b :: Maybe Color
          newboard  = if isNothing abvSquare
                      then mdelete (x, y) b
                      else insert (x, y) (fromJust abvSquare) b
          

mdelete :: Position -> Board -> Board
mdelete p (Bd m x y) = Bd (M.delete p m) x y

rowIsFull :: Board -> Int -> Bool
rowIsFull b row = and (map test [0 .. (ncol b)])
    where test col = isJust $ doLookup (col, row) b

isMoveValid :: Block -> Board -> Bool
isMoveValid block board = and (map test blks)
    where blks = blockPositions block
          test pos@(x, y) = x >= 0 && x <= (ncol board) && isFree pos
          isFree pos = Nothing == doLookup pos board
          
checkBelow :: Block -> Board -> Bool
checkBelow block board = or (map (stopBelow board) (blockPositions block))

stopBelow :: Board -> Position -> Bool
stopBelow b (x, y) = y == 0 || Nothing /= doLookup (x, y - 1) b

doLookup :: Position -> Board -> Maybe Color
doLookup p@(x,y) (Bd b nc nr) = if x<0 || y <0 || x>nc
                               then error ("Tried to look up " ++ show x ++ "," ++ show y)
                               else M.lookup p b

--test :: M.Map (Int, Int) Color
--test = M.insert (0,0) Blue $ M.insert (1,0) Blue $ M.empty

typeColor :: ShapeType -> Color
typeColor O = Red
typeColor I = Blue
typeColor S = Yellow
typeColor Z = Green
typeColor L = Purple
typeColor J = Cyan
typeColor T = Orange

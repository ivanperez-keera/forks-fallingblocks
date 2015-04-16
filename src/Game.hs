module Game where
{-- put this back in
(Game,
             newGame,
             Color(..),
             addBlock,
             move,
             colorAt,
             nrows, ncols,
             activeBlock) where
--}
import Shape
import Board
import Helpers

import Control.Monad (mplus)
import Data.Maybe (fromJust, isJust, isNothing)

-- Score/Num lines/Level/isRunning
type GameInfo = (Int, Int, Int, Bool)

-- board, current piece, next piece, game info
data Game = Gm { board::Board,
                 activeBlock::(Maybe Block),
                 nextBlock::(Maybe Block),
                 gameInfo::GameInfo }


score (Gm _ _ _ (s, _, _, _)) = s
numLines (Gm _ _ _ (_, l, _, _)) = l
curLevel (Gm _ _ _ (_, _, lv, _)) = lv
stillRunning (Gm _ _  _ (_, _, _, r)) = r

newGame :: ShapeType -> ShapeType -> Game
newGame ct nt = Gm (newBoard ncols nrows) c n (0,0, 0, True)
    where c = Just $ makeBlock ct initPos
          n = Just $ makeBlock nt initPos

addBlock :: Game -> ShapeType -> Game
addBlock (Gm b cur next (s, n, lv, _)) t = Gm b blockToUse newNext (s, n, lv, keepGoing)
    where newBlock = makeBlock t initPos
          keepGoing = isMoveValid newBlock b
          (blockToUse, newNext) | keepGoing = (cur, Just newBlock)
                                | otherwise = (cur, Just newBlock)

move :: Game -> Move -> Game
move g@(Gm b Nothing _ _) move = g
move g@(Gm b _ Nothing _) move = g
move g@(Gm board (Just block) (Just nextBlock) (s, n, lv, r)) move 
     | not $ stillRunning g = g
     | otherwise = Gm board' newblock newNext ((computeScore lv s ls), n + ls, level, r)
     where potentialBlock = moveBlock block move
           isValid = isMoveValid potentialBlock board
           isFinalMove = (move == MoveDown) && checkBelow block board
           (newblock, newNext) | isFinalMove = (Just nextBlock, Nothing)
                               | not isValid = (Just block, Just nextBlock)
                               | otherwise = (Just potentialBlock, Just nextBlock)
           (board', ls)   | isFinalMove = 
                              removeCompleteRows $ copyBlockToBoard block board
                          | otherwise = (board, 0)
           level | isFinalMove = (n + ls) `div` 10
                 | otherwise = lv


computeScore :: Int -> Int -> Int -> Int
computeScore level oldScore numLines = oldScore + extra
    where extra | numLines == 1 = 40 * (level + 1)
                | numLines == 2 = 100 * (level + 1)
                | numLines == 3 = 300 * (level + 1)
                | numLines == 4 = 1200 * (level + 1)
                | otherwise = 0 


colorAt :: Game -> Position -> Maybe Color
colorAt (Gm board block _ _) p = boardC `mplus` (block >>= blockC)
    where 
      boardC :: Maybe Color
      boardC = doLookup p board
      blockC :: Block -> Maybe Color
      blockC c | elem p (blockPositions c) = Just . typeColor $ blockType c
               | otherwise = Nothing

instance Show Game where
    show g = unlines $ reverse[ makeRow g y| y <- [0 .. nrows]]

makeRow :: Game -> Int -> String
makeRow g y = [showCol (colorAt g (x,y)) | x <- [0..ncols]]

showCol :: Maybe Color -> Char
showCol Nothing = '.'
showCol (Just c) = head $ show c

initPos :: Position
initPos = (6, 20)

nrows :: Int
nrows = 20

ncols :: Int
ncols = 10



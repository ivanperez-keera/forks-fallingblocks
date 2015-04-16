module Shape (ShapeType(..), 
              Block,
              makeBlock,
              moveBlock,
              blockType,
              blockPositions,
              blockPos) where

import Helpers

-- | The various shape types
data ShapeType = O | I | S | Z | L | J | T
                 deriving (Eq, Show, Enum)

type ShapeConfig = [Position]

rotations :: ShapeType -> [ShapeConfig]
rotations O = [[(-1, -1), (0, -1), (-1, 0), (0,0)]]
rotations I = [[(-2, 0), (-1, 0), (0,0), (1,0)],
               [(0, -2), (0, -1), (0,0), (0, 1)]]
rotations S = [[(-1, -1), (0,-1), (0,0), (1,0)],
               [(1, -1), (1, 0), (0,0), (0,1)]]
rotations Z = [[(-1, 0), (0,0), (0,-1), (1, -1)],
               [(0, -1), (0,0), (1, 0), (1,1)]]
rotations L = [[(-1, -1), (-1, 0), (0,0), (1,0)],
               [(0,-1), (1, -1), (0,0), (0, 1)],
               [(-1, 0), (0,0), (1,0), (1,1)],
               [(0,-1), (0,0), (0,1), (-1,1)]]
rotations J = [[(-1,0), (0,0), (1,0), (1,-1)],
               [(0,-1), (0,0), (0,1), (1,1)],
               [(-1,0), (-1,1), (0,0), (1,0)],
               [(-1,-1), (0,-1), (0,0), (0,1)]]
rotations T = [[(-1,0), (0,0), (0,-1), (1,0)],
               [(0,-1), (0,0), (0,1), (1,0)],
               [(-1,0), (0,0), (1,0), (0,1)],
               [(-1,0), (0,0), (0,-1), (0,1)]]

-- | A falling block.  Knows its current shape, 
--   its current position, and the list of future shapes (rotations)
data Block = Blk ShapeType Position [ShapeConfig]

instance Show Block where
    show (Blk t p (c:cs)) = (show t) ++ " " ++ (show p) ++ " " ++ (show c)

-- | Given a ShapeType and an initial position, create a new block
makeBlock :: ShapeType -> Position -> Block
makeBlock t p = Blk t p rots
    where rots = cycle $ rotations t

-- | Move a block
moveBlock :: Block -> Move -> Block
moveBlock (Blk t p (c:cs)) Rotate = Blk t p cs
moveBlock (Blk t p cs) m = Blk t p' cs
    where p' | m == MoveLeft = addPos p (-1, 0)
             | m == MoveRight = addPos p (1, 0)
             | m == MoveDown = addPos p (0, -1)

addPos :: Position -> Position -> Position
addPos (x, y) (x', y') = (x + x', y + y')

blockType :: Block -> ShapeType
blockType (Blk s _ _) = s

-- | Given a block, get the positions it occupies
blockPositions :: Block -> [Position]
blockPositions (Blk _ p (c:cs)) = map (addPos p) c

blockPos :: Block -> [Position]
blockPos (Blk _ _ (c:cs)) = c
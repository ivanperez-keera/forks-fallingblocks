module Helpers where

-- | The ways a piece can move
data Move = MoveLeft | MoveRight | MoveDown | Rotate
            deriving (Eq, Show)


-- | Position: (col, row)
type Position = (Int, Int)

data Color = Red | Blue | Yellow | Green | Purple | Cyan | Orange
           deriving (Eq)
instance Show Color where
    show _ = "X"

import Board
import Helpers
import Shape

import Test.HUnit
import Data.Maybe (isNothing, isJust)

main :: IO Counts
main = runTestTT $ TestList [first, second, third, forth, fifth]

board1 :: Board
board1 = foldl (\b p -> insert p Red b) (newBoard 2 2) [(0,0), (1,0), (2,0), (1,1)]

first :: Test
first = TestCase $ assertBool "should remove" ((isNothing $ doLookup (0,0) b')
                                               && (isJust $ doLookup (1,0) b'))
    where b' = removeCompleteRows board1

second :: Test
second = TestCase $ assertBool "row is full" (rowIsFull board1 0)

third :: Test
third = TestCase $ assertBool "should move" (isNothing $ doLookup (1, 1) b')
    where b = insert (1,1) Red (newBoard 2 2)
          b' = moveSquare 1 b 1

forth :: Test
forth = TestCase $ assertBool "should move" (isJust $ doLookup (1, 0) b')
    where b = insert (1,1) Red (newBoard 2 2)
          b' = moveSquare 1 b 0
          
board2 :: Board
board2 = foldl (\b p -> insert p Red b) (newBoard 2 2) 
         [(0,0), (1,0), (2,0), (0,1), (1,1), (2,1), (2,2)]

fifth :: Test
fifth = TestCase $ assertEqual "should be same" exp b
    where b = removeCompleteRows board2
          exp = insert (2,0) Red (newBoard 2 2)


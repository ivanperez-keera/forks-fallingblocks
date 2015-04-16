import Shape
import Helpers

import Test.HUnit
import List (sort)
import Monad (unless)

main :: IO Counts
main = runTestTT $ TestList testCases 

movecases = [((makeBlock T initPos), [(20,20), (19, 20), (21, 20), (20, 19)])]

testCases :: [Test]
testCases = map t movecases
    where t (block, ps) = 
              TestCase (assertRightPos ("expected " ++ show ps ++ " got " ++ show block) ps block)

initPos = (20, 20)
-- Helper functions
doMoves :: ShapeType -> [Move] -> Block
doMoves t moves = doMHelper (makeBlock t initPos) moves
    where doMHelper block [] = block
          doMHelper block (p:ps) = doMHelper (moveBlock block p) ps

assertRightPos :: String -> [Position] -> Block -> Assertion
assertRightPos msg exp b = assertSamePos msg exp (blockPositions b)

assertSamePos :: String -> [Position] -> [Position] -> Assertion
assertSamePos msg exp act = unless (samePos exp act) (assertFailure msg)
 
samePos :: [Position] -> [Position] -> Bool
samePos exp act = (sort exp) == (sort act)
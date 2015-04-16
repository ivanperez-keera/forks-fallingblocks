import Game
import Shape
import Helpers

import Test.HUnit

main :: IO Counts
main = runTestTT $ TestList testCases

assertList :: [([Position], Game)]
assertList = [([(5,20), (6,20), (7,20), (6,19)], (addBlock newGame T)),
              ([(5,19), (6,19), (7,19), (6,18)], move (addBlock newGame T) MoveDown)]

testCases :: [Test]
testCases = map t assertList
    where t (poslist, game) = 
              TestCase $ assertBool (show poslist) (hasColorsHere poslist game)      

hasColorsHere :: [Position] -> Game -> Bool
hasColorsHere ps g = and (map test ps)
    where test p = Nothing /= colorAt g p
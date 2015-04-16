import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

import Graphics.UI.SDL.Mixer as Mix

import Game
import Shape
import Helpers
import Board 

import GameController
import SplashController

import GameState

import System.Random (randomRIO)
import Data.Maybe (isNothing)

import GraphicsInfo
import Paths_fallingblocks

main = do
  SDL.init [InitEverything]
  setVideoMode 400 400 32 []
  TTF.init

  setCaption "Falling Blocks!" "Falling Blocks!" 

  mworked <- openAudio 44100 AudioS16Sys 2 4096
  music <- loadMUS =<< getDataFileName "data/music.mp3"
  playMusic music (-1)

  gi <- makeGraphics

  enableKeyRepeat 500 30

  c <- getRandomType
  n <- getRandomType
  gameLoop (GameState gi (newGame c n) 0 True music handleSplashEvents renderSplashScreen True)


gameLoop :: GameState -> IO ()
gameLoop gsfirst = do
  gs <- addPiece gsfirst

  --getEvents - would like to get all events from pollEvent
  events <- getEvents pollEvent []

  --handle input
  let gs' = handleEvents events gs

  --delay - probably want something a bit more
  --        complex here to handle refresh rate
  delay 10

  doRender gs'

  if (keepGoing gs')
    then gameLoop $ tickAndMoveIfNeeded gs'
    else cleanUp gs'

cleanUp :: GameState -> IO ()
cleanUp gs = do
  cleanupGraphics $ grInfo gs
  freeMusic $ mus gs
  closeAudio
  TTF.quit
  SDL.quit



getRandomType :: IO ShapeType 
getRandomType = do
  randomRIO (0,6) >>= \i ->
      return (toEnum i::ShapeType)

addPiece :: GameState -> IO GameState
addPiece gsfirst =
  if isNothing . nextBlock $ game gsfirst
   then randomRIO (0,6) >>= \i -> 
       return $ gsfirst {game = addBlock (game gsfirst) (toEnum i::ShapeType)}
   else return gsfirst

tickAndMoveIfNeeded :: GameState -> GameState
tickAndMoveIfNeeded gs = if ticks gs > t && (stillRunning $ game gs) && (not $ paused gs)
                         then gs { game = move (game gs) MoveDown,
                                   ticks = 0 }
                         else gs { ticks = (ticks gs) + 1 }
    where t = 40 - floor (modifier)
          modifier = 2.3 * (fromIntegral level) :: Float
          level = curLevel $ game gs


-- collects a list of all outstanding events from SDL
getEvents :: IO Event -> [Event] -> IO [Event]
getEvents pEvent es = do
  e <- pEvent
  let hasEvent = e /= NoEvent
  if hasEvent
   then getEvents pEvent (e:es)
   else return (reverse es)



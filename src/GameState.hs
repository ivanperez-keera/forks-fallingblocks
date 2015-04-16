module GameState where

import Game
import GraphicsInfo
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Mixer as Mix

data GameState = GameState { 
      grInfo :: GraphicsInfo,
      game   :: Game,
      ticks   :: Int,
      keepGoing :: Bool,
      mus :: Music,
      curController :: ([Event] -> GameState -> GameState),
      curRenderer :: (GameState -> IO ()),
      paused :: Bool
    }

doRender :: GameState -> IO ()
doRender gs = (curRenderer gs) gs

handleEvents :: [Event] -> GameState -> GameState
handleEvents es gs = (curController gs) es gs
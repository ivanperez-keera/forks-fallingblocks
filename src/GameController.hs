module GameController where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

import Board
import Game
import Shape
import Helpers
import GameState
import GraphicsInfo

-- Takes a list of events and a game state and returns a modified gamestate
handleGameEvents :: [Event] -> GameState -> GameState
handleGameEvents [] gs = gs
handleGameEvents ((KeyUp (Keysym SDLK_q _ _)):es) gs = gs { keepGoing = False}
handleGameEvents ((KeyUp (Keysym SDLK_p _ _)):es) gs 
    | paused gs = gs { paused = False }
    | otherwise = gs { paused = True }
handleGameEvents (e:es) gs = handleGameEvents es (gs' themove)
    where gs' Nothing = gs
          gs' (Just m) = gs { game = move (game gs) m }
          themove | paused gs = Nothing
                  | otherwise = 
                      case e of 
                        (KeyDown (Keysym SDLK_LEFT _ _))  -> Just MoveLeft
                        (KeyDown (Keysym SDLK_RIGHT _ _)) -> Just MoveRight
                        (KeyDown (Keysym SDLK_UP _ _)) -> Just Rotate
                        (KeyDown (Keysym SDLK_DOWN _ _)) -> Just MoveDown
                        otherwise  -> Nothing
         

-- Renders the current game state
renderGameState:: GameState -> IO ()
renderGameState gs = do
  let gri = grInfo gs
      g = game gs
      s = screen gri
      fnt = font gri

  -- Clear the screen
  worked <- fillRect s
            Nothing
            (Pixel 0)

  title <- renderTextSolid (titlefont gri) "FALLING BLOCKS!" (Color 255 0 0)
  blitSurface title Nothing s (Just (Rect 10 10 200 0))
  
--  scoreMessage <- renderTextSolid fnt ("Score:" ++ (show (score g))) (Color 255 0 0)
--  blitSurface scoreMessage Nothing s (Just (Rect 220 45 200 0))

  screenText "Score" fnt s 220 45
  screenText (show $ score g) fnt s 220 70

  screenText "Lines" fnt s 220 100
  screenText (show $ numLines g) fnt s 220 125

  screenText "Level" fnt s 220 155
  screenText (show $ curLevel g) fnt s 220 190
  
  screenText "Next" fnt s 220 220
  
  sequence_ [drawBlock x (nrows - y) 20 60 (colorAt g (x, y)) gri s
             | x <- [0..ncols], y <- [0..nrows]]

  drawBlockRect (bkgdblock gri) s

  drawNextBlock (nextBlock g) g gri

  if paused gs && (stillRunning $ game gs)
   then do 
     gameOver <- renderTextSolid (titlefont gri) "Paused" (Color 255 0 0)
     blitSurface gameOver Nothing s (Just (Rect 50 200 200 0))
   else return False

  if not . stillRunning $ game gs
   then do 
     gameOver <- renderTextSolid (titlefont gri) "Game Over!" (Color 255 0 0)
     blitSurface gameOver Nothing s (Just (Rect 50 200 200 0))
   else return False

  SDL.flip s

screenText :: String -> Font -> Surface -> Int -> Int -> IO Bool
screenText msg fnt s x y = do  
  screenMessage <- renderTextSolid fnt msg (Color 255 0 0)
  blitSurface screenMessage Nothing s (Just (Rect x y 200 0))


drawNextBlock :: (Maybe Block) -> Game -> GraphicsInfo -> IO ()
drawNextBlock Nothing _ _ = return ()
drawNextBlock (Just b) g gri = sequence_ [drawBlock (-1 * x)
 y 250 260 
                                        (Just . typeColor $ blockType b) gri s 
                                            | (x, y) <- blockPos b]
    where s = screen gri


--Change to take in coordinates
drawBlockRect :: Surface -> Surface -> IO ()
drawBlockRect block screen = do
  sequence_ [db2 x y block screen | x <- [0, ncols + 2],  y <- [0..nrows + 2]]
  sequence_ [db2 x y block screen | x <- [0 .. ncols + 2], y<- [0, nrows + 2]]
 
db2 :: Int -> Int -> Surface -> Surface -> IO Bool
db2 x y block screen
    = blitSurface block
      Nothing 
      screen
      (Just (Rect ((x * 15) + 5) ((y * 15) + 45) 15 15))
  

drawBlock :: Int -> Int -> Int -> Int -> Maybe Helpers.Color -> GraphicsInfo -> Surface -> IO Bool
drawBlock _ _ _ _ Nothing _ _ = return False
drawBlock x y xoff yoff (Just c) gri screen
    = blitSurface
      (getBlockSurface gri c)
      Nothing 
      screen
      (Just (Rect ((x * 15)+ xoff) ((y * 15) + yoff) 15 15))

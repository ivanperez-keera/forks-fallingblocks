module SplashController where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import GameState
import GraphicsInfo
import GameController

handleSplashEvents :: [Event] -> GameState -> GameState
handleSplashEvents ((KeyUp _):_) gs = gs { curController = handleGameEvents, 
                                           curRenderer = renderGameState,
                                           paused = False}
handleSplashEvents _ gs = gs


renderSplashScreen :: GameState -> IO ()
renderSplashScreen gs = do
  let gri = grInfo gs
      s = screen gri
  title <- renderTextSolid (titlefont gri) "FALLING BLOCKS!" (Color 255 0 0)
  blitSurface title Nothing s (Just (Rect 10 100 200 0))

  subtext <- renderTextSolid (font gri) "press any key" (Color 255 0 0)
  blitSurface subtext Nothing s (Just (Rect 60 150 200 0))

  SDL.flip s
  return ()

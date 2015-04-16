module GraphicsInfo where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

import Helpers
import Paths_fallingblocks

makeGraphics :: IO GraphicsInfo
makeGraphics = do
  redblock <- loadBMP' "data/redblock.bmp"
  blueblock <- loadBMP' "data/blueblock.bmp"
  yellowblock <- loadBMP' "data/yellowblock.bmp"
  greenblock <- loadBMP' "data/greenblock.bmp"
  purpleblock <- loadBMP' "data/purpleblock.bmp"
  cyanblock <- loadBMP' "data/cyanblock.bmp"
  orangeblock <- loadBMP' "data/orangeblock.bmp"

  greyblock <- loadBMP' "data/greyblock.bmp"
  font <- openFont' "data/Joystix.ttf" 16
  titlefont <- openFont' "data/Joystix.ttf" 20
  screen <- getVideoSurface

  return $ GraphicsInfo screen 
             (BS redblock 
                 blueblock
                 yellowblock
                 greenblock
                 purpleblock
                 cyanblock
                 orangeblock
             ) 
             greyblock font titlefont
      where
        loadBMP' f = loadBMP =<< getDataFileName f
        openFont' f n = do
                 file <- getDataFileName f
                 openFont file n

cleanupGraphics :: GraphicsInfo -> IO ()
cleanupGraphics gi = do
  freeSurface $ screen gi
  let bs = blocks gi
           
  freeSurface $ redBlock bs
  freeSurface $ blueBlock bs
  freeSurface $ yellowBlock bs
  freeSurface $ greenBlock bs
  freeSurface $ purpleBlock bs
  freeSurface $ cyanBlock bs
  freeSurface $ orangeBlock bs

  closeFont $ font gi
  closeFont $ titlefont gi

data BlockSurfaces = BS {
      redBlock :: Surface,
      blueBlock :: Surface,
      yellowBlock :: Surface,
      greenBlock :: Surface,
      purpleBlock :: Surface,
      cyanBlock :: Surface,
      orangeBlock :: Surface
    }


data GraphicsInfo = GraphicsInfo {
      screen :: Surface,
      blocks  :: BlockSurfaces,
      bkgdblock :: Surface,
      font   :: Font,
      titlefont :: Font
    }

getBlockSurface :: GraphicsInfo -> Helpers.Color -> Surface
getBlockSurface gi c = g $ blocks gi
    where g | c == Red = redBlock
            | c == Blue = blueBlock
            | c == Yellow = yellowBlock
            | c == Green = greenBlock
            | c == Purple = purpleBlock
            | c == Cyan = cyanBlock
            | c == Orange = orangeBlock
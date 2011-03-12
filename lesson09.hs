--
-- This code was created by Jeff Molofee '99 (ported to Haskell GHC 2005)
--

module Main where

import Graphics.UI.GLUT 
import System.Exit ( exitWith, ExitCode(..) )
import Data.IORef ( IORef, newIORef, writeIORef )
import Util ( Image(..), bitmapLoad )
import Monad ( when, liftM )
import Random

data Star = Star { starColor :: Color3 GLubyte, starDist, 
                   starAngle :: GLfloat }
            deriving Show

numStars :: Num a => a
numStars = 50

initGL :: IO TextureObject
initGL = do
  tex <- loadGLTextures
  texture Texture2D $= Enabled
  clearColor $= Color4 0 0 0 0.5 -- Clear the background color to black
  clearDepth $= 1 -- enables clearing of the depth buffer
  depthFunc  $= Nothing -- type of depth test
  shadeModel $= Smooth -- enables smooth color shading
  matrixMode $= Projection
  hint PerspectiveCorrection $= Nicest
  blendFunc $= (SrcAlpha, One)
  blend $= Enabled
  loadIdentity  -- reset projection matrix
  Size width height <- get windowSize
  perspective 45 (fromIntegral width/fromIntegral height) 0.1 100 -- calculate the aspect ratio of the window
  matrixMode $= Modelview 0

  flush -- finally, we tell opengl to do it.
  return tex

generateStars :: IO [IORef Star]
generateStars = mapM (\i -> do r <- getStdRandom (randomR (0, 255)) :: IO Int
                               g <- getStdRandom (randomR (0, 255)) :: IO Int
                               b <- getStdRandom (randomR (0, 255)) :: IO Int
                               newIORef (Star {starAngle = 0, 
                                               starColor = Color3 (fromIntegral r) (fromIntegral g) (fromIntegral b), 
                                               starDist  = (i/numStars)*5}))
                [0..numStars-1]

loadGLTextures :: IO TextureObject
loadGLTextures = do
  (Image (Size w h) pd) <- bitmapLoad "Data/Star.bmp"
  texName <- liftM head (genObjectNames 1)
  textureBinding Texture2D $= Just texName
  textureFilter  Texture2D $= ((Linear', Nothing), Linear')
  texImage2D Nothing NoProxy 0 RGB' (TextureSize2D w h) 0 pd
  return texName

resizeScene :: Size -> IO ()
resizeScene (Size w 0) = resizeScene (Size w 1) -- prevent divide by zero
resizeScene s@(Size width height) = do
  viewport   $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45 (fromIntegral width/fromIntegral height) 0.1 100
  matrixMode $= Modelview 0
  flush

colorUpgrade :: Color3 a -> a -> Color4 a
colorUpgrade (Color3 r g b) a = Color4 r g b a

drawScene :: TextureObject -> IORef GLfloat -> IORef GLfloat -> IORef Bool
             -> IORef GLfloat -> [IORef Star] -> IO ()
drawScene tex zoom tilt twinkle spin stars = do
  clear [ColorBuffer, DepthBuffer] -- clear the screen and the depth bufer
  textureBinding Texture2D $= Just tex

  mapM_ ( \(st1, st2, i) -> 
    do loadIdentity       
       s1 <- get st1
       s2 <- get st2
       sp <- get spin
       zo <- get zoom
       ti <- get tilt
       tw <- get twinkle
       translate (Vector3 0 0 (zo::GLfloat))
       rotate ti (Vector3 1 0 (0::GLfloat))
       rotate (starAngle s1) (Vector3 0 1 (0::GLfloat))
       translate (Vector3 (starDist s1) 0 (0::GLfloat))
       rotate (-(starAngle s1)) (Vector3 0 1 (0::GLfloat))
       rotate (-ti) (Vector3 1 0 (0::GLfloat))
       when tw $ 
         do color (colorUpgrade (starColor s2) 255)
            renderPrimitive Quads $ 
              do texCoord (TexCoord2 0 (0::GLfloat))
                 vertex (Vertex3 (-1) (-1) (0::GLfloat))
                 texCoord (TexCoord2 1 (0::GLfloat))
                 vertex (Vertex3 1 (-1) (0::GLfloat))
                 texCoord (TexCoord2 1 (1::GLfloat))
                 vertex (Vertex3 1 1 (0::GLfloat))
                 texCoord (TexCoord2 0 (1::GLfloat))
                 vertex (Vertex3 (-1) 1 (0::GLfloat))
       rotate sp (Vector3 0 0 (1::GLfloat))
       color (colorUpgrade (starColor s1) 255)
       renderPrimitive Quads $ 
         do texCoord (TexCoord2 0 (0::GLfloat))
            vertex (Vertex3 (-1) (-1) (0::GLfloat))
            texCoord (TexCoord2 1 (0::GLfloat))
            vertex (Vertex3 1 (-1) (0::GLfloat))
            texCoord (TexCoord2 1 (1::GLfloat))
            vertex (Vertex3 1 1 (0::GLfloat))
            texCoord (TexCoord2 0 (1::GLfloat))
            vertex (Vertex3 (-1) 1 (0::GLfloat))
       spin $= sp + 0.01
       if starDist s1 < 0 
          then do d <- return ((starDist s1)+5)
                  r <- getStdRandom (randomR (0, 255)) :: IO Int
                  g <- getStdRandom (randomR (0, 255)) :: IO Int
                  b <- getStdRandom (randomR (0, 255)) :: IO Int
                  st1 $= Star { starAngle = (starAngle s1) + i/numStars,
                                starColor = Color3 (fromIntegral r) (fromIntegral g) (fromIntegral b),
                                starDist  = d }
          else do st1 $= Star { starAngle = (starAngle s1) + i/numStars,
                                starColor = (starColor s1),
                                starDist  = (starDist s1)-0.01 }) (zip3 stars (reverse stars) [0..numStars-1]) -- finally the second parameter to mapM_
  -- since this is double buffered, swap the buffers to display what was just
  -- drawn
  flush
  swapBuffers

keyPressed :: IORef Bool -> IORef GLfloat -> IORef GLfloat 
              -> KeyboardMouseCallback
-- 27 is ESCAPE
keyPressed _ _ _ (Char '\27') Down _ _ = exitWith ExitSuccess
keyPressed t _ _ (Char 'T') Down _  _ = do
  twinkle <- get t
  if twinkle
     then t $= False
     else t $= True
keyPressed tw zo ti (Char 't') Down x y = keyPressed tw zo ti (Char 'T') Down x y
keyPressed _ zoom _ (SpecialKey KeyPageUp) Down _ _ = 
  get zoom >>= writeIORef zoom . (subtract 0.2)
keyPressed _ zoom _ (SpecialKey KeyPageDown) Down _ _ = 
  get zoom >>= writeIORef zoom . (+0.2)
keyPressed _ _ tilt (SpecialKey KeyUp) Down _ _ = 
  get tilt >>= writeIORef tilt . (subtract 0.5)          
keyPressed _ _ tilt (SpecialKey KeyDown) Down _ _ = 
  get tilt >>= writeIORef tilt . (+ 0.5)
keyPressed _ _ _ _    _ _ _ = do return ()

main :: IO ()
main = do
     -- Initialize GLUT state - glut will take any command line arguments
     -- that pertain to it or X windows -- look at its documentation at
     -- http://reality.sgi.com/mjk/spec3/spec3.html
     _ <- getArgsAndInitialize
     -- select type of display mode:
     -- Double buffer
     -- RGBA color
     -- Alpha components supported
     -- Depth buffer
     initialDisplayMode $= [ DoubleBuffered, RGBAMode, WithDepthBuffer, 
                             WithAlphaComponent ]
     -- get an 800 x 600 window
     initialWindowSize $= Size 800 600
     -- window starts at upper left corner of the screen
     initialWindowPosition $= Position 0 0
     -- open a window
     _ <- createWindow "Jeff Molofee's GL Code Tutorial ... NeHe '99"
     -- register the function to do all our OpenGL drawing
     twinkle <- newIORef False
     spin <- newIORef 0
     stars <- generateStars
     zoom <- newIORef (-15)
     tilt <- newIORef 90
     -- initialize our window.
     tex <- initGL
     displayCallback $= (drawScene tex zoom tilt twinkle spin stars)
     -- go fullscreen. This is as soon as possible.
     fullScreen
     -- even if there are no events, redraw our gl scene
     idleCallback $= Just (drawScene tex zoom tilt twinkle spin stars)
     -- register the funciton called when our window is resized
     reshapeCallback $= Just resizeScene
     -- register the function called when the keyboard is pressed.
     keyboardMouseCallback $= Just (keyPressed twinkle zoom tilt)
     -- start event processing engine
     mainLoop

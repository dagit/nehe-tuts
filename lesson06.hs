--
-- This code was created by Jeff Molofee '99 (ported to Haskell GHC 2005)
--

module Main where

import Graphics.UI.GLUT 
import System.Exit ( exitWith, ExitCode(..) )
import Monad ( liftM )
import Data.IORef ( IORef, newIORef )
import Util ( Image(..), bitmapLoad )

initGL :: IO TextureObject
initGL = do
  tex <- loadGLTextures
  texture Texture2D $= Enabled
  clearColor $= Color4 0 0 0 0.5 -- Clear the background color to black
  clearDepth $= 1 -- enables clearing of the depth buffer
  depthFunc  $= Just Less -- type of depth test
  shadeModel $= Smooth -- enables smooth color shading
  matrixMode $= Projection
  loadIdentity  -- reset projection matrix
  Size width height <- get windowSize
  perspective 45 (fromIntegral width/fromIntegral height) 0.1 100 -- calculate the aspect ratio of the window
  matrixMode $= Modelview 0

  flush -- finally, we tell opengl to do it.
  return tex

loadGLTextures :: IO TextureObject
loadGLTextures = do
  (Image (Size w h) pd) <- bitmapLoad "Data/NeHe.bmp"
  texName <- liftM head (genObjectNames 1)
  textureBinding Texture2D $= Just texName
  textureFilter  Texture2D $= ((Nearest, Nothing), Nearest)
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

drawScene :: TextureObject -> IORef GLfloat -> IORef GLfloat 
             -> IORef GLfloat -> IO ()
drawScene tex xrot yrot zrot = do
  clear [ColorBuffer, DepthBuffer] -- clear the screen and the depth bufer
  loadIdentity  -- reset view

  translate (Vector3 0 0 (-5.0::GLfloat)) --Move left 5 Units into the screen

  xr <- get xrot
  yr <- get yrot
  zr <- get zrot
  rotate xr (Vector3 1 0 (0::GLfloat)) -- Rotate the triangle on the Y axis
  rotate yr (Vector3 0 1 (0::GLfloat)) -- Rotate the triangle on the Y axis
  rotate zr (Vector3 0 0 (1::GLfloat)) -- Rotate the triangle on the Y axis

  textureBinding Texture2D $= Just tex
  renderPrimitive Quads $  -- start drawing a polygon (4 sided)
    do 
       -- first the front
       texCoord (TexCoord2 0 (0::GLfloat)) 
       vertex (Vertex3 (-1) (-1)   (1::GLfloat))  -- bottom left of quad (Front)
       texCoord (TexCoord2 1 (0::GLfloat)) 
       vertex (Vertex3  1   (-1)   (1::GLfloat))  -- bottom right of quad (Front)
       texCoord (TexCoord2 1 (1::GLfloat)) 
       vertex (Vertex3  1      1   (1::GLfloat))  -- top right of quad (Front)
       texCoord (TexCoord2 0 (1::GLfloat)) 
       vertex (Vertex3 (-1)    1   (1::GLfloat))  -- top left of quad (Front)
       -- now the back
       texCoord (TexCoord2 1 (0::GLfloat)) 
       vertex (Vertex3 (-1) (-1)  (-1::GLfloat))  -- bottom right of quad (Back)
       texCoord (TexCoord2 1 (1::GLfloat)) 
       vertex (Vertex3 (-1)    1  (-1::GLfloat))  -- top right of quad (Back)
       texCoord (TexCoord2 0 (1::GLfloat)) 
       vertex (Vertex3    1    1  (-1::GLfloat))  -- top left of quad (Back)
       texCoord (TexCoord2 0 (0::GLfloat)) 
       vertex (Vertex3    1 (-1)  (-1::GLfloat))  -- bottom left of quad (Back)
       -- now the top
       texCoord (TexCoord2 0 (1::GLfloat))
       vertex (Vertex3 (-1)  1   (-1::GLfloat))  -- top left of quad (Top)
       texCoord (TexCoord2 0 (0::GLfloat))  
       vertex (Vertex3 (-1)  1    (1::GLfloat))  -- bottom left of quad (Top)
       texCoord (TexCoord2 1 (0::GLfloat))  
       vertex (Vertex3  1    1    (1::GLfloat))  -- bottom right of quad (Top)
       texCoord (TexCoord2 1 (1::GLfloat))  
       vertex (Vertex3  1    1   (-1::GLfloat))  -- top right of quad (Top)
       -- now the bottom
       texCoord (TexCoord2 1 (1::GLfloat))  
       vertex (Vertex3  1   (-1)   (1::GLfloat))  -- top right of quad (Bottom)
       texCoord (TexCoord2 0 (1::GLfloat))  
       vertex (Vertex3 (-1) (-1)   (1::GLfloat))  -- top left of quad (Bottom)
       texCoord (TexCoord2 0 (0::GLfloat))  
       vertex (Vertex3 (-1) (-1)  (-1::GLfloat))  -- bottom left of quad (Bottom)
       texCoord (TexCoord2 1 (0::GLfloat))  
       vertex (Vertex3  1   (-1)  (-1::GLfloat))  -- bottom right of quad (Bottom)
       -- now the right
       texCoord (TexCoord2 1 (0::GLfloat))  
       vertex (Vertex3 1 (-1)  (-1::GLfloat))  -- bottom right of quad (Right)
       texCoord (TexCoord2 1 (1::GLfloat))  
       vertex (Vertex3 1    1  (-1::GLfloat))  -- top right of quad (Right)
       texCoord (TexCoord2 0 (1::GLfloat))  
       vertex (Vertex3 1    1   (1::GLfloat))  -- top left of quad (Right)
       texCoord (TexCoord2 0 (0::GLfloat))  
       vertex (Vertex3 1 (-1)   (1::GLfloat))  -- bottom left of quad (Right)
       -- now the left
       texCoord (TexCoord2 0 (0::GLfloat))  
       vertex (Vertex3 (-1) (-1)  (-1::GLfloat))  -- bottom left of quad (Left)
       texCoord (TexCoord2 1 (0::GLfloat))  
       vertex (Vertex3 (-1)    1  (-1::GLfloat))  -- top left of quad (Left)
       texCoord (TexCoord2 1 (1::GLfloat))  
       vertex (Vertex3 (-1)    1   (1::GLfloat))  -- top right of quad (Left)
       texCoord (TexCoord2 0 (1::GLfloat))  
       vertex (Vertex3 (-1) (-1)   (1::GLfloat))  -- bottom right of quad (Left)
  
  xrot $= xr + 0.3
  yrot $= yr + 0.2
  zrot $= zr + 0.4

  -- since this is double buffered, swap the buffers to display what was just
  -- drawn
  flush
  swapBuffers


keyPressed :: KeyboardMouseCallback
-- 27 is ESCAPE
keyPressed (Char '\27') Down _ _ = exitWith ExitSuccess
keyPressed _            _    _ _ = return ()

main :: IO ()
main = do
     -- Initialize GLUT state - glut will take any command line arguments
     -- that pertain to it or X windows -- look at its documentation at
     -- http://reality.sgi.com/mjk/spec3/spec3.html
     getArgsAndInitialize 
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
     createWindow "Jeff Molofee's GL Code Tutorial ... NeHe '99"
     -- register the function to do all our OpenGL drawing
     xrot <- newIORef 0
     yrot <- newIORef 0
     zrot <- newIORef 0
     -- initialize our window.
     tex <- initGL
     displayCallback $= (drawScene tex xrot yrot zrot)
     -- go fullscreen. This is as soon as possible.
     fullScreen
     -- even if there are no events, redraw our gl scene
     idleCallback $= Just (drawScene tex xrot yrot zrot)
     -- register the funciton called when our window is resized
     reshapeCallback $= Just resizeScene
     -- register the function called when the keyboard is pressed.
     keyboardMouseCallback $= Just keyPressed
     -- start event processing engine
     mainLoop

--
-- This code was created by Jeff Molofee '99 (ported to Haskell GHC 2005)
--

module Main where

import Graphics.UI.GLUT 
import System.Exit ( exitWith, ExitCode(..) )
import Data.IORef ( IORef, newIORef, modifyIORef )
import Util ( Image(..), bitmapLoad )
import Monad ( liftM )

boxcol :: [Color3 GLfloat]
boxcol = [Color3 1 0 0, Color3 1 0.5 0, Color3 1 1 0, 
          Color3 0 1 0, Color3 0 1 1]
topcol :: [Color3 GLfloat]
topcol = [Color3 0.5 0 0, Color3 0.5 0.25 0, Color3 0.5 0.5 0,
          Color3 0 0.5 0, Color3 0 0.5 0.5]

buildLists :: IO (DisplayList, DisplayList)
buildLists = do
  box <- defineNewList Compile $ 
    do renderPrimitive Quads $ 
         do { texCoord (TexCoord2 1 (1::GLfloat)); vertex (Vertex3 (-1) (-1) (-1::GLfloat));
              texCoord (TexCoord2 0.0 (1.0::GLfloat)); vertex(Vertex3 1.0 (-1.0) (-1.0::GLfloat));      -- Top Left Of The Texture and Quad
              texCoord (TexCoord2 0.0 (0.0::GLfloat)); vertex(Vertex3 1.0 (-1.0)  (1.0::GLfloat));      -- Bottom Left Of The Texture and Quad
              texCoord (TexCoord2 1.0 (0.0::GLfloat)); vertex(Vertex3(-1.0) (-1.0)  (1.0::GLfloat));      -- Bottom Right Of The Texture and Quad
              -- Front Face
              texCoord (TexCoord2 0.0 (0.0::GLfloat)); vertex(Vertex3(-1.0) (-1.0)  (1.0::GLfloat));      -- Bottom Left Of The Texture and Quad
              texCoord (TexCoord2 1.0 (0.0::GLfloat)); vertex(Vertex3 1.0 (-1.0)  (1.0::GLfloat));      -- Bottom Right Of The Texture and Quad
              texCoord (TexCoord2 1.0 (1.0::GLfloat)); vertex(Vertex3 1.0  1.0  (1.0::GLfloat));      -- Top Right Of The Texture and Quad
              texCoord (TexCoord2 0.0 (1.0::GLfloat)); vertex(Vertex3(-1.0)  1.0  (1.0::GLfloat));      -- Top Left Of The Texture and Quad
              -- Back Face
              texCoord (TexCoord2 1.0 (0.0::GLfloat)); vertex(Vertex3(-1.0) (-1.0) (-1.0::GLfloat));      -- Bottom Right Of The Texture and Quad
              texCoord (TexCoord2 1.0 (1.0::GLfloat)); vertex(Vertex3(-1.0)  1.0 (-1.0::GLfloat));      -- Top Right Of The Texture and Quad
              texCoord (TexCoord2 0.0 (1.0::GLfloat)); vertex(Vertex3 1.0  1.0 (-1.0::GLfloat));      -- Top Left Of The Texture and Quad
              texCoord (TexCoord2 0.0 (0.0::GLfloat)); vertex(Vertex3 1.0 (-1.0) (-1.0::GLfloat));      -- Bottom Left Of The Texture and Quad
              -- Right face
              texCoord (TexCoord2 1.0 (0.0::GLfloat)); vertex(Vertex3 1.0 (-1.0) (-1.0::GLfloat));      -- Bottom Right Of The Texture and Quad
              texCoord (TexCoord2 1.0 (1.0::GLfloat)); vertex(Vertex3 1.0  1.0 (-1.0::GLfloat));      -- Top Right Of The Texture and Quad
              texCoord (TexCoord2 0.0 (1.0::GLfloat)); vertex(Vertex3 1.0  1.0  (1.0::GLfloat));      -- Top Left Of The Texture and Quad
              texCoord (TexCoord2 0.0 (0.0::GLfloat)); vertex(Vertex3 1.0 (-1.0)  (1.0::GLfloat));      -- Bottom Left Of The Texture and Quad
              -- Left Face
              texCoord (TexCoord2 0.0 (0.0::GLfloat)); vertex(Vertex3(-1.0) (-1.0) (-1.0::GLfloat));      -- Bottom Left Of The Texture and Quad
              texCoord (TexCoord2 1.0 (0.0::GLfloat)); vertex(Vertex3(-1.0) (-1.0)  (1.0::GLfloat));      -- Bottom Right Of The Texture and Quad
              texCoord (TexCoord2 1.0 (1.0::GLfloat)); vertex(Vertex3(-1.0)  1.0  (1.0::GLfloat));      -- Top Right Of The Texture and Quad
              texCoord (TexCoord2 0.0 (1.0::GLfloat)); vertex(Vertex3(-1.0)  1.0 (-1.0::GLfloat));}      -- Top Left Of The Texture and Quad
  top <- defineNewList Compile $ 
    do renderPrimitive Quads $ 
         do { texCoord (TexCoord2 0 (1::GLfloat)); vertex (Vertex3 (-1) 1 (-1::GLfloat));
              texCoord (TexCoord2 0 (0::GLfloat)); vertex (Vertex3 (-1) 1 (1::GLfloat));
              texCoord (TexCoord2 1 (0::GLfloat)); vertex (Vertex3 1 1 (1::GLfloat));
              texCoord (TexCoord2 1 (1::GLfloat)); vertex (Vertex3 1 1 (-1::GLfloat)); }
  return (box, top)

initGL :: IO TextureObject
initGL = do
  tex <- loadGLTextures
  texture Texture2D $= Enabled
  clearColor $= Color4 0 0 0 0.5 -- Clear the background color to black
  clearDepth $= 1 -- enables clearing of the depth buffer
  depthFunc  $= Just Less -- type of depth test
  shadeModel $= Smooth -- enables smooth color shading
  matrixMode $= Projection
  hint PerspectiveCorrection $= Nicest
  loadIdentity  -- reset projection matrix
  Size width height <- get windowSize
  perspective 45 (fromIntegral width/fromIntegral height) 0.1 100 -- calculate the aspect ratio of the window
  matrixMode $= Modelview 0
  light (Light 0) $= Enabled
  lighting $= Enabled
  colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
  flush -- finally, we tell opengl to do it.
  return tex

loadGLTextures :: IO TextureObject
loadGLTextures = do
  (Image (Size w h) pd) <- bitmapLoad "Data/cube.bmp"
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
             -> DisplayList -> DisplayList -> IO ()
drawScene tex xrot yrot box top = do
  clear [ColorBuffer, DepthBuffer] -- clear the screen and the depth bufer
  textureBinding Texture2D $= Just tex

  xr <- get xrot
  yr <- get yrot

  mapM_ (\(x,y) -> 
    do { loadIdentity;
         let {x' = fromIntegral x;
              y' = fromIntegral y };
         translate (Vector3 (1.4+x'*2.8-y'*1.4) (((6-y')*2.4)-7)  (-20::GLfloat));
         rotate (45.0-(2.0*y')+xr) (Vector3 1 0 (0::GLfloat));
         rotate (45-yr) (Vector3 0 1 (0::GLfloat));
         color (boxcol !! (y-1));
         callList box;
         color (topcol !! (y-1));
         callList top } ) [(x,y) | y <- [1..5], x <- [0..y-1] ]

  -- since this is double buffered, swap the buffers to display what was just
  -- drawn
  flush
  swapBuffers

keyPressed :: IORef GLfloat -> IORef GLfloat -> KeyboardMouseCallback
-- 27 is ESCAPE
keyPressed _ _ (Char '\27') Down _ _ = exitWith ExitSuccess
keyPressed xrot _ (SpecialKey KeyUp) Down _ _ = modifyIORef xrot (subtract 0.8)
keyPressed xrot _ (SpecialKey KeyDown) Down _ _ = modifyIORef xrot (+0.8)
keyPressed _ yrot (SpecialKey KeyLeft) Down _ _ = modifyIORef yrot (subtract 0.8)
keyPressed _ yrot (SpecialKey KeyRight) Down _ _ = modifyIORef yrot (+0.8)
keyPressed _ _ _            _    _ _ = return ()

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
     
     -- initialize our window.
     tex <- initGL
     (box, top) <- buildLists
     displayCallback $= (drawScene tex xrot yrot box top)
     -- go fullscreen. This is as soon as possible.
     fullScreen
     -- even if there are no events, redraw our gl scene
     idleCallback $= Just (drawScene tex xrot yrot box top)
     -- register the funciton called when our window is resized
     reshapeCallback $= Just resizeScene
     -- register the function called when the keyboard is pressed.
     keyboardMouseCallback $= Just (keyPressed xrot yrot)
     -- start event processing engine
     mainLoop

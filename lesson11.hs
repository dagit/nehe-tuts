--
-- This code was created by Jeff Molofee '99 (ported to Haskell GHC 2005)
--

module Main where

import Graphics.UI.GLUT 
import System.Exit ( exitWith, ExitCode(..) )
import Data.IORef ( IORef, newIORef, modifyIORef )
import Util ( Image(..), bitmapLoad )
import Monad ( liftM, liftM3, when )
import Data.Array.IO ( readArray, IOUArray, newListArray )

type Points = IOUArray (Int, Int, Int) Float

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
  polygonMode $= (Line, Fill) -- I can only assume that the first one is front and the second one is back.  The documenation doesn't say.
  flush -- finally, we tell opengl to do it.
  return tex

loadGLTextures :: IO TextureObject
loadGLTextures = do
  (Image (Size w h) pd) <- bitmapLoad "Data/tim.bmp"
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

drawScene :: TextureObject -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat
             -> Points -> IORef Int -> IORef Int -> IO () 
drawScene tex xrot yrot zrot points wiggleRef offsetRef = do
  clear [ColorBuffer, DepthBuffer] -- clear the screen and the depth bufer
  loadIdentity  -- reset view

  translate (Vector3 0 0 (-12.0::GLfloat)) --Move left 5 Units into the screen

  xr <- get xrot
  yr <- get yrot
  zr <- get zrot
  offset <- get offsetRef
  wiggle <- get wiggleRef
  rotate xr (Vector3 1 0 (0::GLfloat)) -- Rotate the triangle on the Y axis
  rotate yr (Vector3 0 1 (0::GLfloat)) -- Rotate the triangle on the Y axis
  rotate zr (Vector3 0 0 (1::GLfloat)) -- Rotate the triangle on the Y axis
  textureBinding Texture2D $= Just tex
  {-# SCC "renderPrimitive" #-}renderPrimitive Quads $  -- start drawing a polygon (4 sided)
    mapM_ ( \(x, y) -> do
       let x' = (x+offset) `mod` 45
       let fx = fromIntegral x/44 :: GLfloat
       let fy = fromIntegral y/44 :: GLfloat
       let fxb = fromIntegral (x+1)/44 :: GLfloat
       let fyb = fromIntegral (y+1)/44 :: GLfloat
       {-# SCC "TexCoord2" #-}texCoord (TexCoord2 fx fy)
       {-# SCC "vertex1" #-}vertex =<< liftM3 Vertex3 (readArray points (x,y,0))
                                 (readArray points (x,y,1))
                                 (readArray points (x',y,2))
       texCoord (TexCoord2 fx fyb)
       {-# SCC "vertex2" #-}vertex =<< liftM3 Vertex3 (readArray points (x,y+1,0))
                                 (readArray points (x,y+1,1))
                                 (readArray points (x',y+1,2))
       texCoord (TexCoord2 fxb fyb)
       {-# SCC "vertex3" #-}vertex =<< liftM3 Vertex3 (readArray points (x+1,y+1,0))
                                 (readArray points (x+1,y+1,1))
                                 (readArray points ((x'+1)`mod`45,y+1,2))
       texCoord (TexCoord2 fxb fy)
       {-# SCC "vertex4" #-}vertex =<< liftM3 Vertex3 (readArray points (x+1,y,0))
                                 (readArray points (x+1,y,1))
                                 (readArray points ((x'+1)`mod`45,y,2)) )
    [(x,y) | x <- [0..43], y<-[0..43]]
  xrot $= xr + 0.3
  yrot $= yr + 0.2
  zrot $= zr + 0.4

  when (wiggle == 2) $ do
    offsetRef $= offset + 1
    wiggleRef $= 0 

  {-# SCC "modifyIORef" #-}modifyIORef wiggleRef (+ 1)
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
     initialWindowSize $= Size 640 480
     -- window starts at upper left corner of the screen
     initialWindowPosition $= Position 0 0
     -- open a window
     createWindow "Jeff Molofee's GL Code Tutorial ... NeHe '99"
     -- register the function to do all our OpenGL drawing
     xrot <- newIORef 0
     yrot <- newIORef 0
     zrot <- newIORef 0
     wiggle <- newIORef 0
     offset <- newIORef 0
     elems <- return $ concat [[((fromIntegral x/5)-4.5), 
                                ((fromIntegral y/5)-4.5),
                                sin (((fromIntegral x/5)*40/360)*pi*2)] 
                                | x <- [0..44]::[Int], y <- [0..44]::[Int] ]
     points <- newListArray ((0,0,0), (44,44,2)) elems :: IO Points
     -- initialize our window.
     tex <- initGL
     displayCallback $= (drawScene tex xrot yrot zrot points wiggle offset)
     -- go fullscreen. This is as soon as possible.
     --fullScreen
     -- even if there are no events, redraw our gl scene
     idleCallback $= Just (drawScene tex xrot yrot zrot points wiggle offset)
     -- register the funciton called when our window is resized
     reshapeCallback $= Just resizeScene
     -- register the function called when the keyboard is pressed.
     keyboardMouseCallback $= Just keyPressed
     -- start event processing engine
     mainLoop

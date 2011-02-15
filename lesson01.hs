--
-- This code was created by Jeff Molofee '99 (ported to Haskell GHC 2005)
--

module Main where

import Graphics.UI.GLUT
import System.Exit ( exitWith, ExitCode(..) )
import Control.Concurrent ( threadDelay )

initGL :: IO ()
initGL = do
  clearColor $= Color4 0 0 0 0 -- Clear the background color to black
  clearDepth $= 1 -- enables clearing of the depth buffer
  depthFunc $= Just Less -- type of depth test
  shadeModel $= Smooth -- enables smooth color shading
  matrixMode $= Projection
  loadIdentity  -- reset projection matrix
  Size width height <- get windowSize
  perspective 45 (fromIntegral width/fromIntegral height) 0.1 100 -- calculate the aspect ratio of the window
  matrixMode $= Modelview 0

  flush -- finally, we tell opengl to do it.

resizeScene :: Size -> IO ()
resizeScene (Size w 0) = resizeScene (Size w 1) -- prevent divide by zero
resizeScene s@(Size width height) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45 (fromIntegral width/fromIntegral height) 0.1 100
  matrixMode $= Modelview 0
  flush

drawScene :: IO ()
drawScene = do
  clear [ColorBuffer, DepthBuffer] -- clear the screen and the depth bufer
  loadIdentity  -- reset view
  -- since this is double buffered, swap the buffers to display what was just
  -- drawn
  swapBuffers
  flush

keyPressed :: KeyboardMouseCallback
-- 27 is ESCAPE
keyPressed (Char '\27') Down _ _ = exitWith ExitSuccess
keyPressed _            _    _ _ = do threadDelay 100
                                      return ()

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
     -- get a 640 x 480 window
     initialWindowSize $= Size 640 480
     -- window starts at upper left corner of the screen
     initialWindowPosition $= Position 0 0
     -- open a window
     createWindow "Jeff Molofee's GL Code Tutorial ... NeHe '99"
     -- register the function to do all our OpenGL drawing
     displayCallback $= drawScene
     -- go fullscreen. This is as soon as possible.
     fullScreen
     -- register the funciton called when our window is resized
     reshapeCallback $= Just resizeScene
     -- register the function called when the keyboard is pressed.
     keyboardMouseCallback $= Just keyPressed
     -- initialize our window.
     initGL
     -- start event processing engine
     mainLoop

--
-- This code was created by Jeff Molofee '99 (ported to Haskell GHC 2005)
--

module Main where

import Graphics.UI.GLUT
import System.Exit ( exitWith, ExitCode(..) )
import Data.IORef ( IORef, newIORef )

tincrement, qincrement :: GLfloat
tincrement = 0.2
qincrement = -0.15

initGL :: IO ()
initGL = do
  clearColor $= Color4 0 0 0 0 -- Clear the background color to black
  clearDepth $= 1 -- enables clearing of the depth buffer
  depthFunc  $= Just Less -- type of depth test
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
  viewport   $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45 (fromIntegral width/fromIntegral height) 0.1 100
  matrixMode $= Modelview 0
  flush

drawScene :: IORef GLfloat -> IORef GLfloat -> IO ()
drawScene rtri rquad = do
  clear [ColorBuffer, DepthBuffer] -- clear the screen and the depth bufer
  loadIdentity  -- reset view

  translate (Vector3 (-1.5) 0 (-6.0::GLfloat)) --Move left 1.5 Units and into the screen 6.0
  rt <- get rtri
  rotate rt (Vector3 0 1 (0::GLfloat)) -- Rotate the triangle on the Y axis

  -- draw a triangle (in smooth coloring mode)
  renderPrimitive Triangles $  -- start drawing a polygon
    do -- first the front
       color  (Color3 1 0 (0::GLfloat))          -- set The color to Red
       vertex (Vertex3  0      1  (0::GLfloat))  -- top of triangle (front)
       color  (Color3 0 1 (0::GLfloat))          -- set The color to Green
       vertex (Vertex3 (-1) (-1)  (1::GLfloat))  -- left of triangle (front)
       color  (Color3 0 0 (1::GLfloat))          -- set The color to Blue
       vertex (Vertex3  1   (-1)  (1::GLfloat))  -- right of triangle (front)
       -- now the right
       color  (Color3 1 0 (0::GLfloat))          -- set The color to Red
       vertex (Vertex3  0      1  (0::GLfloat))  -- top of triangle (right)
       color  (Color3 0 0 (1::GLfloat))          -- set The color to Blue
       vertex (Vertex3  1   (-1)  (1::GLfloat))  -- left of triangle (right)
       color  (Color3 0 1 (0::GLfloat))          -- set The color to Green
       vertex (Vertex3  1  (-1)  (-1::GLfloat))  -- right of triangle (front)
       -- now the back
       color  (Color3 1 0 (0::GLfloat))          -- set The color to Red
       vertex (Vertex3  0      1  (0::GLfloat))  -- top of triangle (back)
       color  (Color3 0 1 (0::GLfloat))          -- set The color to Green
       vertex (Vertex3  1 (-1)   (-1::GLfloat))  -- left of triangle (back)
       color  (Color3 0 0 (1::GLfloat))          -- set The color to Blue
       vertex (Vertex3 (-1) (-1) (-1::GLfloat))  -- right of triangle (back)
       -- now the left
       color  (Color3 1 0 (0::GLfloat))          -- set The color to Red
       vertex (Vertex3  0      1  (0::GLfloat))  -- top of triangle (left)
       color  (Color3 0 0 (1::GLfloat))          -- set The color to Blue
       vertex (Vertex3 (-1) (-1) (-1::GLfloat))  -- left of triangle (left)
       color  (Color3 0 1 (0::GLfloat))          -- set The color to Green
       vertex (Vertex3 (-1) (-1)  (1::GLfloat))  -- right of triangle (left)

       
  loadIdentity
  translate (Vector3 1.5 0 (-7::GLfloat))  -- move right three units
  rq <- get rquad
  rotate rq (Vector3 1 1 (1::GLfloat)) -- rotate the quad on the x axis

  renderPrimitive Quads $  -- start drawing a polygon (4 sided)
    do -- first the top
       color (Color3 0 1 (0::GLfloat))  -- set color to green
       vertex (Vertex3  1    1   (-1::GLfloat))  -- top right of quad (Top)
       vertex (Vertex3 (-1)  1   (-1::GLfloat))  -- top left of quad (Top)
       vertex (Vertex3 (-1)  1    (1::GLfloat))  -- bottom left of quad (Top)
       vertex (Vertex3  1    1    (1::GLfloat))  -- bottom right of quad (Top)
       -- now the bottom
       color (Color3 1 0.5 (0::GLfloat))  -- set color to orage
       vertex (Vertex3  1   (-1)   (1::GLfloat))  -- top right of quad (Bottom)
       vertex (Vertex3 (-1) (-1)   (1::GLfloat))  -- top left of quad (Bottom)
       vertex (Vertex3 (-1) (-1)  (-1::GLfloat))  -- bottom left of quad (Bottom)
       vertex (Vertex3  1   (-1)  (-1::GLfloat))  -- bottom right of quad (Bottom)
       -- now the front
       color (Color3 1 0 (0::GLfloat))  -- set color to red
       vertex (Vertex3  1      1   (1::GLfloat))  -- top right of quad (Bottom)
       vertex (Vertex3 (-1)    1   (1::GLfloat))  -- top left of quad (Bottom)
       vertex (Vertex3 (-1) (-1)   (1::GLfloat))  -- bottom left of quad (Bottom)
       vertex (Vertex3  1   (-1)   (1::GLfloat))  -- bottom right of quad (Bottom)
       -- now the back
       color (Color3 1 1 (0::GLfloat))  -- set color to yellow
       vertex (Vertex3    1 (-1)  (-1::GLfloat))  -- bottom left of quad (Back)
       vertex (Vertex3 (-1) (-1)  (-1::GLfloat))  -- bottom right of quad (Back)
       vertex (Vertex3 (-1)    1  (-1::GLfloat))  -- top right of quad (Back)
       vertex (Vertex3    1    1  (-1::GLfloat))  -- top left of quad (Back)
       -- now the left
       color (Color3 0 0 (1::GLfloat))  -- set color to blue
       vertex (Vertex3 (-1)    1   (1::GLfloat))  -- top right of quad (Left)
       vertex (Vertex3 (-1)    1  (-1::GLfloat))  -- top left of quad (Left)
       vertex (Vertex3 (-1) (-1)  (-1::GLfloat))  -- bottom left of quad (Left)
       vertex (Vertex3 (-1) (-1)   (1::GLfloat))  -- bottom right of quad (Left)
       -- now the right
       color (Color3 1 0 (1::GLfloat))  -- set color to violet
       vertex (Vertex3 1    1  (-1::GLfloat))  -- top right of quad (Right)
       vertex (Vertex3 1    1   (1::GLfloat))  -- top left of quad (Right)
       vertex (Vertex3 1 (-1)   (1::GLfloat))  -- bottom left of quad (Right)
       vertex (Vertex3 1 (-1)  (-1::GLfloat))  -- bottom right of quad (Right)
  
  rtri  $= rt + tincrement --increase the rotation angle for the triangle
  rquad $= rq + qincrement --increase the rotation angle for the quad

  -- since this is double buffered, swap the buffers to display what was just
  -- drawn
  swapBuffers
  flush

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
     -- get a 640 x 480 window
     initialWindowSize $= Size 800 600
     -- window starts at upper left corner of the screen
     initialWindowPosition $= Position 0 0
     -- open a window
     createWindow "Jeff Molofee's GL Code Tutorial ... NeHe '99"
     -- register the function to do all our OpenGL drawing
     rt <- newIORef 0
     rq <- newIORef 0
     displayCallback $= (drawScene rt rq)
     -- go fullscreen. This is as soon as possible.
     fullScreen
     -- even if there are no events, redraw our gl scene
     idleCallback $= Just (drawScene rt rq)
     -- register the funciton called when our window is resized
     reshapeCallback $= Just resizeScene
     -- register the function called when the keyboard is pressed.
     keyboardMouseCallback $= Just keyPressed
     -- initialize our window.
     initGL
     -- start event processing engine
     mainLoop
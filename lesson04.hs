--
-- This code was created by Jeff Molofee '99 (ported to Haskell GHC 2005)
--

module Main where

import qualified Graphics.UI.GLFW as GLFW
-- everything from here starts with gl or GL
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw ( gluPerspective )
import Data.Bits ( (.|.) )
import System.Exit ( exitWith, ExitCode(..) )
import Control.Monad ( forever )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )

increment :: GLfloat
increment = 1.5

initGL :: IO ()
initGL = do
  glShadeModel gl_SMOOTH -- enables smooth color shading
  glClearColor 0 0 0 0 -- Clear the background color to black
  glClearDepth 1 -- enables clearing of the depth buffer
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL  -- type of depth test
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST

resizeScene :: GLFW.WindowSizeCallback
resizeScene w     0      = resizeScene w 1 -- prevent divide by zero
resizeScene width height = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 100
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

drawScene :: IORef GLfloat -> IORef GLfloat -> IO ()
drawScene rtri rquad = do
  -- clear the screen and the depth buffer
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity -- reset view

  glTranslatef (-1.5) 0 (-6.0) --Move left 1.5 Units and into the screen 6.0
  rt <- readIORef rtri
  glRotatef rt 0 1 0 -- Rotate the triangle on the Y axis

  -- draw a triangle (in smooth coloring mode)
  glBegin gl_TRIANGLES -- start drawing a polygon
  glColor3f    1    0  0  -- set the color to Red
  glVertex3f   0    1  0  -- top
  glColor3f    0    1  0  -- set the color to Green
  glVertex3f   1  (-1) 0  -- bottom right
  glColor3f    0    0  1  -- set the color to Blue
  glVertex3f (-1) (-1) 0  -- bottom left
  glEnd

  glLoadIdentity
  glTranslatef 1.5 0 (-6)  -- move right three units
  rq <- readIORef rquad
  glRotatef rq 1 0 0 -- rotate the quad on the x axis

  glColor3f 0.5 0.5 1  -- set color to a blue shade
  glBegin gl_QUADS  -- start drawing a polygon (4 sided)
  glVertex3f (-1)   1  0 -- top left
  glVertex3f   1    1  0 -- top right
  glVertex3f   1  (-1) 0 -- bottom right
  glVertex3f (-1) (-1) 0 -- bottom left
  glEnd
 
  -- increase the rotation angle for the triangle
  writeIORef rtri  $! rt + increment
  -- increase the rotation angle for the quad
  writeIORef rquad $! rq + increment

  glFlush

shutdown :: GLFW.WindowCloseCallback
shutdown = do
  GLFW.closeWindow
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return True

keyPressed :: GLFW.KeyCallback
keyPressed GLFW.KeyEsc True = shutdown >> return ()
keyPressed _           _    = return ()

main :: IO ()
main = do
     True <- GLFW.initialize
     -- select type of display mode:
     -- Double buffer
     -- RGBA color
     -- Alpha components supported
     -- Depth buffer
     let dspOpts = GLFW.defaultDisplayOptions
                     -- get a 800 x 600 window
                     { GLFW.displayOptions_width  = 800
                     , GLFW.displayOptions_height = 600
                     -- Set depth buffering and RGBA colors
                     , GLFW.displayOptions_numRedBits   = 8
                     , GLFW.displayOptions_numGreenBits = 8
                     , GLFW.displayOptions_numBlueBits  = 8
                     , GLFW.displayOptions_numAlphaBits = 8
                     , GLFW.displayOptions_numDepthBits = 1
                     -- , GLFW.displayOptions_displayMode = GLFW.Fullscreen
                     }
     -- open a window
     True <- GLFW.openWindow dspOpts
     -- window starts at upper left corner of the screen
     GLFW.setWindowPosition 0 0
     GLFW.setWindowTitle "Jeff Molofee's GL Code Tutorial ... NeHe '99"
     -- register the function to do all our OpenGL drawing
     rt <- newIORef 0
     rq <- newIORef 0
     GLFW.setWindowRefreshCallback (drawScene rt rq)
     -- register the funciton called when our window is resized
     GLFW.setWindowSizeCallback resizeScene
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback keyPressed
     GLFW.setWindowCloseCallback shutdown
     -- initialize our window.
     initGL
     -- start event processing engine
     forever $ do
       drawScene rt rq
       GLFW.swapBuffers

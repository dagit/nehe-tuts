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
import Control.Concurrent ( threadDelay )

tincrement, qincrement :: GLfloat
tincrement = 0.2
qincrement = -0.15

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
  glLoadIdentity  -- reset view

  glTranslatef (-1.5) 0 (-6.0) --Move left 1.5 Units and into the screen 6.0
  rt <- readIORef rtri
  glRotatef rt 0 1 0 -- Rotate the triangle on the Y axis

  -- draw a triangle (in smooth coloring mode)
  glBegin gl_TRIANGLES  -- start drawing a polygon
  -- first the front
  glColor3f    1    0    0  -- set The color to Red
  glVertex3f   0    1    0  -- top of triangle (front)
  glColor3f    0    1    0  -- set The color to Green
  glVertex3f (-1) (-1)   1  -- left of triangle (front)
  glColor3f    0    0    1  -- set The color to Blue
  glVertex3f   1  (-1)   1  -- right of triangle (front)
  -- now the right
  glColor3f    1    0    0  -- set The color to Red
  glVertex3f   0    1    0  -- top of triangle (right)
  glColor3f    0    0    1  -- set The color to Blue
  glVertex3f   1  (-1)   1  -- left of triangle (right)
  glColor3f    0    1    0  -- set The color to Green
  glVertex3f   1  (-1) (-1) -- right of triangle (front)
  -- now the back
  glColor3f    1    0    0  -- set The color to Red
  glVertex3f   0    1    0  -- top of triangle (back)
  glColor3f    0    1    0  -- set The color to Green
  glVertex3f   1  (-1) (-1) -- left of triangle (back)
  glColor3f    0    0    1  -- set The color to Blue
  glVertex3f (-1) (-1) (-1) -- right of triangle (back)
  -- now the left
  glColor3f    1    0    0  -- set The color to Red
  glVertex3f   0    1    0  -- top of triangle (left)
  glColor3f    0    0    1  -- set The color to Blue
  glVertex3f (-1) (-1) (-1) -- left of triangle (left)
  glColor3f    0    1    0  -- set The color to Green
  glVertex3f (-1) (-1)   1  -- right of triangle (left)
  glEnd
       
  glLoadIdentity
  glTranslatef 1.5 0 (-7)  -- move right three units
  rq <- readIORef rquad
  glRotatef rq 1 1 1 -- rotate the quad on the x axis

  glBegin gl_QUADS -- start drawing a polygon (4 sided)
  -- first the top
  glColor3f    0    1    0  -- set color to green
  glVertex3f   1    1  (-1) -- top right of quad (Top)
  glVertex3f (-1)   1  (-1) -- top left of quad (Top)
  glVertex3f (-1)   1    1  -- bottom left of quad (Top)
  glVertex3f   1    1    1  -- bottom right of quad (Top)
  -- now the bottom
  glColor3f    1  0.5    0  -- set color to orage
  glVertex3f   1  (-1)   1  -- top right of quad (Bottom)
  glVertex3f (-1) (-1)   1  -- top left of quad (Bottom)
  glVertex3f (-1) (-1) (-1) -- bottom left of quad (Bottom)
  glVertex3f   1  (-1) (-1) -- bottom right of quad (Bottom)
  -- now the front
  glColor3f    1    0    0  -- set color to red
  glVertex3f   1    1    1  -- top right of quad (Bottom)
  glVertex3f (-1)   1    1  -- top left of quad (Bottom)
  glVertex3f (-1) (-1)   1  -- bottom left of quad (Bottom)
  glVertex3f   1  (-1)   1  -- bottom right of quad (Bottom)
  -- now the back
  glColor3f    1    1    0  -- set color to yellow
  glVertex3f   1  (-1) (-1) -- bottom left of quad (Back)
  glVertex3f (-1) (-1) (-1) -- bottom right of quad (Back)
  glVertex3f (-1)   1  (-1) -- top right of quad (Back)
  glVertex3f   1    1  (-1) -- top left of quad (Back)
  -- now the left
  glColor3f    0    0    1  -- set color to blue
  glVertex3f (-1)   1    1  -- top right of quad (Left)
  glVertex3f (-1)   1  (-1) -- top left of quad (Left)
  glVertex3f (-1) (-1) (-1) -- bottom left of quad (Left)
  glVertex3f (-1) (-1)   1  -- bottom right of quad (Left)
  -- now the right
  glColor3f    1    0    1  -- set color to violet
  glVertex3f   1    1  (-1) -- top right of quad (Right)
  glVertex3f   1    1    1  -- top left of quad (Right)
  glVertex3f   1  (-1)   1  -- bottom left of quad (Right)
  glVertex3f   1  (-1) (-1) -- bottom right of quad (Right)
  glEnd
  
  --increase the rotation angle for the triangle
  writeIORef rtri  $! rt + tincrement
  --increase the rotation angle for the quad
  writeIORef rquad $! rq + qincrement

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
       threadDelay 1
       drawScene rt rq
       GLFW.swapBuffers

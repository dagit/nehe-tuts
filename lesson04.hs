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

initGL :: GLFW.Window -> IO ()
initGL win = do
  glShadeModel gl_SMOOTH -- enables smooth color shading
  glClearColor 0 0 0 0 -- Clear the background color to black
  glClearDepth 1 -- enables clearing of the depth buffer
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL  -- type of depth test
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST
  (w,h) <- GLFW.getFramebufferSize win
  resizeScene win w h

resizeScene :: GLFW.WindowSizeCallback
resizeScene win w     0      = resizeScene win w 1 -- prevent divide by zero
resizeScene _   width height = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 100
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

drawScene :: IORef GLfloat -> IORef GLfloat -> GLFW.Window -> IO ()
drawScene rtri rquad _ = do
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
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

keyPressed :: GLFW.KeyCallback
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _   _               _ _                     _ = return ()

main :: IO ()
main = do
     True <- GLFW.init
     -- select type of display mode:
     -- Double buffer
     -- RGBA color
     -- Alpha components supported
     -- Depth buffer
     GLFW.defaultWindowHints
     -- open a window
     Just win <- GLFW.createWindow 800 600 "Lesson 4" Nothing Nothing
     GLFW.makeContextCurrent (Just win)
     -- register the function to do all our OpenGL drawing
     rt <- newIORef 0
     rq <- newIORef 0
     GLFW.setWindowRefreshCallback win (Just (drawScene rt rq))
     -- register the funciton called when our window is resized
     GLFW.setFramebufferSizeCallback win (Just resizeScene)
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback win (Just keyPressed)
     GLFW.setWindowCloseCallback win (Just shutdown)
     -- initialize our window.
     initGL win
     -- start event processing engine
     forever $ do
       GLFW.pollEvents
       drawScene rt rq win
       GLFW.swapBuffers win

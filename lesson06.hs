--
-- This code was created by Jeff Molofee '99 (ported to Haskell GHC 2005)
--

module Main where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.Raw ( glClearColor, glClearDepth
                                     , glEnable, gl_DEPTH_TEST
                                     , glDepthFunc, gl_LEQUAL
                                     , glHint, gl_PERSPECTIVE_CORRECTION_HINT
                                     , gl_NICEST, glViewport, glFlush
                                     , glClear, gl_COLOR_BUFFER_BIT
                                     , gl_DEPTH_BUFFER_BIT, glLoadIdentity
                                     , glTranslatef, glBegin
                                     , glVertex3f, glEnd, gl_QUADS
                                     , glShadeModel, gl_SMOOTH, gl_PROJECTION
                                     , glMatrixMode, gl_MODELVIEW, GLfloat
                                     , glRotatef, glTexCoord2f
                                     , glTexImage2D, gl_TEXTURE_2D, gl_RGB
                                     , gl_UNSIGNED_BYTE, glTexParameteri
                                     , gl_TEXTURE_MIN_FILTER, gl_LINEAR
                                     , gl_TEXTURE_MAG_FILTER, GLuint
                                     , glGenTextures, glBindTexture
                                     )
import Graphics.Rendering.GLU.Raw ( gluPerspective )
import Data.Bits ( (.|.) )
import System.Exit ( exitWith, ExitCode(..) )
import Control.Monad ( forever )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Foreign ( withForeignPtr, plusPtr, mallocForeignPtr, peek )
import qualified Data.ByteString.Internal as BSI
import Control.Concurrent ( threadDelay )
import Util ( Image(..), bitmapLoad )

initGL :: IO GLuint
initGL = do
  glEnable gl_TEXTURE_2D
  glShadeModel gl_SMOOTH
  glClearColor 0 0 0 0
  glClearDepth 1
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST
  loadGLTextures

loadGLTextures :: IO GLuint
loadGLTextures = do
  Just (Image w h pd) <- bitmapLoad "Data/NeHe.bmp"
  putStrLn $ "Image width  = " ++ show w
  putStrLn $ "Image height = " ++ show h
  tex <- mallocForeignPtr >>= \ptr ->
           withForeignPtr ptr $ \p -> do
    glGenTextures 1 p
    peek p :: IO GLuint
  let (ptr, off, _) = BSI.toForeignPtr pd
  withForeignPtr ptr $ \p -> do
    let p' = p `plusPtr` off
    glBindTexture gl_TEXTURE_2D tex
    glTexImage2D gl_TEXTURE_2D 0 3
      (fromIntegral w) (fromIntegral h) 0 gl_RGB gl_UNSIGNED_BYTE
      p'
    let glLinear = fromIntegral gl_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER glLinear
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER glLinear
  return tex

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

drawScene :: GLuint -> IORef GLfloat -> IORef GLfloat 
          -> IORef GLfloat -> IO ()
drawScene tex xrot yrot zrot = do
  -- clear the screen and the depth buffer
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity -- reset view

  glTranslatef 0 0 (-5.0) --Move left 5 Units into the screen

  xr <- readIORef xrot
  yr <- readIORef yrot
  zr <- readIORef zrot
  glRotatef xr 1 0 0 -- Rotate the triangle on the Y axis
  glRotatef yr 0 1 0 -- Rotate the triangle on the Y axis
  glRotatef zr 0 0 1 -- Rotate the triangle on the Y axis

  glBindTexture gl_TEXTURE_2D tex

  glBegin gl_QUADS -- start drawing a polygon (4 sided)
  -- first the front
  glTexCoord2f   0    0
  glVertex3f   (-1) (-1)    1  -- bottom left of quad (Front)
  glTexCoord2f   1    0
  glVertex3f     1  (-1)    1  -- bottom right of quad (Front)
  glTexCoord2f   1    1 
  glVertex3f     1    1     1  -- top right of quad (Front)
  glTexCoord2f   0    1 
  glVertex3f   (-1)   1     1  -- top left of quad (Front)
  -- now the back
  glTexCoord2f   1    0 
  glVertex3f   (-1) (-1) (-1)  -- bottom right of quad (Back)
  glTexCoord2f   1    1 
  glVertex3f   (-1)   1  (-1)  -- top right of quad (Back)
  glTexCoord2f   0    1 
  glVertex3f     1    1  (-1)  -- top left of quad (Back)
  glTexCoord2f   0    0 
  glVertex3f     1  (-1) (-1)  -- bottom left of quad (Back)
  -- now the top
  glTexCoord2f   0    1
  glVertex3f   (-1)   1  (-1)  -- top left of quad (Top)
  glTexCoord2f   0    0  
  glVertex3f   (-1)   1    1   -- bottom left of quad (Top)
  glTexCoord2f   1    0  
  glVertex3f     1    1    1   -- bottom right of quad (Top)
  glTexCoord2f   1    1  
  glVertex3f     1    1  (-1)  -- top right of quad (Top)
  -- now the bottom
  glTexCoord2f   1    1  
  glVertex3f     1  (-1)   1   -- top right of quad (Bottom)
  glTexCoord2f   0    1  
  glVertex3f   (-1) (-1)   1   -- top left of quad (Bottom)
  glTexCoord2f   0    0 
  glVertex3f   (-1) (-1) (-1)  -- bottom left of quad (Bottom)
  glTexCoord2f   1    0  
  glVertex3f     1  (-1) (-1)  -- bottom right of quad (Bottom)
  -- now the right
  glTexCoord2f   1    0  
  glVertex3f     1  (-1) (-1)  -- bottom right of quad (Right)
  glTexCoord2f   1    1  
  glVertex3f     1    1  (-1)  -- top right of quad (Right)
  glTexCoord2f   0    1  
  glVertex3f     1    1    1   -- top left of quad (Right)
  glTexCoord2f   0    0  
  glVertex3f     1  (-1)   1   -- bottom left of quad (Right)
  -- now the left
  glTexCoord2f   0    0  
  glVertex3f   (-1) (-1) (-1)  -- bottom left of quad (Left)
  glTexCoord2f   1    0  
  glVertex3f   (-1)   1  (-1)  -- top left of quad (Left)
  glTexCoord2f   1    1  
  glVertex3f   (-1)   1    1   -- top right of quad (Left)
  glTexCoord2f   0    1 
  glVertex3f   (-1) (-1)   1   -- bottom right of quad (Left)
  
  glEnd
  
  writeIORef xrot $! xr + 0.3
  writeIORef yrot $! yr + 0.2
  writeIORef zrot $! zr + 0.4

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
     xrot <- newIORef 0
     yrot <- newIORef 0
     zrot <- newIORef 0
     tex  <- initGL
     GLFW.setWindowRefreshCallback (drawScene tex xrot yrot zrot)
     -- register the funciton called when our window is resized
     GLFW.setWindowSizeCallback resizeScene
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback keyPressed
     GLFW.setWindowCloseCallback shutdown
     -- initialize our window.
     -- start event processing engine
     forever $ do
       threadDelay 1
       drawScene tex xrot yrot zrot
       GLFW.swapBuffers

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
import Foreign ( withForeignPtr, plusPtr, peek, alloca )
import qualified Data.ByteString.Internal as BSI
import Util ( Image(..), bitmapLoad )
import Paths_nehe_tuts

initGL :: GLFW.Window -> IO GLuint
initGL win = do
  glEnable gl_TEXTURE_2D
  glShadeModel gl_SMOOTH
  glClearColor 0 0 0 0
  glClearDepth 1
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST
  (w,h) <- GLFW.getFramebufferSize win
  resizeScene win w h
  loadGLTextures

loadGLTextures :: IO GLuint
loadGLTextures = do
  fp <- getDataFileName "NeHe.bmp"
  putStrLn $ "loading texture: " ++ fp
  Just (Image w h pd) <- bitmapLoad fp
  putStrLn $ "Image width  = " ++ show w
  putStrLn $ "Image height = " ++ show h
  tex <- alloca $ \p -> do
    glGenTextures 1 p
    peek p
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
resizeScene win w     0      = resizeScene win w 1 -- prevent divide by zero
resizeScene _   width height = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 100
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

drawScene :: GLuint -> IORef GLfloat -> IORef GLfloat 
          -> IORef GLfloat -> GLFW.Window -> IO ()
drawScene tex xrot yrot zrot _ = do
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
     Just win <- GLFW.createWindow 800 600 "Lesson 6" Nothing Nothing
     GLFW.makeContextCurrent (Just win)
     -- register the function to do all our OpenGL drawing
     xrot <- newIORef 0
     yrot <- newIORef 0
     zrot <- newIORef 0
     tex  <- initGL win
     GLFW.setWindowRefreshCallback win (Just (drawScene tex xrot yrot zrot))
     -- register the funciton called when our window is resized
     GLFW.setFramebufferSizeCallback win (Just resizeScene)
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback win (Just keyPressed)
     GLFW.setWindowCloseCallback win (Just shutdown)
     -- initialize our window.
     -- start event processing engine
     forever $ do
       GLFW.pollEvents
       drawScene tex xrot yrot zrot win
       GLFW.swapBuffers win

--
-- This code was created by Jeff Molofee '99 (ported to Haskell GHC 2005)
--

module Main where

import qualified Graphics.UI.GLFW as GLFW
-- everything from here starts with gl or GL
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw ( gluPerspective, gluBuild2DMipmaps )
import Data.Bits ( (.|.) )
import System.Exit ( exitWith, ExitCode(..) )
import Control.Monad ( forever )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Foreign ( withForeignPtr, plusPtr
               , ForeignPtr, newForeignPtr_ )
import Foreign.Storable ( Storable )
import Foreign.Marshal.Array ( newArray, allocaArray, peekArray )
import qualified Data.ByteString.Internal as BSI
import Util ( Image(..), bitmapLoad )
import Paths_nehe_tuts

newArray' :: Storable a => [a] -> IO (ForeignPtr a)
newArray' xs = (newArray xs) >>= newForeignPtr_

glLightfv' :: GLenum -> GLenum -> ForeignPtr GLfloat -> IO ()
glLightfv' l a fp =
  withForeignPtr fp $ glLightfv l a

initGL :: GLFW.Window -> IO [GLuint]
initGL win = do
  glEnable gl_TEXTURE_2D
  glShadeModel gl_SMOOTH
  glClearColor 0 0 0 0.5
  glClearDepth 1
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST
  lightAmbient  <- newArray' [0.5, 0.5, 0.5, 1.0] 
  lightDiffuse  <- newArray' [1.0, 1.0, 1.0, 1.0]
  lightPosition <- newArray' [0.0, 0.0, 2.0, 1.0]
  glLightfv' gl_LIGHT1 gl_AMBIENT  lightAmbient
  glLightfv' gl_LIGHT1 gl_DIFFUSE  lightDiffuse
  glLightfv' gl_LIGHT1 gl_POSITION lightPosition
  glEnable gl_LIGHT1
  glBlendFunc gl_SRC_ALPHA gl_ONE
  (w,h) <- GLFW.getFramebufferSize win
  resizeScene win w h
  loadGLTextures

loadGLTextures :: IO [GLuint]
loadGLTextures = do
  fp <- getDataFileName "glass.bmp"
  Just (Image w h pd) <- bitmapLoad fp
  let numTextures = 3
  texs <- allocaArray numTextures $ \p -> do
            glGenTextures (fromIntegral numTextures) p
            peekArray numTextures p
  let (ptr, off, _) = BSI.toForeignPtr pd
  _ <- withForeignPtr ptr $ \p -> do
    let p' = p `plusPtr` off
        glNearest = fromIntegral gl_NEAREST
        glLinear  = fromIntegral gl_LINEAR
    -- create nearest filtered texture
    glBindTexture gl_TEXTURE_2D (texs!!0)
    glTexImage2D gl_TEXTURE_2D 0 3
      (fromIntegral w) (fromIntegral h)
      0 gl_RGB gl_UNSIGNED_BYTE p'
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER glNearest
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER glNearest
    -- create linear filtered texture
    glBindTexture gl_TEXTURE_2D (texs!!1)
    glTexImage2D gl_TEXTURE_2D 0 3
      (fromIntegral w) (fromIntegral h)
      0 gl_RGB gl_UNSIGNED_BYTE p'
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER glLinear
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER glLinear
    -- create mipmap filtered texture
    glBindTexture gl_TEXTURE_2D (texs!!2)
    glTexImage2D gl_TEXTURE_2D 0 3
      (fromIntegral w) (fromIntegral h)
      0 gl_RGB gl_UNSIGNED_BYTE p'
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER glLinear 
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER
      (fromIntegral gl_LINEAR_MIPMAP_NEAREST)
    gluBuild2DMipmaps gl_TEXTURE_2D 3 (fromIntegral w)
      (fromIntegral h) gl_RGB gl_UNSIGNED_BYTE p'
  return texs

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

drawScene :: [GLuint] -> IORef GLfloat -> IORef GLfloat
          -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat 
          -> IORef Int -> GLFW.Window -> IO ()
drawScene texs xrot yrot xspeed yspeed zdepth filt _ = do
  -- clear the screen and the depth buffer
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity -- reset view

  glTranslatef 0 0 (-5.0) --Move left 5 Units into the screen

  zd <- readIORef zdepth
  glTranslatef 0 0 zd --Move left 5 Units into the screen

  xr <- readIORef xrot
  yr <- readIORef yrot
  glRotatef xr 1 0 0 -- Rotate the triangle on the Y axis
  glRotatef yr 0 1 0 -- Rotate the triangle on the Y axis
  f <- readIORef filt
  glBindTexture gl_TEXTURE_2D (texs!!f)
  
  glBegin gl_QUADS -- start drawing a polygon (4 sided)
  -- first the front
  glNormal3f     0    0    1
  glTexCoord2f   0    0 
  glVertex3f   (-1) (-1)   1  -- bottom left of quad (Front)
  glTexCoord2f   1    0 
  glVertex3f     1  (-1)   1  -- bottom right of quad (Front)
  glTexCoord2f   1    1 
  glVertex3f     1    1    1  -- top right of quad (Front)
  glTexCoord2f   0    1 
  glVertex3f   (-1)   1    1  -- top left of quad (Front)
  -- now the back
  glNormal3f     0    0  (-1)
  glTexCoord2f   1    0 
  glVertex3f   (-1) (-1) (-1) -- bottom right of quad (Back)
  glTexCoord2f   1    1 
  glVertex3f   (-1)   1  (-1) -- top right of quad (Back)
  glTexCoord2f   0    1 
  glVertex3f     1    1  (-1) -- top left of quad (Back)
  glTexCoord2f   0    0 
  glVertex3f     1  (-1) (-1) -- bottom left of quad (Back)
  -- now the top
  glNormal3f     0    1    0
  glTexCoord2f   0    1
  glVertex3f   (-1)   1  (-1) -- top left of quad (Top)
  glTexCoord2f   0    0  
  glVertex3f   (-1)   1    1  -- bottom left of quad (Top)
  glTexCoord2f   1    0  
  glVertex3f     1    1    1  -- bottom right of quad (Top)
  glTexCoord2f   1    1  
  glVertex3f     1    1  (-1) -- top right of quad (Top)
  -- now the bottom
  glNormal3f     0  (-1)   0
  glTexCoord2f   1    1  
  glVertex3f     1  (-1)   1  -- top right of quad (Bottom)
  glTexCoord2f   0    1  
  glVertex3f   (-1) (-1)   1  -- top left of quad (Bottom)
  glTexCoord2f   0    0  
  glVertex3f   (-1) (-1) (-1) -- bottom left of quad (Bottom)
  glTexCoord2f   1    0  
  glVertex3f     1  (-1) (-1) -- bottom right of quad (Bottom)
  -- now the right
  glNormal3f     1    0    0
  glTexCoord2f   1    0  
  glVertex3f     1  (-1) (-1) -- bottom right of quad (Right)
  glTexCoord2f   1    1  
  glVertex3f     1    1  (-1) -- top right of quad (Right)
  glTexCoord2f   0    1  
  glVertex3f     1    1    1  -- top left of quad (Right)
  glTexCoord2f   0    0
  glVertex3f     1  (-1)   1  -- bottom left of quad (Right)
  -- now the left
  glNormal3f   (-1)   0    1
  glTexCoord2f   0    0  
  glVertex3f   (-1) (-1) (-1) -- bottom left of quad (Left)
  glTexCoord2f   1    0  
  glVertex3f   (-1)   1  (-1) -- top left of quad (Left)
  glTexCoord2f   1    1  
  glVertex3f   (-1)   1    1  -- top right of quad (Left)
  glTexCoord2f   0    1  
  glVertex3f   (-1) (-1)   1  -- bottom right of quad (Left)

  glEnd

  xsp <- readIORef xspeed
  ysp <- readIORef yspeed  
  writeIORef xrot $! xr + xsp
  writeIORef yrot $! yr + ysp
  
  glFlush

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

keyPressed :: IORef Bool -> IORef Bool -> IORef Int -> IORef GLfloat
           -> IORef GLfloat -> IORef GLfloat -> GLFW.KeyCallback
keyPressed _ _ _ _ _ _ win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed l _ _ _ _ _ _   GLFW.Key'L      _ GLFW.KeyState'Pressed _ = do
  le <- readIORef l
  if le == True
    then glEnable  gl_LIGHTING
    else glDisable gl_LIGHTING
  writeIORef l $! not le
keyPressed _ _ filt _ _ _ _ GLFW.Key'F _ GLFW.KeyState'Pressed _ = do
  f <- readIORef filt
  writeIORef filt $! (f + 1) `mod` 3
keyPressed _ b _ _ _ _ _ GLFW.Key'B _ GLFW.KeyState'Pressed _ = do
  bp <- readIORef b
  if bp == True
    then glEnable  gl_BLEND >> glDisable gl_DEPTH_TEST
    else glDisable gl_BLEND >> glEnable  gl_DEPTH_TEST
  writeIORef b $! not bp
keyPressed _ _ _ zdepth _ _ _ GLFW.Key'PageUp _ GLFW.KeyState'Pressed _ = do
  zd <- readIORef zdepth
  writeIORef zdepth $! zd - 0.2
keyPressed _ _ _ zdepth _ _ _ GLFW.Key'PageDown _ GLFW.KeyState'Pressed _ = do
  zd <- readIORef zdepth
  writeIORef zdepth $! zd + 0.2
keyPressed _ _ _ _ xspeed _ _ GLFW.Key'Up _ GLFW.KeyState'Pressed _ = do
  xs <- readIORef xspeed
  writeIORef xspeed $! xs - 0.1
keyPressed _ _ _ _ xspeed _ _ GLFW.Key'Down _ GLFW.KeyState'Pressed _ = do
  xs <- readIORef xspeed
  writeIORef xspeed $! xs + 0.1
keyPressed _ _ _ _ _ yspeed _ GLFW.Key'Right _ GLFW.KeyState'Pressed _ = do
  xs <- readIORef yspeed
  writeIORef yspeed $! xs + 0.1
keyPressed _ _ _ _ _ yspeed _ GLFW.Key'Left _ GLFW.KeyState'Pressed _ = do
  ys <- readIORef yspeed
  writeIORef yspeed $! ys - 0.1
keyPressed _ _ _ _ _ _ _ _ _ _ _ = return ()

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
     Just win <- GLFW.createWindow 800 600 "Lesson 8" Nothing Nothing
     GLFW.makeContextCurrent (Just win)
     -- window starts at upper left corner of the screen
     lighting <- newIORef True
     blending <- newIORef True
     xrot     <- newIORef (0::GLfloat)
     yrot     <- newIORef (0::GLfloat)
     xspeed   <- newIORef (0::GLfloat)
     yspeed   <- newIORef (0::GLfloat)
     zdepth   <- newIORef (-5.0 :: GLfloat)
     filt     <- newIORef (0::Int)
     -- initialize our window.
     texs <- initGL win
     GLFW.setWindowRefreshCallback win $
       Just (drawScene texs xrot yrot xspeed yspeed zdepth filt)
     -- register the funciton called when our window is resized
     GLFW.setFramebufferSizeCallback win (Just resizeScene)
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback win $
       Just (keyPressed lighting blending filt zdepth xspeed yspeed)
     GLFW.setWindowCloseCallback win (Just shutdown)
     forever $ do
       GLFW.pollEvents
       drawScene texs xrot yrot xspeed yspeed zdepth filt win
       GLFW.swapBuffers win

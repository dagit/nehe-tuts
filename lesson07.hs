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
import System.Directory ( getCurrentDirectory, setCurrentDirectory )
import Util ( Image(..), bitmapLoad )

newArray' :: Storable a => [a] -> IO (ForeignPtr a)
newArray' xs = (newArray xs) >>= newForeignPtr_

glLightfv' :: GLenum -> GLenum -> ForeignPtr GLfloat -> IO ()
glLightfv' l a fp =
  withForeignPtr fp $ glLightfv l a

initGL :: IO [GLuint]
initGL = do
  glEnable gl_TEXTURE_2D
  glShadeModel gl_SMOOTH
  glClearColor 0 0 0 0
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
  loadGLTextures

loadGLTextures :: IO [GLuint]
loadGLTextures = do
  Just (Image w h pd) <- bitmapLoad "Data/Crate.bmp"
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
resizeScene w     0      = resizeScene w 1 -- prevent divide by zero
resizeScene width height = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 100
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

drawScene :: [GLuint] -> IORef GLfloat -> IORef GLfloat
          -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat 
          -> IORef Int -> IO ()
drawScene texs xrot yrot xspeed yspeed zdepth filt = do
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
shutdown = do
  GLFW.closeWindow
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return True

keyPressed :: IORef Bool -> IORef Int -> IORef GLfloat
           -> IORef GLfloat -> IORef GLfloat -> GLFW.KeyCallback
keyPressed _ _ _ _ _ GLFW.KeyEsc   True = shutdown >> return ()
keyPressed l _ _ _ _ (GLFW.CharKey 'L') True = do
  le <- readIORef l
  if le == True
    then glEnable  gl_LIGHTING
    else glDisable gl_LIGHTING
  writeIORef l $! not le
keyPressed _ filt _ _ _ (GLFW.CharKey 'F') True = do
  f <- readIORef filt
  writeIORef filt $! (f + 1) `mod` 3
keyPressed l f zd xs ys (GLFW.CharKey 'l') d =
  keyPressed l f zd xs ys (GLFW.CharKey 'L') d
keyPressed l f zd xs ys (GLFW.CharKey 'f') d =
  keyPressed l f zd xs ys (GLFW.CharKey 'F') d
keyPressed _ _ zdepth _ _ GLFW.KeyPageup True = do
  zd <- readIORef zdepth
  writeIORef zdepth $! zd - 0.2
keyPressed _ _ zdepth _ _ GLFW.KeyPagedown True = do
  zd <- readIORef zdepth
  writeIORef zdepth $! zd + 0.2
keyPressed _ _ _ xspeed _ GLFW.KeyUp True = do
  xs <- readIORef xspeed
  writeIORef xspeed $! xs - 0.1
keyPressed _ _ _ xspeed _ GLFW.KeyDown True = do
  xs <- readIORef xspeed
  writeIORef xspeed $! xs + 0.1
keyPressed _ _ _ _ yspeed GLFW.KeyRight True = do
  xs <- readIORef yspeed
  writeIORef yspeed $! xs + 0.1
keyPressed _ _ _ _ yspeed GLFW.KeyLeft True = do
  ys <- readIORef yspeed
  writeIORef yspeed $! ys - 0.1
keyPressed _ _ _ _ _ _ _ = return ()

main :: IO ()
main = do
     cd <- getCurrentDirectory
     True <- GLFW.initialize
     setCurrentDirectory cd
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
     lighting <- newIORef True
     xrot     <- newIORef (0::GLfloat)
     yrot     <- newIORef (0::GLfloat)
     xspeed   <- newIORef (0::GLfloat)
     yspeed   <- newIORef (0::GLfloat)
     zdepth   <- newIORef (-5.0 :: GLfloat)
     filt     <- newIORef (0::Int)
     -- initialize our window.
     texs <- initGL
     GLFW.setWindowRefreshCallback
       (drawScene texs xrot yrot xspeed yspeed zdepth filt)
     -- register the funciton called when our window is resized
     GLFW.setWindowSizeCallback resizeScene
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback (keyPressed lighting filt zdepth xspeed yspeed)
     GLFW.setWindowCloseCallback shutdown
     forever $ do
       GLFW.pollEvents 
       drawScene texs xrot yrot xspeed yspeed zdepth filt
       GLFW.swapBuffers

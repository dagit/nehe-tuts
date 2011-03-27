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
import Control.Monad ( forever, forM_ )
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef )
import Foreign ( withForeignPtr, plusPtr, alloca, peek )
import qualified Data.ByteString.Internal as BSI
import Util ( Image(..), bitmapLoad )

boxcol :: [(GLfloat, GLfloat, GLfloat)]
boxcol = [(1, 0, 0), (1, 0.5, 0), (1, 1, 0), 
          (0, 1, 0), (0, 1, 1)]
topcol :: [(GLfloat, GLfloat, GLfloat)]
topcol = [(0.5, 0, 0), (0.5, 0.25, 0), (0.5, 0.5, 0),
          (0, 0.5, 0), (0, 0.5, 0.5)]

buildLists :: IO (GLuint, GLuint)
buildLists = do
  box <- glGenLists 2
  glNewList box gl_COMPILE
  glBegin gl_QUADS
  glTexCoord2f 1   1   >> glVertex3f   (-1) (-1)   (-1)   -- Top Right Of The Texture and Quad
  glTexCoord2f 0.0 1.0 >> glVertex3f   1.0  (-1.0) (-1.0) -- Top Left Of The Texture and Quad
  glTexCoord2f 0.0 0.0 >> glVertex3f   1.0  (-1.0)  1.0   -- Bottom Left Of The Texture and Quad
  glTexCoord2f 1.0 0.0 >> glVertex3f (-1.0) (-1.0)  1.0   -- Bottom Right Of The Texture and Quad
  -- Front Face
  glTexCoord2f 0.0 0.0 >> glVertex3f (-1.0) (-1.0)  1.0   -- Bottom Left Of The Texture and Quad
  glTexCoord2f 1.0 0.0 >> glVertex3f   1.0  (-1.0)  1.0   -- Bottom Right Of The Texture and Quad
  glTexCoord2f 1.0 1.0 >> glVertex3f   1.0    1.0   1.0   -- Top Right Of The Texture and Quad
  glTexCoord2f 0.0 1.0 >> glVertex3f (-1.0)   1.0   1.0   -- Top Left Of The Texture and Quad
  -- Back Face
  glTexCoord2f 1.0 0.0 >> glVertex3f (-1.0) (-1.0) (-1.0) -- Bottom Right Of The Texture and Quad
  glTexCoord2f 1.0 1.0 >> glVertex3f (-1.0)   1.0  (-1.0) -- Top Right Of The Texture and Quad
  glTexCoord2f 0.0 1.0 >> glVertex3f   1.0    1.0  (-1.0) -- Top Left Of The Texture and Quad
  glTexCoord2f 0.0 0.0 >> glVertex3f   1.0  (-1.0) (-1.0) -- Bottom Left Of The Texture and Quad
  -- Right face
  glTexCoord2f 1.0 0.0 >> glVertex3f   1.0  (-1.0) (-1.0) -- Bottom Right Of The Texture and Quad
  glTexCoord2f 1.0 1.0 >> glVertex3f   1.0    1.0  (-1.0) -- Top Right Of The Texture and Quad
  glTexCoord2f 0.0 1.0 >> glVertex3f   1.0    1.0    1.0  -- Top Left Of The Texture and Quad
  glTexCoord2f 0.0 0.0 >> glVertex3f   1.0  (-1.0)   1.0  -- Bottom Left Of The Texture and Quad
  -- Left Face
  glTexCoord2f 0.0 0.0 >> glVertex3f (-1.0) (-1.0) (-1.0) -- Bottom Left Of The Texture and Quad
  glTexCoord2f 1.0 0.0 >> glVertex3f (-1.0) (-1.0)   1.0  -- Bottom Right Of The Texture and Quad
  glTexCoord2f 1.0 1.0 >> glVertex3f (-1.0)   1.0    1.0  -- Top Right Of The Texture and Quad
  glTexCoord2f 0.0 1.0 >> glVertex3f (-1.0)   1.0  (-1.0) -- Top Left Of The Texture and Quad

  glEndList
  let top = box + 1
  glNewList top gl_COMPILE
  glBegin gl_QUADS
  glTexCoord2f 0 1 >> glVertex3f (-1) 1 (-1)
  glTexCoord2f 0 0 >> glVertex3f (-1) 1   1
  glTexCoord2f 1 0 >> glVertex3f   1  1   1
  glTexCoord2f 1 1 >> glVertex3f   1  1 (-1)
  glEnd
  glEndList
  return (box, top)

initGL :: IO GLuint
initGL = do
  tex <- loadTextures
  glEnable gl_TEXTURE_2D
  glShadeModel gl_SMOOTH
  glClearColor 0 0 0 0.5
  glClearDepth 1
  glEnable gl_DEPTH_TEST
  glEnable gl_LEQUAL
  glEnable gl_LIGHT0
  glEnable gl_LIGHTING
  glEnable gl_COLOR_MATERIAL
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST
  return tex

loadTextures :: IO GLuint
loadTextures = do
  Just (Image w h pd) <- bitmapLoad "Data/cube.bmp"
  putStrLn $ "Image w = " ++ show w
  putStrLn $ "Image h = " ++ show h
  tex <- alloca $ \p -> do
            glGenTextures 1 p
            peek p
  let (ptr, off, _) = BSI.toForeignPtr pd
  _ <- withForeignPtr ptr $ \p -> do
    let p' = p `plusPtr` off
        glNearest  = fromIntegral gl_NEAREST
    -- create linear filtered texture
    glBindTexture gl_TEXTURE_2D tex
    glTexImage2D gl_TEXTURE_2D 0 3
      (fromIntegral w) (fromIntegral h)
      0 gl_RGB gl_UNSIGNED_BYTE p'
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER glNearest
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER glNearest
  return tex

shutdown :: GLFW.WindowCloseCallback
shutdown = do
  GLFW.closeWindow
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return True

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
          -> GLuint -> GLuint -> IO ()
drawScene tex xrot yrot box top = do
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glBindTexture gl_TEXTURE_2D tex

  xr <- readIORef xrot
  yr <- readIORef yrot

  forM_ [(x,y) | y <- [1..5], x <- [0..y-1] ] $ \(x,y) -> do
    glLoadIdentity
    let x' = fromIntegral x
        y' = fromIntegral y
        color (r,g,b) = glColor3f r g b
    glTranslatef (1.4+x'*2.8-y'*1.4) (((6-y')*2.4)-7)  (-20)
    glRotatef (45.0-(2.0*y')+xr) 1 0 0
    glRotatef (45-yr) 0 1 0
    color (boxcol !! (y-1))
    glCallList box
    color (topcol !! (y-1))
    glCallList top 
  glFlush

keyPressed :: IORef GLfloat -> IORef GLfloat -> GLFW.KeyCallback
keyPressed _    _    GLFW.KeyEsc   True = shutdown >> return ()
keyPressed xrot _    GLFW.KeyUp    True = modifyIORef xrot (subtract 0.8)
keyPressed xrot _    GLFW.KeyDown  True = modifyIORef xrot (+0.8)
keyPressed _    yrot GLFW.KeyLeft  True = modifyIORef yrot (subtract 0.8)
keyPressed _    yrot GLFW.KeyRight True = modifyIORef yrot (+0.8)
keyPressed _    _    _             _    = return ()

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
                     , GLFW.displayOptions_numDepthBits = 24
                     -- , GLFW.displayOptions_displayMode = GLFW.Fullscreen
                     }
     -- open a window
     True <- GLFW.openWindow dspOpts
     xrot <- newIORef 0
     yrot <- newIORef 0
     
     -- initialize our window.
     tex <- initGL
     (box, top) <- buildLists
     GLFW.setWindowRefreshCallback $
       drawScene tex xrot yrot box top
     GLFW.setWindowSizeCallback resizeScene
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback $
       keyPressed xrot yrot
     GLFW.setWindowCloseCallback shutdown
     GLFW.getWindowRefreshRate >>= print
     forever $ do
       drawScene tex xrot yrot box top
       GLFW.swapBuffers

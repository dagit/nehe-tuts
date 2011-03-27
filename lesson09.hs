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
import Control.Monad ( forever, when, forM, forM_ )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Foreign ( withForeignPtr, plusPtr
               , ForeignPtr, newForeignPtr_
               , alloca, peek )
import Foreign.Storable ( Storable )
import Foreign.Marshal.Array ( newArray )
import qualified Data.ByteString.Internal as BSI
import Util ( Image(..), bitmapLoad )
import System.Random ( getStdRandom, randomR )

data Star = Star { starColor :: !(GLubyte, GLubyte, GLubyte)
                 , starDist  :: !GLfloat 
                 , starAngle :: !GLfloat
                 }
            deriving Show

numStars :: Num a => a
numStars = 50

newArray' :: Storable a => [a] -> IO (ForeignPtr a)
newArray' xs = (newArray xs) >>= newForeignPtr_

glLightfv' :: GLenum -> GLenum -> ForeignPtr GLfloat -> IO ()
glLightfv' l a fp =
  withForeignPtr fp $ glLightfv l a

initGL :: IO GLuint
initGL = do
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
  glEnable gl_BLEND
  loadGLTextures

generateStars :: IO [IORef Star]
generateStars = forM [0 .. numStars -1] $ \i -> do
  r <- getStdRandom (randomR (0, 255)) :: IO Int
  g <- getStdRandom (randomR (0, 255)) :: IO Int
  b <- getStdRandom (randomR (0, 255)) :: IO Int
  newIORef (Star { starAngle = 0
                 , starColor = (fromIntegral r,fromIntegral g,fromIntegral b)
                 , starDist  = (i/numStars)*5})

loadGLTextures :: IO GLuint
loadGLTextures = do
  Just (Image w h pd) <- bitmapLoad "Data/Star.bmp"
  tex <- alloca $ \p -> do
            glGenTextures 1 p
            peek p
  let (ptr, off, _) = BSI.toForeignPtr pd
  _ <- withForeignPtr ptr $ \p -> do
    let p' = p `plusPtr` off
        glLinear  = fromIntegral gl_LINEAR
    -- create linear filtered texture
    glBindTexture gl_TEXTURE_2D tex
    glTexImage2D gl_TEXTURE_2D 0 3
      (fromIntegral w) (fromIntegral h)
      0 gl_RGB gl_UNSIGNED_BYTE p'
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER glLinear
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER glLinear
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

glColor4ub' :: (GLubyte, GLubyte, GLubyte) -> GLubyte -> IO ()
glColor4ub' (r,g,b) a = glColor4ub r g b a

drawScene :: GLuint -> IORef GLfloat -> IORef GLfloat
          -> IORef Bool -> IORef GLfloat -> [IORef Star]
          -> IO ()
drawScene tex zoom tilt twinkle spin stars = do
  -- clear the screen and the depth buffer
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glBindTexture gl_TEXTURE_2D tex

  let starTuples = zip3 stars
                        (reverse stars)
                        [0 .. numStars -1]
  forM_ starTuples $ \(st1, st2, i) -> do
    glLoadIdentity
    s1 <- readIORef st1
    s2 <- readIORef st2
    sp <- readIORef spin
    zo <- readIORef zoom
    ti <- readIORef tilt
    tw <- readIORef twinkle
    glTranslatef 0 0 zo
    glRotatef ti 1 0 0
    glRotatef (starAngle s1) 0 1 0
    glTranslatef (starDist s1) 0 0
    glRotatef (-(starAngle s1)) 0 1 0
    glRotatef (-ti) 1 0 0
    when tw $ do
      glColor4ub' (starColor s2) 255
      glBegin gl_QUADS
      glTexCoord2f 0 0 >> glVertex3f (-1) (-1) 0
      glTexCoord2f 1 0 >> glVertex3f 1 (-1) 0
      glTexCoord2f 1 1 >> glVertex3f 1 1 0
      glTexCoord2f 0 1 >> glVertex3f (-1) 1 0
      glEnd
    glRotatef sp 0 0 1
    glColor4ub' (starColor s1) 255
    glBegin gl_QUADS
    glTexCoord2f 0 0 >> glVertex3f (-1) (-1) 0
    glTexCoord2f 1 0 >> glVertex3f 1 (-1) 0
    glTexCoord2f 1 1 >> glVertex3f 1 1 0
    glTexCoord2f 0 1 >> glVertex3f (-1) 1 0
    glEnd

    writeIORef spin $! sp + 0.01
    if starDist s1 < 0
      then do
        let d = starDist s1 + 5
        r <- getStdRandom (randomR (0, 255)) :: IO Int
        g <- getStdRandom (randomR (0, 255)) :: IO Int
        b <- getStdRandom (randomR (0, 255)) :: IO Int
        writeIORef st1 $! Star
          { starAngle = starAngle s1 + i/numStars
          , starColor = (fromIntegral r,fromIntegral g, fromIntegral b)
          , starDist  = d
          }
      else do
        writeIORef st1 $! Star
          { starAngle = starAngle s1 + i/numStars
          , starColor = starColor s1
          , starDist  = (starDist s1)-0.01 }

  glFlush

shutdown :: GLFW.WindowCloseCallback
shutdown = do
  GLFW.closeWindow
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return True

keyPressed :: IORef Bool -> IORef GLfloat -> IORef GLfloat
           -> GLFW.KeyCallback
keyPressed _ _ _ GLFW.KeyEsc   True = shutdown >> return ()
keyPressed t _ _ (GLFW.CharKey 'T') True = do
  twinkle <- readIORef t
  writeIORef t $! not twinkle
keyPressed tw zo ti (GLFW.CharKey 't') d =
  keyPressed tw zo ti (GLFW.CharKey 'T') d
keyPressed _ zoom _ GLFW.KeyPageup True = do
  zd <- readIORef zoom
  writeIORef zoom $! zd - 0.2
keyPressed _ zoom _ GLFW.KeyPagedown True = do
  zd <- readIORef zoom
  writeIORef zoom $! zd + 0.2
keyPressed _ _ tilt GLFW.KeyUp True = do
  xs <- readIORef tilt 
  writeIORef tilt $! xs - 0.5
keyPressed _ _ tilt GLFW.KeyDown True = do
  xs <- readIORef tilt
  writeIORef tilt $! xs + 0.5
keyPressed _ _ _ _ _ = return ()

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
     twinkle <- newIORef False
     spin    <- newIORef 0
     stars   <- generateStars
     zoom    <- newIORef (-15)
     tilt    <- newIORef 90
     -- initialize our window.
     tex <- initGL
     GLFW.setWindowRefreshCallback
       (drawScene tex zoom tilt twinkle spin stars)
     -- register the funciton called when our window is resized
     GLFW.setWindowSizeCallback resizeScene
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback $
       keyPressed twinkle zoom tilt
     GLFW.setWindowCloseCallback shutdown
     forever $ do
       GLFW.pollEvents 
       drawScene tex zoom tilt twinkle spin stars
       GLFW.swapBuffers

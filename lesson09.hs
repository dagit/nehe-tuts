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
import Paths_nehe_tuts

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

initGL :: GLFW.Window -> IO GLuint
initGL win = do
  glEnable gl_TEXTURE_2D
  glShadeModel gl_SMOOTH
  glClearColor 0 0 0 0.5
  glClearDepth 1
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST
  glBlendFunc gl_SRC_ALPHA gl_ONE
  glEnable gl_BLEND
  (w,h) <- GLFW.getFramebufferSize win
  resizeScene win w h
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
  fp <- getDataFileName "Star.bmp"
  Just (Image w h pd) <- bitmapLoad fp
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
resizeScene win w     0      = resizeScene win w 1 -- prevent divide by zero
resizeScene _   width height = do
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
          -> GLFW.Window -> IO ()
drawScene tex zoom tilt twinkle spin stars _ = do
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
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

keyPressed :: IORef Bool -> IORef GLfloat -> IORef GLfloat
           -> GLFW.KeyCallback
keyPressed _ _    _    win GLFW.Key'Escape  _ GLFW.KeyState'Pressed  _ = shutdown win
keyPressed t _    _    _   GLFW.Key'T       _ GLFW.KeyState'Pressed  _ = do
  twinkle <- readIORef t
  writeIORef t $! not twinkle
keyPressed _ zoom _    _   GLFW.Key'PageUp   _ GLFW.KeyState'Pressed _ = do
  zd <- readIORef zoom
  writeIORef zoom $! zd - 0.2
keyPressed _ zoom _    _   GLFW.Key'PageDown _ GLFW.KeyState'Pressed _ = do
  zd <- readIORef zoom
  writeIORef zoom $! zd + 0.2
keyPressed _ _    tilt _   GLFW.Key'Up       _ GLFW.KeyState'Pressed _ = do
  xs <- readIORef tilt 
  writeIORef tilt $! xs - 0.5
keyPressed _ _    tilt _   GLFW.Key'Down     _ GLFW.KeyState'Pressed _ = do
  xs <- readIORef tilt
  writeIORef tilt $! xs + 0.5
keyPressed _ _    _    _   _                 _ _                     _ = return ()

main :: IO ()
main = do
     True <- GLFW.init
     -- select type of display mode:
     -- Double buffer
     -- RGBA color
     -- Alpha components supported
     -- Depth buffer
     -- open a window
     GLFW.defaultWindowHints
     Just win <- GLFW.createWindow 800 600 "Lesson 9" Nothing Nothing
     GLFW.makeContextCurrent (Just win)
     twinkle <- newIORef False
     spin    <- newIORef 0
     stars   <- generateStars
     zoom    <- newIORef (-15)
     tilt    <- newIORef 90
     -- initialize our window.
     tex <- initGL win
     GLFW.setWindowRefreshCallback win $
       Just (drawScene tex zoom tilt twinkle spin stars)
     -- register the funciton called when our window is resized
     GLFW.setFramebufferSizeCallback win (Just resizeScene)
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback win $
       Just (keyPressed twinkle zoom tilt)
     GLFW.setWindowCloseCallback win (Just shutdown)
     forever $ do
       GLFW.pollEvents
       drawScene tex zoom tilt twinkle spin stars win
       GLFW.swapBuffers win

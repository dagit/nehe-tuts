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
import Control.Monad ( forever, when, forM_, join )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Foreign ( withForeignPtr, plusPtr, alloca, peek )
import qualified Data.ByteString.Internal as BSI
import Util ( Image(..), bitmapLoad )
import Data.Array.IO ( readArray, IOUArray, newListArray )
import Control.Applicative ( (<$>), (<*>) )
import System.Directory ( getCurrentDirectory, setCurrentDirectory )

type Points = IOUArray (Int, Int, Int) Float

initGL :: IO GLuint
initGL = do
  glEnable gl_TEXTURE_2D
  glShadeModel gl_SMOOTH
  glClearColor 0 0 0 0.5
  glClearDepth 1
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST
  glPolygonMode gl_BACK gl_FILL
  glPolygonMode gl_FRONT gl_LINE
  loadGLTextures

loadGLTextures :: IO GLuint
loadGLTextures = do
  Just (Image w h pd) <- bitmapLoad "Data/tim.bmp"
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

drawScene :: GLuint -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat
          -> Points -> IORef Int -> IORef Int -> IO () 
drawScene tex xrot yrot zrot points wiggleRef offsetRef = do
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT

  glLoadIdentity

  glTranslatef 0 0 (-12)

  xr <- readIORef xrot
  yr <- readIORef yrot
  zr <- readIORef zrot
  offset <- readIORef offsetRef
  wiggle <- readIORef wiggleRef
  glRotatef xr 1 0 0
  glRotatef yr 0 1 0
  glRotatef zr 0 0 1
  glBindTexture gl_TEXTURE_2D tex
  glBegin gl_QUADS
  forM_ [(x,y) | x <- [0..43], y<-[0..43]] $ \(x,y) -> do
    let x'  = (x+offset) `mod` 45
        fx  = fromIntegral x/44 :: GLfloat
        fy  = fromIntegral y/44 :: GLfloat
        fxb = fromIntegral (x+1)/44 :: GLfloat
        fyb = fromIntegral (y+1)/44 :: GLfloat
    glTexCoord2f fx fy
    join $ glVertex3f <$> (readArray' points (x,y,0))
                      <*> (readArray' points (x,y,1))
                      <*> (readArray' points (x',y,2))
    glTexCoord2f fx fyb
    join $ glVertex3f <$> (readArray' points (x,y+1,0))
                      <*> (readArray' points (x,y+1,1))
                      <*> (readArray' points (x',y+1,2))
    glTexCoord2f fxb fyb
    join $ glVertex3f <$> (readArray' points (x+1,y+1,0))
                      <*> (readArray' points (x+1,y+1,1))
                      <*> (readArray' points ((x'+1)`mod`45,y+1,2))
    glTexCoord2f fxb fy
    join $ glVertex3f <$> (readArray' points (x+1,y,0))
                      <*> (readArray' points (x+1,y,1))
                      <*> (readArray' points ((x'+1)`mod`45,y,2))
    return ()
  glEnd

  writeIORef xrot $! xr + 0.3
  writeIORef yrot $! yr + 0.2
  writeIORef zrot $! zr + 0.4

  when (wiggle == 2) $ do
    writeIORef offsetRef $! offset + 1
    writeIORef wiggleRef $! 0 

  w <- readIORef wiggleRef
  writeIORef wiggleRef $! w + 1
 
  glFlush

readArray' :: IOUArray (Int, Int, Int) Float -> (Int, Int, Int) -> IO GLfloat
readArray' a (x,y,z) = do
  r <- readArray a (x,y,z)
  -- This line is extremely slow, because a RULES is missing
  -- in openglraw.  You can replace it with Unsafe.Coerce.unsafeCoerce
  -- on most platforms to get a HUGE speed up.
  return $! realToFrac r

keyPressed :: GLFW.KeyCallback
keyPressed GLFW.KeyEsc True = shutdown >> return ()
keyPressed _           _    = return ()

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
                     , GLFW.displayOptions_numDepthBits = 24
                     -- , GLFW.displayOptions_displayMode = GLFW.Fullscreen
                     }
     -- open a window
     True <- GLFW.openWindow dspOpts
     -- window starts at upper left corner of the screen
     GLFW.setWindowPosition 0 0
     GLFW.setWindowTitle "Jeff Molofee's GL Code Tutorial ... NeHe '99"
     xrot <- newIORef 0
     yrot <- newIORef 0
     zrot <- newIORef 0
     wiggle <- newIORef 0
     offset <- newIORef 0
     let elems = concat [[((fromIntegral x/5)-4.5), 
                          ((fromIntegral y/5)-4.5),
                          sin (((fromIntegral x/5)*40/360)*pi*2)] 
                        | x <- [0..44]::[Int], y <- [0..44]::[Int] ]
     points <- newListArray ((0,0,0), (44,44,2)) elems :: IO Points
     -- initialize our window.
     tex <- initGL
     GLFW.setWindowRefreshCallback
       (drawScene tex xrot yrot zrot points wiggle offset)
     -- register the funciton called when our window is resized
     GLFW.setWindowSizeCallback resizeScene
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback $
       keyPressed
     GLFW.setWindowCloseCallback shutdown
     GLFW.getWindowRefreshRate >>= print
     forever $ do
       drawScene tex xrot yrot zrot points wiggle offset
       GLFW.swapBuffers

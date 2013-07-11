--
-- This code was created by Jeff Molofee '99 (ported to Haskell GHC 2005)
--

module Main where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw ( gluPerspective, gluBuild2DMipmaps )
import Data.Bits ( (.|.) )
import System.Exit ( exitWith, ExitCode(..) )
import System.IO ( openFile, IOMode(..), hGetContents )
import Data.IORef ( IORef, newIORef, writeIORef, readIORef, modifyIORef )
import Foreign ( ForeignPtr, withForeignPtr, newForeignPtr_
               , plusPtr )
import Foreign.Storable ( Storable )
import Foreign.Marshal.Array ( allocaArray, peekArray, newArray )
import qualified Data.ByteString.Internal as BSI
import Util ( Image(..), bitmapLoad )
import Control.Monad ( when, forM_, forever )
import Paths_nehe_tuts

type Sector = [Tri]

data Tri = Tri !Vert !Vert !Vert
           deriving (Eq, Ord, Show)

data Vert = Vert { vertX :: !GLfloat
                 , vertY :: !GLfloat
                 , vertZ :: !GLfloat
                 , vertU :: !GLfloat
                 , vertV :: !GLfloat
                 }
            deriving (Eq, Ord, Show)

piover180 :: GLfloat
piover180 = pi/180.0

data Global = Global { xrot, yrot, xspeed, yspeed, walkbias, walkbiasangle,
                       lookupdown, xpos, zpos, camx, camy, camz,
                       therotate, heading, zdepth :: IORef GLfloat,
                       filterSelector :: IORef Int,
                       lighting, blend :: IORef Bool }
mkGlobal :: IO Global
mkGlobal = do xr <- newIORef 0  -- please someone find a nicer way...
              yr <- newIORef 0
              xs <- newIORef 0
              ys <- newIORef 0
              wb <- newIORef 0
              wba <- newIORef 0
              lud <- newIORef 0
              xp <- newIORef 0
              zp <- newIORef 0
              cx <- newIORef 0
              cy <- newIORef 0
              cz <- newIORef 0
              tr <- newIORef 0
              h <- newIORef 0
              zd <- newIORef 0
              fs <- newIORef 0
              b  <- newIORef True
              l  <- newIORef True
              return Global { xrot = xr, yrot = yr, xspeed = xs, yspeed = ys,
                              walkbias = wb, walkbiasangle = wba, lookupdown =
                              lud, xpos = xp, zpos = zp, camx = cx, camy = cy,
                              camz = cz, therotate = tr, heading = h,
                              zdepth = zd, filterSelector = fs, blend = b,
                              lighting = l }

newArray' :: Storable a => [a] -> IO (ForeignPtr a)
newArray' xs = (newArray xs) >>= newForeignPtr_

glLightfv' :: GLenum -> GLenum -> ForeignPtr GLfloat -> IO ()
glLightfv' l a fp =
  withForeignPtr fp $ glLightfv l a

readRef :: IO (IORef a) -> IO a
readRef r = r >>= readIORef

setupWorld :: IO Sector
setupWorld = do
  fp <- getDataFileName "world.txt"
  h  <- openFile fp ReadMode
  ls <- (fmap (filter ignorable) ((fmap lines . hGetContents) h))
  let numtris = read ((head . tail . words . head) ls)
      tris    = readTris (tail ls)
  when (length tris /= numtris) $ do
    putStrLn "error reading world.txt"
    exitWith (ExitFailure 1)
  return tris
  where
  readTris :: [String] -> [Tri]
  readTris (l1:l2:l3:ls) = (Tri (readVert (words l1))
                                (readVert (words l2))
                                (readVert (words l3))) : readTris ls
  readTris [] = []
  readTris _  = undefined
  readVert :: [String] -> Vert
  readVert (x:y:z:u:v:[]) = Vert { vertX = read x
                                 , vertY = read y
                                 , vertZ = read z
                                 , vertU = read u
                                 , vertV = read v
                                 }
  readVert _              = undefined
  ignorable ('/':'/':_) = False
  ignorable []          = False
  ignorable _           = True

initGL :: GLFW.Window -> IO [GLuint]
initGL win = do
  glEnable gl_TEXTURE_2D
  glShadeModel gl_SMOOTH
  glClearColor 0 0 0 0.5
  glClearDepth 1
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST
  lightAmbient  <- newArray' [0.5, 0.5, 0.5, 1]
  lightDiffuse  <- newArray' [1, 1, 1, 1]
  lightPosition <- newArray' [0, 0, 2, 1]
  glLightfv' gl_LIGHT1 gl_AMBIENT  lightAmbient
  glLightfv' gl_LIGHT1 gl_DIFFUSE  lightDiffuse
  glLightfv' gl_LIGHT1 gl_POSITION lightPosition
  glEnable gl_LIGHTING
  glEnable gl_LIGHT1
  glBlendFunc gl_SRC_ALPHA gl_ONE
  glEnable gl_BLEND
  (w,h) <- GLFW.getFramebufferSize win
  resizeScene win w h
  loadTextures

loadTextures :: IO [GLuint]
loadTextures = do
  fp <- getDataFileName "mud.bmp"
  Just (Image w h pd) <- bitmapLoad fp
  let numTexs = 3 :: Int
  texs <- allocaArray (fromIntegral numTexs) $ \p -> do
    glGenTextures (fromIntegral numTexs) p
    peekArray numTexs p
  let (ptr, off, _) = BSI.toForeignPtr pd
  _ <- withForeignPtr ptr $ \p -> do
    let p' = p `plusPtr` off
        glLinear  = fromIntegral gl_LINEAR
        glNearest = fromIntegral gl_NEAREST
    -- create nearest filter texture
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

drawScene :: [GLuint] -> Sector -> Global -> GLFW.Window -> IO ()
drawScene texs sector globals _ = do
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity  -- reset view
  xtrans <- fmap negate (readIORef (xpos globals))
  ytrans <- fmap (\y -> -y-0.25) (readIORef (walkbias globals))
  ztrans <- fmap negate (readIORef (zpos globals))
  sceneroty <- fmap (\y -> 360 - y) (readIORef (yrot globals))
  --print sceneroty
  look <- readIORef (lookupdown globals)
  filt <- readIORef (filterSelector globals)
  glRotatef look 1 0 0
  glRotatef sceneroty 0 1 0
  
  glTranslatef xtrans ytrans ztrans
  glBindTexture gl_TEXTURE_2D (texs!!filt)
  forM_ sector $ \(Tri (Vert x1 y1 z1 u1 v1)
                       (Vert x2 y2 z2 u2 v2)
                       (Vert x3 y3 z3 u3 v3)) -> do
    glBegin gl_TRIANGLES
    glNormal3f 0 0 1
    glTexCoord2f u1 v1 >> glVertex3f x1 y1 z1
    glTexCoord2f u2 v2 >> glVertex3f x2 y2 z2
    glTexCoord2f u3 v3 >> glVertex3f x3 y3 z3
    glEnd 
  glFlush

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

updateIORef :: Show a => IO (IORef a) -> (a -> a) -> IO ()
updateIORef ioref f = do ref <- ioref
                         r <- readIORef ref
                         print r
                         writeIORef ref (f r)

keyPressed :: Global -> GLFW.KeyCallback
keyPressed _ win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed g _   GLFW.Key'B      _ GLFW.KeyState'Pressed _ = do
  r <- readIORef (blend g)
  if r
     then do
       glDisable gl_BLEND
       glEnable gl_DEPTH_TEST
     else do
       glEnable gl_BLEND
       glDisable gl_DEPTH_TEST
  writeIORef (blend g) $! not r
keyPressed g _ GLFW.Key'F _ GLFW.KeyState'Pressed _ =
  modifyIORef (filterSelector g) (\x -> (x+1) `mod` 3)
keyPressed g _ GLFW.Key'L _ GLFW.KeyState'Pressed _ = do
  l <- readIORef (lighting g)
  if l
     then glDisable gl_LIGHTING
     else glEnable  gl_LIGHTING  
  writeIORef (lighting g) $! not l
keyPressed g _ GLFW.Key'Right _ GLFW.KeyState'Pressed _ =
  modifyIORef (yrot g) (subtract 1.5)
keyPressed g _ GLFW.Key'Left _ GLFW.KeyState'Pressed _ =
  modifyIORef (yrot g) (+ 1.5)
keyPressed g _ GLFW.Key'Up _ GLFW.KeyState'Pressed _ = do
  h <- readIORef (yrot g)
  modifyIORef (xpos g) (subtract ((sin (h * piover180))*0.05))
  modifyIORef (zpos g) (subtract ((cos (h * piover180))*0.05))
  modifyIORef (walkbiasangle g) (\w -> (w+10) `fmod` 360)
  wba <- readIORef (walkbiasangle g)
  modifyIORef (walkbias g) (\_ -> ((sin (wba * piover180))/20))
keyPressed g _ GLFW.Key'Down _ GLFW.KeyState'Pressed _ = do
  h <- readIORef (yrot g)
  modifyIORef (xpos g) (+ (sin (h * piover180))*0.05)
  modifyIORef (zpos g) (+ (cos (h * piover180))*0.05)
  modifyIORef (walkbiasangle g) (\w -> (w-10) `fmod` 360)
  wba <- readIORef (walkbiasangle g)
  modifyIORef (walkbias g) (\_ -> ((sin (wba * piover180))/20))
keyPressed g _ GLFW.Key'PageUp _ GLFW.KeyState'Pressed _ = do
  modifyIORef (zdepth g) (subtract 0.2)
  modifyIORef (lookupdown g) (subtract 0.2)
keyPressed g _ GLFW.Key'PageDown _ GLFW.KeyState'Pressed _ = do
  modifyIORef (zdepth g) (+ 0.2)
  modifyIORef (lookupdown g) (+ 0.2)
keyPressed _ _ _ _ _ _  = return ()

fmod :: RealFrac a => a -> Int -> a
fmod x m = (fromIntegral ((floor x :: Int) `mod` m)) + 
           (x - (fromIntegral (floor x :: Int)))

main :: IO ()
main = do
     True <- GLFW.init
     sector <- setupWorld
     -- select type of display mode:
     -- Double buffer
     -- RGBA color
     -- Alpha components supported
     -- Depth buffer
     -- open a window
     Just win <- GLFW.createWindow 800 600 "Lesson 10" Nothing Nothing
     GLFW.makeContextCurrent (Just win)
     -- initialize our window.
     tex <- initGL win
     globals <- mkGlobal
     GLFW.setWindowRefreshCallback win $
       Just (drawScene tex sector globals)
     -- register the funciton called when our window is resized
     GLFW.setFramebufferSizeCallback win (Just resizeScene)
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback win $
       Just (keyPressed globals)
     GLFW.setWindowCloseCallback win (Just shutdown)
     forever $ do
       GLFW.pollEvents
       drawScene tex sector globals win
       GLFW.swapBuffers win

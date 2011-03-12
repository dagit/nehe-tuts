--
-- This code was created by Jeff Molofee '99 (ported to Haskell GHC 2005)
--

module Main where

import Graphics.UI.GLUT 
import System.Exit ( exitWith, ExitCode(..) )
import System.IO ( openFile, IOMode(..), hGetContents )
import Data.IORef ( IORef, newIORef, writeIORef, readIORef, modifyIORef )
import Util ( Image(..), bitmapLoad )
import Monad ( when )

type Sector = [Tri]

data Tri = Tri Vert Vert Vert
           deriving (Eq, Ord, Show)

data Vert = Vert { vertPoint :: Vertex3 GLfloat, 
                   vertTx, vertTy :: GLfloat }
            deriving (Eq, Ord, Show)

piover180 :: GLfloat
piover180 = 0.0174532925

data Global = Global { xrot, yrot, xspeed, yspeed, walkbias, walkbiasangle,
                       lookupdown, xpos, zpos, camx, camy, camz,
                       therotate, heading, zdepth :: IORef GLfloat,
                       filterSelector :: IORef Int }
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
              return Global { xrot = xr, yrot = yr, xspeed = xs, yspeed = ys,
                              walkbias = wb, walkbiasangle = wba, lookupdown =
                              lud, xpos = xp, zpos = zp, camx = cx, camy = cy,
                              camz = cz, therotate = tr, heading = h,
                              zdepth = zd, filterSelector = fs }

lightAmbient, lightDiffuse :: Color4 GLfloat
lightAmbient = Color4 0.5 0.5 0.5 1
lightDiffuse = Color4 1 1 1 1

lightPosition :: Vertex4 GLfloat
lightPosition = Vertex4 0 0 2 1

readRef :: IO (IORef a) -> IO a
readRef r = r >>= readIORef

setupWorld :: IO Sector
setupWorld = do h <- openFile "Data/world.txt" ReadMode
                ls <- (fmap (filter ignorable) ((fmap lines . hGetContents) h))
                let numtris = read ((head . tail . words . head) ls)
                let tris = readTris (tail ls)
                when (length tris /= numtris) $ putStrLn "error reading world.txt" >> exitWith (ExitFailure 1)
                return tris
  where
  readTris (l1:l2:l3:ls) = Tri (readVert (words l1)) (readVert (words l2))
                               (readVert (words l3)) : readTris ls
  readTris [] = []
  readTris _  = undefined
  readVert (x:y:z:u:v:[]) = Vert (readVertex (x,y,z)) (read u) (read v)
  readVert _              = undefined
  readVertex (x,y,z) = Vertex3 (read x) (read y) (read z)
  ignorable ('/':'/':_) = False
  ignorable []          = False
  ignorable _           = True
                
initGL :: IO [TextureObject]
initGL = do
  tex <- loadGLTextures
  texture Texture2D $= Enabled
  clearColor $= Color4 0 0 0 0.5 -- Clear the background color to black
  clearDepth $= 1 -- enables clearing of the depth buffer
  depthFunc  $= Just Less -- type of depth test
  shadeModel $= Smooth -- enables smooth color shading
  matrixMode $= Projection
  hint PerspectiveCorrection $= Nicest
  ambient (Light 1) $= lightAmbient
  diffuse (Light 1) $= lightDiffuse
  position (Light 1) $= lightPosition
  light (Light 1) $= Enabled
  lighting $= Enabled
  loadIdentity  -- reset projection matrix
  Size width height <- get windowSize
  perspective 45 (fromIntegral width/fromIntegral height) 0.1 100 -- calculate the aspect ratio of the window
  matrixMode $= Modelview 0
  color (Color4 1 1 1 (0.5::GLfloat))
  blendFunc $= (SrcAlpha, One)

  flush -- finally, we tell opengl to do it.
  return tex

loadGLTextures :: IO [TextureObject]
loadGLTextures = do
  (Image (Size w h) pd) <- bitmapLoad "Data/mud.bmp"
  texNames <- (genObjectNames 3)
  -- create nearest filtered texture
  textureBinding Texture2D $= Just (texNames !! 0)
  textureFilter  Texture2D $= ((Nearest, Nothing), Nearest)
  texImage2D Nothing NoProxy 0 RGB' (TextureSize2D w h) 0 pd
  -- create linear filtered texture
  textureBinding Texture2D $= Just (texNames !! 1)
  textureFilter  Texture2D $= ((Linear', Nothing), Linear')
  texImage2D Nothing NoProxy 0 RGB' (TextureSize2D w h) 0 pd
  -- create mipmap filtered texture
  textureBinding Texture2D $= Just (texNames !! 2)
  textureFilter  Texture2D $= ((Linear', Just Nearest), Linear')
  texImage2D Nothing NoProxy 0 RGB' (TextureSize2D w h) 0 pd
  build2DMipmaps Texture2D RGB' w h pd
  return texNames

resizeScene :: Size -> IO ()
resizeScene (Size w 0) = resizeScene (Size w 1) -- prevent divide by zero
resizeScene s@(Size width height) = do
  viewport   $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45 (fromIntegral width/fromIntegral height) 0.1 100
  matrixMode $= Modelview 0
  flush

drawScene :: [TextureObject] -> Sector -> Global -> IO ()
drawScene texs sector globals = do
  clear [ColorBuffer, DepthBuffer] -- clear the screen and the depth bufer
  loadIdentity  -- reset view
  xtrans <- fmap negate (readIORef (xpos globals))
  ytrans <- fmap (\y -> -y-0.25) (readIORef (walkbias globals))
  ztrans <- fmap negate (readIORef (zpos globals))
  sceneroty <- fmap (\y -> 360 - y) (readIORef (yrot globals))
  --print sceneroty
  look <- readIORef (lookupdown globals)
  filt <- readIORef (filterSelector globals)
  rotate look (Vector3 1 0 0)
  rotate sceneroty (Vector3 0 1 0)
  
  translate (Vector3 xtrans ytrans ztrans)
  textureBinding Texture2D $= Just (texs!!filt)
  mapM_ (\t -> renderPrimitive Triangles $
    do normal (Normal3 0 0 (1::GLfloat))
       case t of Tri (Vert (Vertex3 x1 y1 z1) u1 v1) 
                     (Vert (Vertex3 x2 y2 z2) u2 v2) 
                     (Vert (Vertex3 x3 y3 z3) u3 v3) -> do texCoord (TexCoord2 u1 v1)
                                                           vertex (Vertex3 x1 y1 z1)
                                                           texCoord (TexCoord2 u2 v2)
                                                           vertex (Vertex3 x2 y2 z2)
                                                           texCoord (TexCoord2 u3 v3)
                                                           vertex (Vertex3 x3 y3 z3)) sector
  
  -- since this is double buffered, swap the buffers to display what was just
  -- drawn
  flush
  swapBuffers

updateIORef :: Show a => IO (IORef a) -> (a -> a) -> IO ()
updateIORef ioref f = do ref <- ioref
                         r <- readIORef ref
                         print r
                         writeIORef ref (f r)

keyPressed :: Global -> KeyboardMouseCallback
-- 27 is ESCAPE
keyPressed _ (Char '\27') Down _ _ = exitWith ExitSuccess
keyPressed g (Char 'B') Down x y = keyPressed g (Char 'b') Down x y
keyPressed _ (Char 'b') Down _ _  = do
  r <- get blend
  if r == Enabled
     then blend $= Disabled >> depthFunc $= (Just Less)
     else blend $= Enabled >> depthFunc $= Nothing
keyPressed g (Char 'f') Down _ _ = modifyIORef (filterSelector g) (\x -> x+1 `mod` 3)
keyPressed g (Char 'F') Down x y = keyPressed g (Char 'f') Down x y
keyPressed g (Char 'L') Down x y = keyPressed g (Char 'l') Down x y
keyPressed _ (Char 'l') Down _ _ = do
  l <- get lighting
  if l == Enabled
     then lighting $= Disabled
     else lighting $= Enabled  
keyPressed g (SpecialKey KeyRight) Down _ _ = modifyIORef (yrot g) (subtract 1.5)
keyPressed g (SpecialKey KeyLeft) Down _ _ = modifyIORef (yrot g) (+ 1.5)
keyPressed g (SpecialKey KeyUp) Down _ _ = do
  h <- readIORef (yrot g)
  modifyIORef (xpos g) (subtract ((sin (h * piover180))*0.05))
  modifyIORef (zpos g) (subtract ((cos (h * piover180))*0.05))
  modifyIORef (walkbiasangle g) (\w -> (w+10) `fmod` 360)
  wba <- readIORef (walkbiasangle g)
  modifyIORef (walkbias g) (\_ -> ((sin (wba * piover180))/20))
keyPressed g (SpecialKey KeyDown) Down _  _ = do
  h <- readIORef (yrot g)
  modifyIORef (xpos g) (+ (sin (h * piover180))*0.05)
  modifyIORef (zpos g) (+ (cos (h * piover180))*0.05)
  modifyIORef (walkbiasangle g) (\w -> (w-10) `fmod` 360)
  wba <- readIORef (walkbiasangle g)
  modifyIORef (walkbias g) (\_ -> ((sin (wba * piover180))/20))
keyPressed g (SpecialKey KeyPageUp) Down _ _ = do
  modifyIORef (zdepth g) (subtract 0.2)
  modifyIORef (lookupdown g) (subtract 0.2)
keyPressed g (SpecialKey KeyPageDown) Down _ _ = do
  modifyIORef (zdepth g) (+ 0.2)
  modifyIORef (lookupdown g) (+ 0.2)

keyPressed _ _           _    _ _ = return ()

fmod :: RealFrac a => a -> Int -> a
fmod x m = (fromIntegral ((floor x :: Int) `mod` m)) + 
           (x - (fromIntegral (floor x :: Int)))

main :: IO ()
main = do
     -- Initialize GLUT state - glut will take any command line arguments
     -- that pertain to it or X windows -- look at its documentation at
     -- http://reality.sgi.com/mjk/spec3/spec3.html
     sector <- setupWorld

     _ <- getArgsAndInitialize
     -- select type of display mode:
     -- Double buffer
     -- RGBA color
     -- Alpha components supported
     -- Depth buffer
     initialDisplayMode $= [ DoubleBuffered, RGBAMode, WithDepthBuffer, 
                             WithAlphaComponent ]
     -- get an 800 x 600 window
     initialWindowSize $= Size 800 600
     -- window starts at upper left corner of the screen
     initialWindowPosition $= Position 0 0
     -- open a window
     _ <- createWindow "Jeff Molofee's GL Code Tutorial ... NeHe '99"
     -- register the function to do all our OpenGL drawing
     -- initialize our window.
     texs <- initGL
     global <- mkGlobal
     displayCallback $= (drawScene texs sector global)
     -- go fullscreen. This is as soon as possible.
     fullScreen
     -- even if there are no events, redraw our gl scene
     idleCallback $= Just (drawScene texs sector global)
     -- register the funciton called when our window is resized
     reshapeCallback $= Just resizeScene
     -- register the function called when the keyboard is pressed.
     keyboardMouseCallback $= Just (keyPressed global)
     -- start event processing engine
     mainLoop

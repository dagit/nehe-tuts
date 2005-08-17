--
-- This code was created by Jeff Molofee '99 (ported to Haskell GHC 2005)
--

module Main where

import System.Exit
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL 
import Graphics.Rendering.OpenGL.GLU
import Graphics.UI.GLUT
import Control.Concurrent
import Data.IORef
import System.IO
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.Ptr
import Data.Word
import Data.Int
import Control.Monad
import GHC.IOBase

data Endian = LittleEndian | BigEndian
              deriving (Eq, Ord, Show)

data Image = Image Size (PixelData Word8)

lightAmbient :: Color4 GLfloat
lightAmbient = Color4 0.5 0.5 0.5 1.0
lightDiffuse :: Color4 GLfloat
lightDiffuse = Color4 1.0 1.0 1.0 1.0
lightPosition :: Vertex4 GLfloat
lightPosition = Vertex4 0.0 0.0 2.0 1.0

initGL = do
  texs <- loadGLTextures
  texture Texture2D $= Enabled
  clearColor $= Color4 0 0 0 0.5 -- Clear the background color to black
  clearDepth $= 1 -- enables clearing of the depth buffer
  depthFunc  $= Just Less -- type of depth test
  hint PerspectiveCorrection $= Nicest
  ambient (Light 1) $= lightAmbient
  diffuse (Light 1) $= lightDiffuse
  position (Light 1) $= lightPosition
  light (Light 1) $= Enabled
  lighting $= Enabled
  shadeModel $= Smooth -- enables smooth color shading
  matrixMode $= Projection
  loadIdentity  -- reset projection matrix
  Size width height <- get windowSize
  perspective 45 (fromIntegral width/fromIntegral height) 0.1 100 -- calculate the aspect ratio of the window
  matrixMode $= Modelview 0

  color (Color4 1 1 1 (0.5::GLfloat))
  blendFunc $= (SrcAlpha, One)
  flush -- finally, we tell opengl to do it.
  return texs

bitmapLoad :: String -> IO Image
bitmapLoad f = do
  handle <- openBinaryFile f ReadMode
  hSeek handle RelativeSeek 18
  width <- readInt handle
  putStrLn ("Width of "++f++": "++show width)
  height <- readInt handle
  putStrLn ("Height of "++f++": "++show height)
  planes <- readShort handle
  bpp <- readShort handle
  size <- return (width*height*3)
  hSeek handle RelativeSeek 24
  putStrLn ("Planes = "++(show planes))
  bgrBytes <- (readBytes handle (fromIntegral size) :: IO (Ptr Word8))
  rgbBytes <- bgr2rgb bgrBytes (fromIntegral size)
  return (Image (Size (fromIntegral width)
                      (fromIntegral height)) 
          (PixelData RGB UnsignedByte rgbBytes))
-- Begin low level Bitmap loading code
endian :: Endian
endian = let r = unsafePerformIO (
                 do w <- allocaBytes 4 (\p -> do pokeElemOff p 0 (0::Word8)
                                                 pokeElemOff p 1 (1::Word8)
                                                 pokeElemOff p 2 (2::Word8)
                                                 pokeElemOff p 3 (3::Word8)
                                                 peek (castPtr p) :: IO Int32)
                    return w)
         in case r of 50462976 -> LittleEndian
                      66051    -> BigEndian
                      _        -> undefined

bgr2rgb :: Ptr Word8 -> Int -> IO (Ptr Word8)
bgr2rgb p n = mapM_ (\i -> do b <- peekElemOff p (i+0)
                              g <- peekElemOff p (i+1)
                              r <- peekElemOff p (i+2)
                              pokeElemOff p (i+0) r
                              pokeElemOff p (i+1) g
                              pokeElemOff p (i+2) b) [0,3..n-3] 
              >> return p
                  
readStorable :: Storable a => Handle -> Int -> IO (Ptr a)
readStorable h n = do p <- mallocBytes n
                      hGetBuf h p n
                      return p

-- This is only needed if you're on PowerPC instead of x86
-- if you are on x86 use the following:
-- reverseBytes p _ = return p
reverseBytes :: Ptr Word8 -> Int -> IO (Ptr Word8)
reverseBytes p n | endian == BigEndian = 
                   do p' <- mallocBytes n
                      mapM_ (\i -> peekElemOff p i >>= pokeElemOff p' (n-i-1)) 
                            [0..n-1]
                      return p'
                 | endian == LittleEndian = do p' <- mallocBytes n
                                               copyBytes p' p n
                                               return p'
                            
readBytes :: Storable a => Handle -> Int -> IO (Ptr a)
readBytes h n = do p <- mallocBytes n
                   hGetBuf h p n
                   return p

readShort :: Handle -> IO Word16
readShort h = do p <- readBytes h 2 :: IO (Ptr Word8)
                 p' <- reverseBytes (castPtr p) 2
                 free p
                 r <- peek (castPtr p')
                 free p'
                 return r

readInt :: Handle -> IO Int32
readInt h = do p <- readBytes h 4 :: IO (Ptr Word8)
               p' <- reverseBytes (castPtr p) 4
               free p
               r <- peek (castPtr p')
               free p'
               return r
-- End low level bitmap loading code

loadGLTextures = do
  (Image (Size w h) pd) <- bitmapLoad "Data/glass.bmp"
  texNames <- genObjectNames 3
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

resizeScene (Size w 0) = resizeScene (Size w 1) -- prevent divide by zero
resizeScene s@(Size width height) = do
  viewport   $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45 (fromIntegral width/fromIntegral height) 0.1 100
  matrixMode $= Modelview 0
  flush

drawScene texs xrot yrot xspeed yspeed zdepth filter = do
  clear [ColorBuffer, DepthBuffer] -- clear the screen and the depth bufer
  loadIdentity  -- reset view
  zd <- get zdepth
  translate (Vector3 0 0 (zd::GLfloat)) --Move left 5 Units into the screen

  xr <- get xrot
  yr <- get yrot
  rotate xr (Vector3 1 0 (0::GLfloat)) -- Rotate the triangle on the Y axis
  rotate yr (Vector3 0 1 (0::GLfloat)) -- Rotate the triangle on the Y axis
  f <- get filter
  textureBinding Texture2D $= Just (texs!!f)
  renderPrimitive Quads $  -- start drawing a polygon (4 sided)
    do 
       -- first the front
       normal (Normal3 0.0 0.0 (1.0::GLfloat))
       texCoord (TexCoord2 0 (0::GLfloat)) 
       vertex (Vertex3 (-1) (-1)   (1::GLfloat))  -- bottom left of quad (Front)
       texCoord (TexCoord2 1 (0::GLfloat)) 
       vertex (Vertex3  1   (-1)   (1::GLfloat))  -- bottom right of quad (Front)
       texCoord (TexCoord2 1 (1::GLfloat)) 
       vertex (Vertex3  1      1   (1::GLfloat))  -- top right of quad (Front)
       texCoord (TexCoord2 0 (1::GLfloat)) 
       vertex (Vertex3 (-1)    1   (1::GLfloat))  -- top left of quad (Front)
       -- now the back
       normal (Normal3 0.0 0.0 (-1.0::GLfloat))
       texCoord (TexCoord2 1 (0::GLfloat)) 
       vertex (Vertex3 (-1) (-1)  (-1::GLfloat))  -- bottom right of quad (Back)
       texCoord (TexCoord2 1 (1::GLfloat)) 
       vertex (Vertex3 (-1)    1  (-1::GLfloat))  -- top right of quad (Back)
       texCoord (TexCoord2 0 (1::GLfloat)) 
       vertex (Vertex3    1    1  (-1::GLfloat))  -- top left of quad (Back)
       texCoord (TexCoord2 0 (0::GLfloat)) 
       vertex (Vertex3    1 (-1)  (-1::GLfloat))  -- bottom left of quad (Back)
       -- now the top
       normal (Normal3 0.0 1.0 (0.0::GLfloat))
       texCoord (TexCoord2 0 (1::GLfloat))
       vertex (Vertex3 (-1)  1   (-1::GLfloat))  -- top left of quad (Top)
       texCoord (TexCoord2 0 (0::GLfloat))  
       vertex (Vertex3 (-1)  1    (1::GLfloat))  -- bottom left of quad (Top)
       texCoord (TexCoord2 1 (0::GLfloat))  
       vertex (Vertex3  1    1    (1::GLfloat))  -- bottom right of quad (Top)
       texCoord (TexCoord2 1 (1::GLfloat))  
       vertex (Vertex3  1    1   (-1::GLfloat))  -- top right of quad (Top)
       -- now the bottom
       normal (Normal3 0.0 (-1.0) (0.0::GLfloat))
       texCoord (TexCoord2 1 (1::GLfloat))  
       vertex (Vertex3  1   (-1)   (1::GLfloat))  -- top right of quad (Bottom)
       texCoord (TexCoord2 0 (1::GLfloat))  
       vertex (Vertex3 (-1) (-1)   (1::GLfloat))  -- top left of quad (Bottom)
       texCoord (TexCoord2 0 (0::GLfloat))  
       vertex (Vertex3 (-1) (-1)  (-1::GLfloat))  -- bottom left of quad (Bottom)
       texCoord (TexCoord2 1 (0::GLfloat))  
       vertex (Vertex3  1   (-1)  (-1::GLfloat))  -- bottom right of quad (Bottom)
       -- now the right
       normal (Normal3 1.0 0.0 (0.0::GLfloat))
       texCoord (TexCoord2 1 (0::GLfloat))  
       vertex (Vertex3 1 (-1)  (-1::GLfloat))  -- bottom right of quad (Right)
       texCoord (TexCoord2 1 (1::GLfloat))  
       vertex (Vertex3 1    1  (-1::GLfloat))  -- top right of quad (Right)
       texCoord (TexCoord2 0 (1::GLfloat))  
       vertex (Vertex3 1    1   (1::GLfloat))  -- top left of quad (Right)
       texCoord (TexCoord2 0 (0::GLfloat))  
       vertex (Vertex3 1 (-1)   (1::GLfloat))  -- bottom left of quad (Right)
       -- now the left
       normal (Normal3 (-1.0) 0.0 (1.0::GLfloat))
       texCoord (TexCoord2 0 (0::GLfloat))  
       vertex (Vertex3 (-1) (-1)  (-1::GLfloat))  -- bottom left of quad (Left)
       texCoord (TexCoord2 1 (0::GLfloat))  
       vertex (Vertex3 (-1)    1  (-1::GLfloat))  -- top left of quad (Left)
       texCoord (TexCoord2 1 (1::GLfloat))  
       vertex (Vertex3 (-1)    1   (1::GLfloat))  -- top right of quad (Left)
       texCoord (TexCoord2 0 (1::GLfloat))  
       vertex (Vertex3 (-1) (-1)   (1::GLfloat))  -- bottom right of quad (Left)
  xsp <- get xspeed
  ysp <- get yspeed  
  xrot $= xr + xsp
  yrot $= yr + ysp
  -- since this is double buffered, swap the buffers to display what was just
  -- drawn
  flush
  swapBuffers
  --threadDelay 100

keyPressed :: IORef Int -> IORef GLfloat -> IORef GLfloat ->
              IORef GLfloat -> KeyboardMouseCallback
-- 27 is ESCAPE
keyPressed _ _ _ _ (Char '\27') Down _ _ = exitWith ExitSuccess
keyPressed _ _ _ _ (Char 'L') Down   _ _ = do l <- get lighting 
                                              if l == Enabled 
                                                 then lighting $= Disabled
                                                 else lighting $= Enabled
                                              return ()
keyPressed filter _ _ _ (Char 'F') Down  _  _ = 
  get filter >>= writeIORef filter . (flip mod 3) . (+1)
keyPressed _ _ _ _ (Char 'B') Down _ _ = do
  b <- get blend
  if b == Enabled 
     then blend $= Disabled >> depthFunc $= Just Less
     else blend $= Enabled >> depthFunc $= Nothing
  return ()
keyPressed f zd xs ys (Char 'l') d x y = keyPressed f zd xs ys (Char 'L') d x y
keyPressed f zd xs ys (Char 'f') d x y = keyPressed f zd xs ys (Char 'F') d x y
keyPressed f zd xs ys (Char 'b') d x y = keyPressed f zd xs ys (Char 'B') d x y
keyPressed _ zdepth _ _ (SpecialKey KeyPageUp) Down _ _ = 
  get zdepth >>= writeIORef zdepth . (subtract 0.2)
keyPressed _ zdepth _ _ (SpecialKey KeyPageDown) Down _ _ = 
  get zdepth >>= writeIORef zdepth . (+0.2)
keyPressed _ _ xspeed _ (SpecialKey KeyUp) Down _ _ = 
  get xspeed >>= writeIORef xspeed . (subtract 0.1)          
keyPressed _ _ xspeed _ (SpecialKey KeyDown) Down _ _ = 
  get xspeed >>= writeIORef xspeed . (+ 0.1)
keyPressed _ _ _ yspeed (SpecialKey KeyRight) Down _ _ = do 
  get yspeed >>= writeIORef yspeed . (+ 0.1)
keyPressed _ _ _ yspeed (SpecialKey KeyLeft) Down _ _ = do
  get yspeed >>= writeIORef yspeed . (subtract 0.1)
keyPressed _ _ _ _     _       _    _ _ = return ()

main = do
     -- Initialize GLUT state - glut will take any command line arguments
     -- that pertain to it or X windows -- look at its documentation at
     -- http://reality.sgi.com/mjk/spec3/spec3.html
--     bitmapLoad "Data/lesson06/NeHe.bmp"
     getArgsAndInitialize 
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
     createWindow "Jeff Molofee's GL Code Tutorial ... NeHe '99"
     -- register the function to do all our OpenGL drawing
     xrot <- newIORef (0::GLfloat)
     yrot <- newIORef (0::GLfloat)
     xspeed <- newIORef (0::GLfloat)
     yspeed <- newIORef (0::GLfloat)
     zdepth <- newIORef (-5.0 :: GLfloat)
     filter <- newIORef 0
     blend <- newIORef False
     -- initialize our window.
     texs <- initGL
     displayCallback $= (drawScene texs xrot yrot xspeed yspeed zdepth filter)
     -- go fullscreen. This is as soon as possible.
     fullScreen
     -- even if there are no events, redraw our gl scene
     idleCallback $= Just (drawScene texs xrot yrot xspeed yspeed zdepth filter)
     -- register the funciton called when our window is resized
     reshapeCallback $= Just resizeScene
     -- register the function called when the keyboard is pressed.
     keyboardMouseCallback $= Just (keyPressed filter zdepth xspeed yspeed)
     -- start event processing engine
     mainLoop

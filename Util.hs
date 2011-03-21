module Util ( bitmapLoad, Image(..) ) where

import Data.Word ( Word8, Word32 )
import Data.Serialize.Get ( Get, runGet, getWord32le, getWord16le
                          , skip, getByteString )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign ( Ptr, pokeElemOff, peekElemOff, plusPtr
               , withForeignPtr )

data Image = Image !Int !Int !BS.ByteString

bitmapLoad :: FilePath -> IO (Maybe Image)
bitmapLoad f = do
  bs <- BS.readFile f
  case runGet getBitmap bs of
    Left  err -> putStrLn err >> return Nothing
    Right i@(Image _ _ bytes) -> do
      let (ptr, offset, len) = BSI.toForeignPtr bytes
      withForeignPtr ptr $ bgr2rgb len offset
      return $! Just i

-- | Returns a bitmap in bgr format.
getBitmap :: Get Image
getBitmap = do
  skip 18
  width   <- getWord32le
  height  <- getWord32le
  _planes <- getWord16le
  bpp     <- getWord16le
  let size = fromIntegral $
              width * height * (fromIntegral (bpp `div` 8) :: Word32) :: Int
  skip 24
  bgrBytes <- getByteString size
  return $! Image (fromIntegral width)
                  (fromIntegral height)
                  bgrBytes

bgr2rgb :: Int -> Int -> Ptr Word8 -> IO ()
bgr2rgb n o p = do
  mapM_ (\i -> do
    b <- peekElemOff p' (i+0)
    r <- peekElemOff p' (i+2)
    pokeElemOff p' (i+0) (r::Word8)
    pokeElemOff p' (i+2) (b::Word8))
    [0,3..n-3]
  where
  p' = p `plusPtr` o

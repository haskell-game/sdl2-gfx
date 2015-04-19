{-|

Module      : SDL.ImageFilter
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.cc
Stability   : experimental

Bindings to @SDL2_gfx@'s MMX image filter functionality.

-}

module SDL.ImageFilter
  (
  -- * Query MMX
    usingMMX
  , disableMMX
  , enableMMX

  -- * Binary operations
  , add
  , mean
  , sub
  , absDiff
  , mult
  , multNor
  , multDivBy2
  , multDivBy4
  , bitAnd
  , bitOr
  , div
  , bitNegation

  -- , addByte
  -- , addUInt
  -- , addByteToHalf
  -- , subByte
  -- , subUInt
  -- , shiftRight
  -- , shiftRightUInt
  -- , multByByte
  -- , shiftRightAndMultByByte
  -- , shiftLeftByte
  -- , shiftLeftUInt
  -- , shiftLeft
  -- , binarizeUsingThreshold
  -- , clipToRange
  -- , normalizeLinear
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word              (Word8)
import Prelude         hiding (div)
import Foreign.C.Types        (CUChar, CUInt, CInt)
import Foreign.Ptr            (castPtr, Ptr)
import Foreign.Marshal.Alloc  (mallocBytes, finalizerFree)
import Foreign.ForeignPtr     (newForeignPtr)
import Data.Vector.Storable   (Vector)
import System.IO.Unsafe       (unsafePerformIO)

import qualified SDL.Raw.ImageFilter
import qualified Data.Vector.Storable as V

-- type Bytes s = MVector s Word8

-- | Are we using MMX code?
usingMMX :: MonadIO m => m Bool
usingMMX = (==1) <$> SDL.Raw.ImageFilter.mmxDetect

-- | Disable MMX, use non-MMX code instead.
disableMMX :: MonadIO m => m ()
disableMMX = liftIO SDL.Raw.ImageFilter.mmxOff

-- | Use MMX code if available.
enableMMX :: MonadIO m => m ()
enableMMX = liftIO SDL.Raw.ImageFilter.mmxOn

{-# INLINE minLen #-}
minLen :: Integral a => Vector Word8 -> Vector Word8 -> a
minLen x = fromIntegral . min (V.length x) . V.length

mallocVector :: Int -> (Ptr CUChar -> IO a) -> IO (Vector Word8)
mallocVector len act = do
  p <- mallocBytes len
  _ <- act p -- TODO: Check for errors?
  f <- newForeignPtr finalizerFree $ castPtr p
  return $ V.unsafeFromForeignPtr0 f len

binary
  :: (Ptr CUChar -> Ptr CUChar -> Ptr CUChar -> CUInt -> IO CInt) ->
     (Vector Word8 -> Vector Word8 -> Vector Word8)
binary f x y =
  unsafePerformIO .
    V.unsafeWith x $ \x' ->
      V.unsafeWith y $ \y' ->
        mallocVector (minLen x y) $ \z' ->
          f (castPtr x') (castPtr y') z' (minLen x y)

add :: Vector Word8 -> Vector Word8 -> Vector Word8
add = binary SDL.Raw.ImageFilter.add

mean :: Vector Word8 -> Vector Word8 -> Vector Word8
mean = binary SDL.Raw.ImageFilter.mean

sub :: Vector Word8 -> Vector Word8 -> Vector Word8
sub = binary SDL.Raw.ImageFilter.sub

absDiff :: Vector Word8 -> Vector Word8 -> Vector Word8
absDiff = binary SDL.Raw.ImageFilter.absDiff

mult :: Vector Word8 -> Vector Word8 -> Vector Word8
mult = binary SDL.Raw.ImageFilter.mult

multNor :: Vector Word8 -> Vector Word8 -> Vector Word8
multNor = binary SDL.Raw.ImageFilter.multNor

multDivBy2 :: Vector Word8 -> Vector Word8 -> Vector Word8
multDivBy2 = binary SDL.Raw.ImageFilter.multDivBy2

multDivBy4 :: Vector Word8 -> Vector Word8 -> Vector Word8
multDivBy4 = binary SDL.Raw.ImageFilter.multDivBy4

bitAnd :: Vector Word8 -> Vector Word8 -> Vector Word8
bitAnd = binary SDL.Raw.ImageFilter.bitAnd

bitOr :: Vector Word8 -> Vector Word8 -> Vector Word8
bitOr = binary SDL.Raw.ImageFilter.bitOr

div :: Vector Word8 -> Vector Word8 -> Vector Word8
div = binary SDL.Raw.ImageFilter.div

bitNegation :: Vector Word8 -> Vector Word8
bitNegation x =
  unsafePerformIO .
    V.unsafeWith x $ \x' ->
      mallocVector (V.length x) $ \y' ->
        SDL.Raw.ImageFilter.bitNegation
          (castPtr x') y' (fromIntegral $ V.length x)

-- addByte = undefined
-- addUInt = undefined
-- addByteToHalf = undefined
-- subByte = undefined
-- subUInt = undefined
-- shiftRight = undefined
-- shiftRightUInt = undefined
-- multByByte = undefined
-- shiftRightAndMultByByte = undefined
-- shiftLeftByte = undefined
-- shiftLeftUInt = undefined
-- shiftLeft = undefined
-- binarizeUsingThreshold = undefined
-- clipToRange = undefined
-- normalizeLinear = undefined

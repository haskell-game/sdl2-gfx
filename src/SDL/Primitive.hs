{-|

Module      : SDL.Primitive
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.cc
Stability   : experimental

Bindings to @SDL2_gfx@'s primitives drawing functionality. These functions
should allow you to render various simple shapes such as lines, ellipses or
polygons.

All of the monadic functions within this module are capable of throwing an
'SDLException' if they encounter an error.

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module SDL.Primitive where

import Control.Exception      (throwIO)
import Control.Monad          (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits              ((.&.), (.|.))
import Data.ByteString        (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen, unsafePackCString)
import Data.Data              (Data)
import Data.Int               (Int16)
import Data.Text              (Text)
import Data.Text.Encoding     (decodeUtf8)
import Data.Text.Foreign      (lengthWord16, unsafeCopyToPtr)
import Data.Typeable          (Typeable)
import Data.Word              (Word8, Word16)
import Foreign.C.String       (CString, withCString)
import Foreign.C.Types        (CUShort, CInt)
import Foreign.Marshal.Alloc  (allocaBytes, alloca)
import Foreign.Marshal.Utils  (with, fromBool, toBool)
import Foreign.Ptr            (Ptr, castPtr, nullPtr)
import Foreign.Storable       (peek, pokeByteOff)
import GHC.Generics           (Generic)
import Linear                 (V4(..), V2(..))
import SDL                    (Surface(..))
import SDL.Exception          (SDLException, throwIfNeg_)
import SDL.Raw.Filesystem     (rwFromConstMem)
import SDL.Internal.Types     (Renderer(..))

import qualified SDL.Raw
import qualified SDL.Raw.Primitive

type Color = V4 Word8

-- The SDL2_gfx API expects Int16, while SDL2 uses CInt. We could force Int16,
-- but that would cause issues for the end user always having to convert
-- between vector types in order to use both SDL2 and SDL2_gfx. I'm therefore
-- currently opting to accept CInt and convert to Int16, overflows be damned.
cint :: CInt -> Int16
cint = fromIntegral

pixel :: MonadIO m => Renderer -> V2 CInt -> Color -> m ()
pixel (Renderer rndr) (V2 x y) (V4 r g b a) =
  throwIfNeg_ "SDL.Primitive.pixel" "pixelRGBA" $
    SDL.Raw.Primitive.pixel rndr (cint x) (cint y) r g b a

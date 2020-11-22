{-|

Module      : SDL.Raw.ImageFilter
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.eu
Stability   : experimental

Raw bindings to the @SDL2_gfx@ library, specifically the MMX image filter
functionality from @SDL2_imageFilter.h@.

-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE TemplateHaskell #-}

module SDL.Raw.ImageFilter
  ( mmxDetect
  , mmxOff
  , mmxOn
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
  , addByte
  , addUInt
  , addByteToHalf
  , subByte
  , subUInt
  , shiftRight
  , shiftRightUInt
  , multByByte
  , shiftRightAndMultByByte
  , shiftLeft
  , binarizeUsingThreshold
  , clipToRange
  , normalizeLinear
  ) where

import Foreign.C.Types (CUChar(..), CInt(..), CUInt(..))
import Foreign.Ptr     (Ptr)
import Prelude  hiding (div)
import SDL.Raw.Helper  (liftF)

liftF "mmxDetect" "SDL_imageFilterMMXdetect"
  [t|IO CInt|]

liftF "mmxOff" "SDL_imageFilterMMXoff"
  [t|IO ()|]

liftF "mmxOn" "SDL_imageFilterMMXon"
  [t|IO ()|]

liftF "add" "SDL_imageFilterAdd"
  [t|Ptr CUChar -> Ptr CUChar -> Ptr CUChar -> CUInt -> IO CInt|]

liftF "mean" "SDL_imageFilterMean"
  [t|Ptr CUChar -> Ptr CUChar -> Ptr CUChar -> CUInt -> IO CInt|]

liftF "sub" "SDL_imageFilterSub"
  [t|Ptr CUChar -> Ptr CUChar -> Ptr CUChar -> CUInt -> IO CInt|]

liftF "absDiff" "SDL_imageFilterAbsDiff"
  [t|Ptr CUChar -> Ptr CUChar -> Ptr CUChar -> CUInt -> IO CInt|]

liftF "mult" "SDL_imageFilterMult"
  [t|Ptr CUChar -> Ptr CUChar -> Ptr CUChar -> CUInt -> IO CInt|]

liftF "multNor" "SDL_imageFilterMultNor"
  [t|Ptr CUChar -> Ptr CUChar -> Ptr CUChar -> CUInt -> IO CInt|]

liftF "multDivBy2" "SDL_imageFilterMultDivby2"
  [t|Ptr CUChar -> Ptr CUChar -> Ptr CUChar -> CUInt -> IO CInt|]

liftF "multDivBy4" "SDL_imageFilterMultDivby4"
  [t|Ptr CUChar -> Ptr CUChar -> Ptr CUChar -> CUInt -> IO CInt|]

liftF "bitAnd" "SDL_imageFilterBitAnd"
  [t|Ptr CUChar -> Ptr CUChar -> Ptr CUChar -> CUInt -> IO CInt|]

liftF "bitOr" "SDL_imageFilterBitOr"
  [t|Ptr CUChar -> Ptr CUChar -> Ptr CUChar -> CUInt -> IO CInt|]

liftF "div" "SDL_imageFilterDiv"
  [t|Ptr CUChar -> Ptr CUChar -> Ptr CUChar -> CUInt -> IO CInt|]

liftF "bitNegation" "SDL_imageFilterBitNegation"
  [t|Ptr CUChar -> Ptr CUChar -> CUInt -> IO CInt|]

liftF "addByte" "SDL_imageFilterAddByte"
  [t|Ptr CUChar -> Ptr CUChar -> CUInt -> CUChar -> IO CInt|]

liftF "addUInt" "SDL_imageFilterAddUint"
  [t|Ptr CUChar -> Ptr CUChar -> CUInt -> CUInt -> IO CInt|]

liftF "addByteToHalf" "SDL_imageFilterAddByteToHalf"
  [t|Ptr CUChar -> Ptr CUChar -> CUInt -> CUChar -> IO CInt|]

liftF "subByte" "SDL_imageFilterSubByte"
  [t|Ptr CUChar -> Ptr CUChar -> CUInt -> CUChar -> IO CInt|]

liftF "subUInt" "SDL_imageFilterSubUint"
  [t|Ptr CUChar -> Ptr CUChar -> CUInt -> CUInt -> IO CInt|]

liftF "shiftRight" "SDL_imageFilterShiftRight"
  [t|Ptr CUChar -> Ptr CUChar -> CUInt -> CUChar -> IO CInt|]

liftF "shiftRightUInt" "SDL_imageFilterShiftRightUint"
  [t|Ptr CUChar -> Ptr CUChar -> CUInt -> CUChar -> IO CInt|]

liftF "multByByte" "SDL_imageFilterMultByByte"
  [t|Ptr CUChar -> Ptr CUChar -> CUInt -> CUChar -> IO CInt|]

liftF "shiftRightAndMultByByte" "SDL_imageFilterShiftRightAndMultByByte"
  [t|Ptr CUChar -> Ptr CUChar -> CUInt -> CUChar -> CUChar -> IO CInt|]

liftF "shiftLeft" "SDL_imageFilterShiftLeft"
  [t|Ptr CUChar -> Ptr CUChar -> CUInt -> CUChar -> IO CInt|]

liftF "binarizeUsingThreshold" "SDL_imageFilterBinarizeUsingThreshold"
  [t|Ptr CUChar -> Ptr CUChar -> CUInt -> CUChar -> IO CInt|]

liftF "clipToRange" "SDL_imageFilterClipToRange"
  [t|Ptr CUChar -> Ptr CUChar -> CUInt -> CUChar -> CUChar -> IO CInt|]

liftF "normalizeLinear" "SDL_imageFilterNormalizeLinear"
  [t|Ptr CUChar -> Ptr CUChar -> CUInt -> CInt -> CInt -> CInt -> CInt -> IO CInt|]

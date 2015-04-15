{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Framerate

main :: IO ()
main = do

  SDL.initialize [SDL.InitVideo]
  w <- SDL.createWindow "sdl2-gfx-example" SDL.defaultWindow
  SDL.showWindow w

  -- TODO: Example.

  SDL.destroyWindow w
  SDL.quit

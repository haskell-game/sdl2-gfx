{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Framerate

main :: IO ()
main = do

  SDL.initialize [SDL.InitVideo]
  w <- SDL.createWindow "sdl2-gfx-example" SDL.defaultWindow
  SDL.showWindow w

  let fps = 10
  count <- SDL.Framerate.with fps 0 $ \_ count -> do
    print (count :: Int)
    return $
      if count < 10
        then SDL.Framerate.Continue $ count + 1
        else SDL.Framerate.Break count

  putStrLn $ "Final state was " ++ show count ++ "."

  SDL.destroyWindow w
  SDL.quit


module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Counter" (1600, 900) (0, 0)) -- Or FullScreen
              white            -- Background color
              60               -- Frames per second
              beginState{screensize = (1600.0, 900.0)} -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
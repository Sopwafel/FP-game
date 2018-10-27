
module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
              white            -- Background color
              60               -- Frames per second
              beginState       -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
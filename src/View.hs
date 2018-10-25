-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

--viewGameState :: GameState -> Picture

viewPure :: GameState -> Picture
viewPure GameState {gameObjects =Player {image = img, location = (a,b)}} = img
    
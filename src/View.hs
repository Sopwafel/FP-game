-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

--viewGameState :: GameState -> Picture

viewPure :: GameState -> Picture
viewPure GameState {player =Player {image = img, location = (x,y)}} = translate x y img
    
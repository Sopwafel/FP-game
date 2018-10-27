

-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

--viewGameState :: GameState -> Picture


-- | Once we get more gameObjects, we use a Pictures object, like this:
-- | Pictures[picture1, picture2, picture3]
-- | This itself is a Picture, so we can return it from viewPure
viewPure :: GameState -> Picture
viewPure GameState {player =Player {image = img, location = (x,y)}} = translate x y img
    
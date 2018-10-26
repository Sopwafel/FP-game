-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- | This object contains all gameObjects. Is changed every tick by Controller, and drawn every tick by View
data GameState = GameState {
                   player :: Player
                 , pressedKeys :: [Key]
                 }

-- initialState :: GameState
-- initialState = GameState ShowNothing 0

--Field:: (Draw a) => [a]
type GameObjects = Player


-- class Draw a where
    -- Draw :: a ->
    
data Player = Player { image :: Picture, location :: Point}
testPlayer = Player { image = Circle 10.0,  location = (0.0, 0.0)}

beginState = GameState { player = testPlayer, pressedKeys = []}

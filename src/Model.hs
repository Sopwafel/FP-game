-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss

data GameState = GameState {
                   gameObjects :: GameObjects
                 , elapsedTime :: Float
                 }

-- initialState :: GameState
-- initialState = GameState ShowNothing 0

--Field:: (Draw a) => [a]
type GameObjects = Player

-- class Draw a where
    -- Draw :: a ->
    
data Player = Player { image :: Picture, location :: Point}
testPlayer = Player { image = Circle 10.0,  location = (10.0, 10.0)}

beginState = GameState { gameObjects = testPlayer, elapsedTime = 0.0}

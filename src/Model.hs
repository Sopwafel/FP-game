-- | This module contains the data types
--   which represent the state of the game
module Model where


data GameState = GameState {
                   gameObjects :: GameObjects
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState ShowNothing 0

--Field:: (Pic a) => [a]
type GameObjects = [a]

class Draw a where
    Draw :: a ->
    
data Player = Player { image :: Picture, location :: Point}
testPlayer = Player { image = Circle 10f,  location = (10f, 10f)}

beginState = GameState { [testPlayer], 0f}

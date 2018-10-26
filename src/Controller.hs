-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

nO_SECS_BETWEEN_CYCLES =1

-- | Handle one iteration of the game
newStep :: Float -> GameState -> IO GameState
newStep secs gstate = return gstate

step :: Float -> GameState -> IO GameState
step secs gstate = return (doPressedKeys gstate)
  -- | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  -- = return gstate
  -- | otherwise
  -- = -- Just update the elapsed time
    -- return $ gstate --{ elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)


-- | TODO: make these functions only add a Key to the pressedKey field
-- | Because currently we only move on keyPress and keyUp events, not while the key is being held
inputKey :: Event -> GameState -> GameState
inputKey (EventKey key keyState _ _) gstate@GameState{player = pl@Player{location = (x,y)}, keyList = keylist}
  | key == (SpecialKey KeyUp) && keyState == Up = gstate {player = pl {location = (x,(y+10))}, keyList = keylist}   -- And add keyUp to  keyList
  | key == (SpecialKey KeyUp) && keyState == Down = gstate {keyList = keylist}                                      -- Remove keyUp from keylist
  | otherwise = gstate
    --gstate { infoToShow = ShowAChar c }
inputKey _ gstate = gstate -- Otherwise keep the same


-- | TODO: fix type error. We need to do keyBeingPressed on gstate for all keys in keyList
-- | but now we get an array with a gstate for every key
doPressedKeys :: GameState -> GameState
doPressedKeys gstate@GameState{pressedKeys = keyList} = map (keyBeingPressed gstate) keyList 

-- | I was thinking we could map this function over the keyPressed array
-- | And change the gamestate for each key that's being held
keyBeingPressed :: GameState -> Key -> GameState
keyBeingPressed gstate@GameState{player = pl@Player{location = (x,y)}} (SpecialKey KeyUp)    = gstate {player = pl {location = (x,(y+10))}}
keyBeingPressed gstate@GameState{player = pl@Player{location = (x,y)}} (SpecialKey KeyDown)  = undefined
keyBeingPressed gstate@GameState{player = pl@Player{location = (x,y)}} (SpecialKey KeyRight) = undefined
keyBeingPressed gstate@GameState{player = pl@Player{location = (x,y)}} (SpecialKey KeyLeft)  = undefined
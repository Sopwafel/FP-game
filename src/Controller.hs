

-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List
import Data.Maybe

nO_SECS_BETWEEN_CYCLES =1

stepSize = 5

-- | Handle one iteration of the game
newStep :: Float -> GameState -> IO GameState
newStep secs gstate = return gstate

-- | Updates the gamestate
step :: Float -> GameState -> IO GameState
step secs gstate = return (moveEnemyBullets (updateEnemies (movePlayerBullets (doPressedKeys (handleWaves gstate)))))

-- || Update gamestate fields ########################################################################################### | --
movePlayerBullets :: GameState -> GameState
movePlayerBullets gstate@GameState{friendlyBullets = pBullets} = gstate {friendlyBullets = (map moveABullet pBullets)}

moveEnemyBullets :: GameState -> GameState
moveEnemyBullets gstate@GameState{enemyBullets = enemyBullets} = gstate {enemyBullets = (map moveABullet enemyBullets)}

-- | Moves enemies, updates their shot cooldown and spawns bullets if necessary
updateEnemies :: GameState -> GameState
updateEnemies gstate@GameState{enemies = enemies, enemyBullets = enemyBullets} = gstate {enemies = (map updateAnEnemy enemies), enemyBullets = (spawnBullets enemies enemyBullets)}

-- | Updates spawn cooldown for waves and spawns enemies if necessary
handleWaves :: GameState -> GameState
handleWaves gstate@GameState{enemies = enemies, waves = waves} = gstate {waves = newWaves, enemies = (spawnEnemies newWaves enemies)}
    where
        newWaves = (mapMaybe updateAWave waves)

-- | Puts all enemies that should be spawned by [Wave] this step in [Enemy]
spawnEnemies :: [Wave] -> [Enemy] -> [Enemy]
spawnEnemies [] enemies = enemies
spawnEnemies (x:xs) enemies
    | waveNeedsSpawn x = spawnEnemies xs ((nextEnemy x) : enemies)
    | otherwise        = spawnEnemies xs enemies

-- | Puts all bullets that should be spawned by [Enemy] this step in [Bullet]
spawnBullets :: [Enemy] -> [Bullet] -> [Bullet]
spawnBullets [] bullets = bullets
spawnBullets (x:xs) bullets
    | enemyCanShoot x = spawnBullets xs ((enemyShoots x) : bullets)
    | otherwise       = spawnBullets xs bullets
    
-- | Functions that handle a single object (for map)
-- | TODO: ask how this can be done betterly
updateAWave :: Wave -> Maybe Wave
updateAWave wave@Wave{stepCounter = stepCounter, interval = interval, totalEnemies = totalEnemies, enemyCounter = enemyCounter}
    | totalEnemies == enemyCounter = Nothing
    | interval == stepCounter = Just wave {stepCounter = 0, enemyCounter = enemyCounter +1}
    | otherwise = Just wave {stepCounter = stepCounter + 1}
    
-- | Moves a bullet according to its path
moveABullet :: Bullet -> Bullet
moveABullet bullet@Bullet{location = (x,y), path = StraightPath v} = bullet {location = (x+v, y)} :: Bullet

-- | Updates an enemies shot cooldown and location
updateAnEnemy :: Enemy -> Enemy
updateAnEnemy enemy@Enemy{location   = (x,y), path = StraightPath v, shotCooldownCounter = shotCooldownCounter, shotCooldown = shotCooldown}
    | shotCooldownCounter < shotCooldown = enemy  {location = (x+v, y), shotCooldownCounter = shotCooldownCounter +1} :: Enemy
    | otherwise = enemy  {location = (x+v, y), shotCooldownCounter = 0} :: Enemy

-- || User input ######################################################################################################## | --
    
-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)


-- | TODO: make these functions only add a Key to the pressedKey field
-- | Because currently we only move on keyPress and keyUp events, not while the key is being held
inputKey :: Event -> GameState -> GameState
inputKey (EventKey key keyState _ _) gstate@GameState{player = p, pressedKeys = list, waves = waves}
    | keyState == Down && key == (Char 's') =  gstate{waves = testWave : waves}
    | keyState == Down   = gstate {pressedKeys = newlist}
    | keyState == Up     = gstate {pressedKeys = filterlist}
    | otherwise = gstate
    where
        newlist    = key : list
        filterlist = filter ((/=) key) list
    --gstate { infoToShow = ShowAChar c }
inputKey _ gstate = gstate -- Otherwise keep the same


-- | Acts out all keys that are currently pressed
doPressedKeys :: GameState -> GameState
doPressedKeys gstate@GameState{pressedKeys = keyList} = foldr keyBeingPressed gstate keyList 

-- | I was thinking we could map this function over the keyPressed array
-- | And change the gamestate for each key that's being held
keyBeingPressed :: Key -> GameState -> GameState
keyBeingPressed key gstate@GameState{player = pl@Player{location   = (x,y), bullet = pBullet}}
    | key == (SpecialKey KeyUp)    = gstate {player = pl {location = (x,(y+stepSize))}}
    | key == (SpecialKey KeyDown)  = gstate {player = pl {location = (x,(y-stepSize))}}
    | key == (SpecialKey KeyRight) = gstate {player = pl {location = ((x+stepSize),y)}}
    | key == (SpecialKey KeyLeft)  = gstate {player = pl {location = ((x-stepSize),y)}}
    | key == (Char 'a')            = addPlayerBullet gstate
    | otherwise = gstate

-- | Spawns a player bullet every step it is called
addPlayerBullet :: GameState -> GameState
addPlayerBullet gstate@GameState{player = pl@Player{location = (x,y), bullet = pBullet}, friendlyBullets = friendBullets} = 
    gstate {friendlyBullets = (newBullet : friendBullets)}
    where
        newBullet = pBullet {location = (x,y)} :: Bullet



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
step secs gstate = return (updateEXPLOSIONS (checkCollisions (moveEnemyBullets (updateEnemies (movePlayerBullets (doPressedKeys (handleWaves gstate)))))))

-- || Collision checks ################################################################################################## | --
-- |
checkCollisions :: GameState -> GameState
checkCollisions gstate = enemyCollision (playerCollision gstate)

-- | Check if the player gets hit by a bullet
playerCollision :: GameState -> GameState
playerCollision gstate@GameState{player = player, enemyBullets = bullets} = gstate{player = (collideBulletsWithObject bullets player)}

-- | Check if enemies get hit by bullets
enemyCollision :: GameState -> GameState
enemyCollision gstate@GameState{enemies = enemies, friendlyBullets = bullets} = gstate{enemies = (map (collideBulletsWithObject bullets) enemies)}

collideBulletsWithObject :: (Collideable a, DamageAble a) => [Bullet] -> a -> a
collideBulletsWithObject [] a = a
collideBulletsWithObject (x:xs) a
    | collides a x = collideBulletsWithObject xs (doDamage a x)
    | otherwise = collideBulletsWithObject xs a

-- || Update gamestate fields ########################################################################################### | --
movePlayerBullets :: GameState -> GameState
movePlayerBullets gstate@GameState{friendlyBullets = pBullets} = gstate {friendlyBullets = (mapMaybe update pBullets)}

moveEnemyBullets :: GameState -> GameState
moveEnemyBullets gstate@GameState{enemyBullets = enemyBullets} = gstate {enemyBullets = (mapMaybe update enemyBullets)}

 -- | Moves enemies, updates their shot cooldown and spawns bullets or explosions if necessary
updateEnemies :: GameState -> GameState
updateEnemies gstate@GameState{enemies = enemies, enemyBullets = enemyBullets, explosions = explosions} = gstate {enemies = (mapMaybe update enemies), enemyBullets = (spawnBullets enemies enemyBullets), explosions = ((explodeEnemies enemies) ++ explosions)}

explodeEnemies :: [Enemy] -> [Explosion]
explodeEnemies [] = []
explodeEnemies (e@Enemy{health = h}:xs)
    | h < 0 = explode e : explodeEnemies xs
    | otherwise = explodeEnemies xs
    

 -- | Updates spawn cooldown for waves and spawns enemies if necessary
handleWaves :: GameState -> GameState
handleWaves gstate@GameState{enemies = enemies, waves = waves} = gstate {waves = newWaves, enemies = (spawnEnemies newWaves enemies)}
    where
        newWaves = (mapMaybe updateAWave waves)

-- | Update the list of EXPLOSIONS!!!
updateEXPLOSIONS :: GameState -> GameState
updateEXPLOSIONS gstate@GameState{explosions = ex} = gstate {explosions = mapMaybe update ex}

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

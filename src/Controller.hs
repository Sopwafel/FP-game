

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

-- | Updates the gamestate
step :: Float -> GameState -> IO GameState
step secs gstate = return (checkCollisions (updateFields (doPressedKeys gstate)))

updateFields :: GameState -> GameState
updateFields gstate = updatePowerUps(updateEXPLOSIONS (updateEnemyBullets (updateEnemies (updatePlayerBullets (updateWaves gstate)))))

-- || Collision checks ################################################################################################## | --
-- | Checks enemy collision with player bullets and player collision with enemy bullets
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

--collidePowerUpsWithPlayer :: Player -> [PowerUp] -> Player

-- || Update gamestate fields ########################################################################################### | --

updatePowerUps :: GameState -> GameState
updatePowerUps gstate@GameState{powerUps = powerUps} = gstate {powerUps = (mapMaybe (update gstate) powerUps)}

updatePlayerBullets :: GameState -> GameState
updatePlayerBullets gstate@GameState{friendlyBullets = pBullets} = gstate {friendlyBullets = (mapMaybe (update gstate) pBullets)}

updateEnemyBullets :: GameState -> GameState
updateEnemyBullets gstate@GameState{enemyBullets = enemyBullets} = gstate {enemyBullets = (mapMaybe (update gstate) enemyBullets)}

 -- | Moves enemies, updates their shot cooldown and spawns bullets or explosions if necessary
updateEnemies :: GameState -> GameState
updateEnemies gstate@GameState{enemies = enemies, enemyBullets = enemyBullets, explosions = explosions, player = Player{location = ploc}, score = score, screensize = screensize} = 
    gstate {enemies = (mapMaybe (update gstate) enemies), 
    enemyBullets = (spawnBullets enemies enemyBullets ploc), 
    explosions = ((explodeEnemies enemies) ++ explosions),
    score = score + (scoreEnemies enemies)}

 -- | Updates spawn cooldown for waves and spawns enemies if necessary
updateWaves :: GameState -> GameState
updateWaves gstate@GameState{enemies = enemies, waves = waves, screensize = screensize} = gstate {waves = newWaves, enemies = (spawnEnemies newWaves enemies)}
    where
        newWaves = (mapMaybe (update gstate) waves)
        
-- | Update the list of EXPLOSIONS!!!
updateEXPLOSIONS :: GameState -> GameState
updateEXPLOSIONS gstate@GameState{explosions = ex} = gstate {explosions = mapMaybe (update gstate) ex }

-- | Enemies that are damaged to below 0 hp stay in the list for one step, and in this time they can be scored
scoreEnemies :: [Enemy] -> Int
scoreEnemies [] = 0
scoreEnemies (Enemy{health = h, score = score}:xs)
    | h < 0 = score + scoreEnemies xs
    | otherwise = scoreEnemies xs
    
-- || Spawn stuff ######################################################################################################## | --

-- | Puts all enemies that should be spawned by [Wave] this step in [Enemy]
spawnEnemies :: [Wave] -> [Enemy] -> [Enemy]
spawnEnemies [] enemies = enemies
spawnEnemies (x:xs) enemies
    | waveNeedsSpawn x = spawnEnemies xs ((nextEnemy x) : enemies)
    | otherwise        = spawnEnemies xs enemies

 -- | Puts all bullets that should be spawned by [Enemy] this step in [Bullet]. Point is the location of the player
spawnBullets :: [Enemy] -> [Bullet] -> Point -> [Bullet]
spawnBullets [] bullets point = bullets
spawnBullets (x:xs) bullets point
    | enemyCanShoot x = spawnBullets xs ((enemyShoots x point) : bullets) point
    | otherwise       = spawnBullets xs bullets point
    
-- | Spawn an explosion on every < 0 hp enemy
explodeEnemies :: [Enemy] -> [Explosion]
explodeEnemies [] = []
explodeEnemies (e@Enemy{health = h}:xs)
    | h < 0 = explode e : explodeEnemies xs
    | otherwise = explodeEnemies xs

-- || User input ######################################################################################################## | --
    
-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)


-- | TODO: make these functions only add a Key to the pressedKey field
-- | Because currently we only move on keyPress and keyUp events, not while the key is being held
inputKey :: Event -> GameState -> GameState
inputKey (EventKey key keyState _ _) gstate@GameState{player = p, pressedKeys = list, waves = waves, powerUps = powerUps}
    | keyState == Down && key == (Char 's') =  gstate{waves = testWave : waves}     -- Spawn testwave
    | keyState == Down && key == (Char 'p') =  gstate{powerUps = testPowerUp : powerUps}
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
keyBeingPressed key gstate@GameState{player = pl@Player{location   = (x,y), bullet = pBullet}, screensize = (width, height)}
    | x >= (width/2)   && key == (SpecialKey KeyRight) = gstate
    | x <= -(width/2)  && key == (SpecialKey KeyLeft)  = gstate
    | y >= (height/2)  && key == (SpecialKey KeyUp)    = gstate
    | y <= -(height/2) && key == (SpecialKey KeyDown)  = gstate
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

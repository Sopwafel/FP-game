

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
step secs gstate@PlayingState{} = return (checkCollisions (updateFields (doPressedKeys gstate)))
step secs gstate@MenuState{}    = return (doPressedKeys gstate)
srep secs gstate@PausedState{}  = return (doPressedKeys gstate)

updateFields :: GameState -> GameState
updateFields gstate = updatePowerUps(updateEXPLOSIONS (updateEnemyBullets (updateEnemies (updatePlayerBullets (updateWaves gstate)))))

-- || Collision checks ################################################################################################## | --
-- | Checks enemy collision with player bullets and player collision with enemy bullets
checkCollisions :: GameState -> GameState
checkCollisions gstate = collidePowerUpsWithPlayer (enemyCollision (playerCollision gstate))

-- | Check if the player gets hit by a bullet
playerCollision :: GameState -> GameState
playerCollision gstate@PlayingState{player = player, enemyBullets = bullets} = gstate{player = (collideBulletsWithObject bullets player), enemyBullets = (removeCollidedBullets bullets player)}

-- | Check if enemies get hit by bullets
enemyCollision :: GameState -> GameState
enemyCollision gstate@PlayingState{enemies = enemies, friendlyBullets = bullets} = gstate{enemies = (map (collideBulletsWithObject bullets) enemies), friendlyBullets = newFriendlyBullets}
    where
        newFriendlyBullets = removeCollidedBulletsList bullets enemies    

collideBulletsWithObject :: (Collideable a, DamageAble a) => [Bullet] -> a -> a
collideBulletsWithObject [] a = a
collideBulletsWithObject (x:xs) a
    | collides a x = collideBulletsWithObject xs (doDamage a x)
    | otherwise = collideBulletsWithObject xs a

removeCollidedBulletsList :: (Collideable a, DamageAble a) => [Bullet] -> [a] -> [Bullet]
removeCollidedBulletsList bullets [] = bullets
removeCollidedBulletsList bullets (x:xs) = removeCollidedBulletsList (removeCollidedBullets bullets x) xs

removeCollidedBullets :: (Collideable a, DamageAble a) => [Bullet] -> a -> [Bullet]
removeCollidedBullets [] _ = []
removeCollidedBullets (x:xs) a
    | collides a x = removeCollidedBullets xs a
    | otherwise = x : removeCollidedBullets xs a

-- | Powers up the player and removes collected powerUps
collidePowerUpsWithPlayer :: GameState -> GameState
collidePowerUpsWithPlayer gstate@PlayingState{player = player, powerUps = gamePowerUps} = gstate{player = newPlayer, powerUps = updatedGamePowerUpList}
    where 
        updatedGamePowerUpList = mapMaybe (removeIfCollides player) gamePowerUps
        newPlayer = powerUpPlayer gamePowerUps player 

powerUpPlayer :: [PowerUp] -> Player -> Player
powerUpPlayer [] a = a
powerUpPlayer (x:xs) a
    | collides a x = powerUpPlayer xs (powerUp a x)
    | otherwise = powerUpPlayer xs a


-- || Update gamestate fields ########################################################################################### | --

updatePowerUps :: GameState -> GameState
updatePowerUps gstate@PlayingState{powerUps = powerUps} = gstate {powerUps = (mapMaybe (update gstate) powerUps)}

updatePlayerBullets :: GameState -> GameState
updatePlayerBullets gstate@PlayingState{friendlyBullets = pBullets} = gstate {friendlyBullets = (mapMaybe (update gstate) pBullets)}

updateEnemyBullets :: GameState -> GameState
updateEnemyBullets gstate@PlayingState{enemyBullets = enemyBullets} = gstate {enemyBullets = (mapMaybe (update gstate) enemyBullets)}

 -- | Moves enemies, updates their shot cooldown and spawns bullets or explosions if necessary
updateEnemies :: GameState -> GameState
updateEnemies gstate@PlayingState{enemies = enemies, enemyBullets = enemyBullets, explosions = explosions, player = Player{location = ploc}, score = score, screensize = screensize} = 
    gstate {enemies = (mapMaybe (update gstate) enemies), 
    enemyBullets = (spawnBullets enemies enemyBullets ploc), 
    explosions = ((explodeEnemies enemies) ++ explosions),
    score = score + (scoreEnemies enemies)}

 -- | Updates spawn cooldown for waves and spawns enemies if necessary
updateWaves :: GameState -> GameState
updateWaves gstate@PlayingState{enemies = enemies, waves = waves, screensize = screensize} = gstate {waves = newWaves, enemies = (spawnEnemies newWaves enemies)}
    where
        newWaves = (mapMaybe (update gstate) waves)
        
-- | Update the list of EXPLOSIONS!!!
updateEXPLOSIONS :: GameState -> GameState
updateEXPLOSIONS gstate@PlayingState{explosions = ex} = gstate {explosions = mapMaybe (update gstate) ex }

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
inputKey (EventKey key keyState _ _) gstate@PlayingState{player = p, pressedKeys = list, waves = waves, powerUps = powerUps}
    | keyState == Down && key == (Char 's') =  gstate{waves = testWave : waves}     -- Spawn testwave
    | keyState == Down && key == (Char 'o') =  gstate{powerUps = testPowerUp : powerUps}
    | keyState == Down   = gstate {pressedKeys = newlist}
    | keyState == Up     = gstate {pressedKeys = filterlist}
    | otherwise = gstate
    where
        newlist    = key : list
        filterlist = filter ((/=) key) list
    --gstate { infoToShow = ShowAChar c }
inputKey (EventKey key keyState _ _) gstate@MenuState{pressedKeys = list}
    | keyState == Down   = gstate {pressedKeys = newlist}
    | keyState == Up     = gstate {pressedKeys = filterlist}
    | otherwise = gstate
    where
        newlist    = key : list
        filterlist = filter ((/=) key) list
inputKey _ gstate = gstate -- Otherwise keep the same


-- | Acts out all keys that are currently pressed
doPressedKeys :: GameState -> GameState
-- would be nice if the line below worked, but i dont understand the error so commented line it is
-- doPressedKeys gstate {pressedKeys = keyList} = foldr keyBeingPressed gstate keyList
doPressedKeys gstate@PlayingState{pressedKeys = keyList} = foldr keyBeingPressed gstate keyList 
doPressedKeys gstate@MenuState{pressedKeys = keyList}    = foldr keyBeingPressed gstate keyList
doPressedKeys gstate@PausedState{pressedKeys = keyList}  = foldr keyBeingPressed gstate keyList

-- | I was thinking we could map this function over the keyPressed array
-- | And change the gamestate for each key that's being held
keyBeingPressed :: Key -> GameState -> GameState
keyBeingPressed key gstate@PlayingState{player = pl@Player{location   = (x,y), size = sz}, screensize = (width, height), pressedKeys = keys}
    | x >= (width/2 - (fromIntegral sz))   && key == (SpecialKey KeyRight) = gstate
    | x <= -(width/2 - (fromIntegral sz))  && key == (SpecialKey KeyLeft)  = gstate
    | y >= (height/2 - (fromIntegral sz))  && key == (SpecialKey KeyUp)    = gstate
    | y <= -(height/2 - (fromIntegral sz)) && key == (SpecialKey KeyDown)  = gstate
    | key == (SpecialKey KeyUp)    = gstate {player = pl {location = (x,(y+stepSize))}}
    | key == (SpecialKey KeyDown)  = gstate {player = pl {location = (x,(y-stepSize))}}
    | key == (SpecialKey KeyRight) = gstate {player = pl {location = ((x+stepSize),y)}}
    | key == (SpecialKey KeyLeft)  = gstate {player = pl {location = ((x-stepSize),y)}}
    | key == (Char 'a')            = addPlayerBullet gstate
    | key == (Char 'p')            = PausedState {unpause = key, gameState = gstate, text = [OnScreenText{location = (-450, 400), scale = 1.0, text = "Press p to unpause"}], pressedKeys = keys}
    | otherwise = gstate
keyBeingPressed key gstate@MenuState{buttons = buttons} = checkButtons key buttons gstate
keyBeingPressed key gstate@PausedState{unpause = p, gameState = state}
    | key == p  = state
    | otherwise = gstate

checkButtons :: Key -> [Button] -> GameState -> GameState
checkButtons key (x@Button{key = thing, switchto = next} : xs) gstate
    | key == thing = next
    | otherwise    = gstate

-- | Spawns a player bullet every step it is called
addPlayerBullet :: GameState -> GameState
addPlayerBullet gstate@PlayingState{player = pl@Player{location = (x,y), bullet = pBullet, powerUps = powerUps}, friendlyBullets = friendBullets} = 
    gstate {friendlyBullets = (newBullet : friendBullets)}
    where
        newBullet = pBullet {location = (x,y)} :: Bullet

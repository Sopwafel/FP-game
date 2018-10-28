

-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game



-- | This object contains all gameObjects. Is changed every tick by Controller, and drawn every tick by View
data GameState = GameState {
                   player :: Player
                 , pressedKeys :: [Key]         -- All keys that are currently pressed down.
                 , enemies :: [Enemy]           -- All enemies. TODO: Move every step, check for collission with player, make shoot
                 , friendlyBullets :: [Bullet]  -- All friendly bullets. Get moved every step. TODO: Check for collission with enemies
                 , enemyBullets :: [Bullet]     -- All enemy bullets. Get moved every step. TODO: check collission with player
                 , waves :: [Wave]              -- Every step, all waves are evaluated and updated. If necessary, an enemy is spawned from them.
                 }

-- | Pictures!
square = Polygon [(-2,-2),(2,-2),(2,2),(-2,2),(-2,-2)]

-- || Game Data Types ################################################################################################### | --

-- | This works because of the DuplicateRecordFields extension
data Bullet = Bullet {location :: Point, damage :: Int, image :: Picture, path :: ObjectPath}

data Enemy  = Enemy {location :: Point, health :: Int, image :: Picture, path :: ObjectPath, bullet :: Bullet, shotCooldown :: Int, shotCooldownCounter :: Int } 
enemyCanShoot :: Enemy -> Bool
enemyCanShoot Enemy{shotCooldown = shotCooldown, shotCooldownCounter = shotCooldownCounter} = shotCooldown == shotCooldownCounter
enemyShoots :: Enemy -> Bullet
enemyShoots Enemy{location = location, bullet = bullet} = bullet {location = location}

-- | The kinds of paths a bullet or enemy can follow
data ObjectPath = StraightPath Float       -- Float is speed
    | AimedPath    Float Float             -- First Float is speed, second has range -1..1 and is the direction.
    | HomingPath   Float Float Float       -- First Float is speed, second has range -1..1 and is the direction, third has range 0..1 and is turning rate

data Player = Player { image :: Picture, location :: Point, bullet :: Bullet, shotCooldown :: Int}

data Explosion = Explosion {location :: Point, size :: Int}
    
-- || Wave logic ######################################################################################################## | --

-- | stepCounter gets +1 every step, and once counter == interval, the next enemy is spawned. 
data Wave = Wave{pattern :: SpawnPattern, enemies :: [Enemy], interval :: Int, enemyCounter :: Int, stepCounter :: Int, totalEnemies :: Int }
   
-- | Should be in -1..1 range. Spawns n enemies on 1/points of the screen height at the right of the screen 
data SpawnPattern = SpawnPattern [Float]

-- | Evaluates a wave and returns the correct Enemy in the correct spot
nextEnemy :: Wave -> Enemy
nextEnemy Wave{enemyCounter = n, enemies = enemies, pattern = (SpawnPattern np)} = testEnemy{location = (800, (450*(np!!patternIndex)))}
    where
        enemyIndex   = (length enemies) `mod` n
        patternIndex = n `mod` (length np)
       
waveNeedsSpawn :: Wave -> Bool
waveNeedsSpawn Wave{interval = interval, stepCounter = stepCounter} = interval == stepCounter

-- || Objects ########################################################################################################### | --
-- | These are actual objects with values filled in | --
spawnPattern1 = SpawnPattern [-0.5, 0.0, 0.5]
testEnemy     = Enemy {image = color red (ThickCircle 5.0 5.0), path = StraightPath (-3.0), bullet = testBullet {path = StraightPath (-5.0)}, shotCooldown = 30, shotCooldownCounter = 0}
testBullet    = Bullet {damage = 1, image = Circle 1.0, path = StraightPath 5.0}
testPlayer    = Player { image = color blue (ThickCircle 5.0 10.0),  location = (0.0, 0.0), bullet = testBullet, shotCooldown = 10}
testWave      = Wave {pattern = spawnPattern1, enemies = [testEnemy], interval = 30, enemyCounter = 1, stepCounter = 0, totalEnemies = 5}
beginState    = GameState { player = testPlayer, pressedKeys = [], enemies = [], friendlyBullets = [], enemyBullets = [], waves = []}


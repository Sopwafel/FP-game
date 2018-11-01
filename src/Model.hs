

-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game



-- | This object contains all gameObjects. Is changed every tick by Controller, and drawn every tick by View
data GameState = GameState {
                   player          :: Player
                 , pressedKeys     :: [Key]       -- All keys that are currently pressed down.
                 , enemies         :: [Enemy]     -- All enemies. TODO: Move every step, check for collission with player, make shoot
                 , friendlyBullets :: [Bullet]    -- All friendly bullets. Get moved every step. TODO: Check for collission with enemies
                 , enemyBullets    :: [Bullet]    -- All enemy bullets. Get moved every step. TODO: check collission with player
                 , waves           :: [Wave]      -- Every step, all waves are evaluated and updated. If necessary, an enemy is spawned from them.
				 , explosions      :: [Explosion] -- All EXPLOSIONS currently in the game.
                 }

-- | Pictures!
square = Polygon [(-2,-2),(2,-2),(2,2),(-2,2),(-2,-2)]

-- || Type Classes and instances ######################################################################################### | --
-- | Returns a picture of a drawable object
class Draw d where
    draw            :: d -> Picture
    
instance Draw Bullet where
    draw Bullet {image = img, location = (x,y)} = (translate x y img)
instance Draw Enemy where
    draw Enemy {image = img, location = (x,y)} = (translate x y img)
    
-- | Updates an object for 1 step
class Update a where
    update          :: a -> Maybe a

instance Update Bullet where
    update bullet@Bullet{location = (x,y), path = StraightPath v} = Just (bullet {location = (x+v, y)} :: Bullet)    
instance Update Enemy where
    update enemy@Enemy{location   = (x,y), path = StraightPath v, shotCooldownCounter = shotCooldownCounter, shotCooldown = shotCooldown, health = h}
        | h < 0 = Nothing
        | shotCooldownCounter < shotCooldown = Just enemy  {location = (x+v, y), shotCooldownCounter = shotCooldownCounter +1} :: Maybe Enemy
        | otherwise = Just enemy  {location = (x+v, y), shotCooldownCounter = 0} :: Maybe Enemy
instance Update Explosion where
    update boom@Explosion{countdown = count, location = (x, y), velocity = (x2, y2)}
        | count > 0 = Just boom {countdown = count - 1, location = ((x + x2), (y + y2))}
        | otherwise = Nothing

-- | An object that can collide with another object
class Collideable a where
    loc             :: a -> Point
    hitboxSize      :: a -> Int

instance Collideable Bullet where
    loc Bullet{location = a} = a
    hitboxSize Bullet{size = a} = a
instance Collideable Enemy where
    loc Enemy{location = a} = a
    hitboxSize Enemy{size = a} = a
instance Collideable Player where
    loc Player{location = a} = a
    hitboxSize Player{size = a} = a
    
-- | A thing that does damage
class Damage a where
    damage          :: a -> Int
    
instance Damage Bullet where
    damage Bullet{damagePoints = d} = d
-- | A thing that can take damage
class DamageAble a where
    doDamage        :: (Damage b) => a -> b -> a
    explode         :: a -> Explosion
    
instance DamageAble Enemy where
    doDamage enemy@Enemy{health = h} bullet = enemy {health = (h-(damage bullet))}
    explode enemy@Enemy{location = location} = testExplosion {location = location }
instance DamageAble Player where
    doDamage p@Player{health = h} bullet = p {health = (h-(damage bullet))}
    explode player@Player{location = location} = testExplosion {location = location}
    
-- | 
collides :: (Collideable a, Collideable b) => a -> b -> Bool
collides a b = collideHelper (loc a) (loc b) (fromIntegral((hitboxSize a) `div` 2)) (fromIntegral((hitboxSize b) `div` 2))
collideHelper :: Point -> Point -> Float -> Float -> Bool
collideHelper (x,y) (x2,y2) s1 s2 = x < (x2+s2) && (x+s1) > x2 && y < (y2+s2) && (y+s1) > y2

-- || Instances########################################################################################################## | --
-- | Bullet




    
-- | Enemy




-- | Player


    
-- || Game Data Types ################################################################################################### | --

-- | This works because of the DuplicateRecordFields extension
data Bullet = Bullet {location :: Point, damagePoints :: Int, image :: Picture, path :: ObjectPath, size :: Int}
  
data Enemy  = Enemy {location :: Point, health :: Int, image :: Picture, path :: ObjectPath, bullet :: Bullet, shotCooldown :: Int, shotCooldownCounter :: Int, size :: Int } 
       
enemyCanShoot :: Enemy -> Bool
enemyCanShoot Enemy{shotCooldown = shotCooldown, shotCooldownCounter = shotCooldownCounter} = shotCooldown == shotCooldownCounter
enemyShoots :: Enemy -> Bullet
enemyShoots Enemy{location = location, bullet = bullet} = bullet {location = location}

-- | The kinds of paths a bullet or enemy can follow
data ObjectPath = StraightPath Float       -- Float is speed
    | AimedPath    Float Float             -- First Float is speed, second has range -1..1 and is the direction.
    | HomingPath   Float Float Float       -- First Float is speed, second has range -1..1 and is the direction, third has range 0..1 and is turning rate

data Player = Player { image :: Picture, location :: Point, bullet :: Bullet, shotCooldown :: Int, size :: Int, health :: Int}

data Explosion = Explosion {location :: Point, scale :: Float, countdown :: Int, velocity :: Point}

    
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
testEnemy     = Enemy {health = 1, image = color black (ThickCircle 5.0 5.0), path = StraightPath (-3.0), bullet = testBullet {path = StraightPath (-5.0)}, shotCooldown = 30, shotCooldownCounter = 0, size = 10}
testBullet    = Bullet {damagePoints = 1, image = Circle 2.0, path = StraightPath 5.0, size = 20}
testPlayer    = Player { health = 10, image = color black (ThickCircle 5.0 10.0),  location = (0.0, 0.0), bullet = testBullet, shotCooldown = 10, size = 10}
testWave      = Wave {pattern = spawnPattern1, enemies = [testEnemy], interval = 30, enemyCounter = 1, stepCounter = 0, totalEnemies = 5}
testExplosion = Explosion { scale = 100.0, countdown = 300, velocity = (0.0,0.0)}
beginState    = GameState { player = testPlayer, pressedKeys = [], enemies = [], friendlyBullets = [], enemyBullets = [], waves = [], explosions = []}


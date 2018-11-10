

-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

screenSize = (1600.0, 900.0)

-- | This object contains all gameObjects. Is changed every tick by Controller, and drawn every tick by View
data GameState = PlayingState {
                   player          :: Player
                 , pressedKeys     :: [Key]       -- All keys that are currently pressed down.
                 , enemies         :: [Enemy]     -- All enemies. TODO: Move every step, check for collission with player, make shoot
                 , friendlyBullets :: [Bullet]    -- All friendly bullets. Get moved every step. TODO: Check for collission with enemies
                 , enemyBullets    :: [Bullet]    -- All enemy bullets. Get moved every step. TODO: check collission with player
                 , waves           :: [Wave]      -- Every step, all waves are evaluated and updated. If necessary, an enemy is spawned from them.
                 , explosions      :: [Explosion] -- All EXPLOSIONS currently in the game.
				 , screensize      :: Point       -- Size of the widow
                 , score           :: Int
                 , powerUps        :: [PowerUp]
                 }
                | MenuState {
		           screensize  :: Point
                 , buttons     :: [Button]
				 , pressedKeys :: [Key]
				 , text        :: [OnScreenText]
		         }
				| PausedState {
				   unpause     :: Key
				 , gameState   :: GameState
				 , text        :: [OnScreenText]
				 , pressedKeys :: [Key]
				 }

-- || Type Classes and instances ######################################################################################### | --
-- | Returns a picture of a drawable object
-- | Instances of this class have been put in View.hs
class Draw d where
    draw            :: d -> Picture
    

    
-- | Updates an object for 1 step.
class Update a where
    update          :: GameState -> a -> Maybe a

instance Update Bullet where
    update PlayingState{screensize = screensize, player = Player{location = playerLocation}} bullet@Bullet{location = location, path = path}  
        | outOfScreen location screensize = Nothing
        | otherwise = Just (bullet {location = (updateLocation location path), path = path} :: Bullet)   
instance Update Enemy where
    update  PlayingState{screensize = screensize, player = Player{location = playerLocation}} enemy@Enemy{location   = (x,y), path = StraightPath v, shotCooldownCounter = shotCooldownCounter, shotCooldown = shotCooldown, health = h}
        | outOfScreen (x,y) screensize = Nothing
        | h < 0 = Nothing
        | shotCooldownCounter < shotCooldown = Just enemy  {location = (x+v, y), shotCooldownCounter = shotCooldownCounter +1} :: Maybe Enemy
        | otherwise = Just enemy  {location = (x+v, y), shotCooldownCounter = 0} :: Maybe Enemy
instance Update Explosion where
    update  PlayingState{screensize = screensize, player = Player{location = playerLocation}} boom@Explosion{countdown = count, location = (x, y), velocity = (x2, y2)}
        | count > 0 = Just boom {countdown = count - 1, location = ((x + x2), (y + y2))}
        | otherwise = Nothing
instance Update Wave where
    update PlayingState{screensize = screensize, player = Player{location = playerLocation}} wave@Wave{stepCounter = stepCounter, interval = interval, totalEnemies = totalEnemies, enemyCounter = enemyCounter}
        | totalEnemies == enemyCounter = Nothing
        | interval == stepCounter = Just wave {stepCounter = 0, enemyCounter = enemyCounter +1}
        | otherwise = Just wave {stepCounter = stepCounter + 1}
instance Update PowerUp where
    update PlayingState{screensize = screensize, player = Player{location = playerLocation}} p@PowerUp{location = location, path = path} 
        | outOfScreen location screensize = Nothing
        | otherwise = Just (p {location = (updateLocation location path)} :: PowerUp)   

outOfScreen :: Point -> Point -> Bool
outOfScreen (x,y) (width, height) = or [(x >= (width/2)),(x <= -(width/2)), (y >= (height/2) ), (y <= -(height/2))]

        
-- | An object that can collide with another object
class Collideable a where
    loc             :: a -> Point
    hitboxSize      :: a -> Int

instance Collideable Bullet where
    loc Bullet{location = a} = a
    hitboxSize Bullet{image = i} = imgSize i
instance Collideable Enemy where
    loc Enemy{location = a} = a
    hitboxSize Enemy{image = i}  = imgSize i
instance Collideable Player where
    loc Player{location = a} = a
    hitboxSize Player{size = a} = a
instance Collideable PowerUp where
    loc PowerUp{location = a} = a
    hitboxSize PowerUp{size = a} = a

imgSize :: Picture -> Int
imgSize (Circle s) = round s
imgSize (Color _ (ThickCircle a b)) = (round (2*a)) + (round (2*b))

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

-- || Functions ######################################################################################################### | --
-- | Checks if two Collideables collide
-- | Seems to break for bigger sizes TODO
collides :: (Collideable a, Collideable b) => a -> b -> Bool
collides a b = collideHelper (loc a) (loc b) (fromIntegral((hitboxSize a) `div` 2)) (fromIntegral((hitboxSize b) `div` 2))
collideHelper :: Point -> Point -> Float -> Float -> Bool
collideHelper (x,y) (x2,y2) s1 s2 = x < (x2+s2) && (x+s1) > x2 && y < (y2+s2) && (y+s1) > y2

removeIfCollides :: (Collideable a, Collideable b) => a -> b -> Maybe b
removeIfCollides a b 
    | collides a b = Nothing
    | otherwise = Just b

-- putPowerUpsInBullets :: Bullet -> [PowerUp] -> Bullet
-- putPowerUpsInBullets bullet powerUps = foldr putPowerUpInBullet bullet powerUps



-- | Check if an enemy can shoot
enemyCanShoot :: Enemy -> Bool
enemyCanShoot Enemy{shotCooldown = shotCooldown, shotCooldownCounter = shotCooldownCounter} = shotCooldown == shotCooldownCounter
-- | Create the enemies bullet
enemyShoots :: Enemy -> Point -> Bullet
enemyShoots Enemy{location = location, bullet = bullet@Bullet{path = StraightPath v}} point = bullet {location = location}
enemyShoots Enemy{location = location, bullet = bullet@Bullet{path = AimedPath p v} } point = aimBulletAtPoint point (bullet {location = location})

-- | Aims a bullet at a point. I feel like the speed is still a bit buggy
aimBulletAtPoint :: Point -> Bullet -> Bullet
aimBulletAtPoint point b@Bullet{location = bulletPoint, path = path} = b {path = aimPathAtPoint point bulletPoint path }
    -- where
    --     dx   = x-xb
    --     dy   = y-yb
    --     total= (abs dx)+(abs dy)

-- | Aim to a point from the point of the bullet
aimPathAtPoint :: Point -> Point -> ObjectPath -> ObjectPath
aimPathAtPoint (x,y) (xb,yb) (AimedPath v _) = AimedPath v ((dx / total), (dy / total))
    where
        dx   = x-xb
        dy   = y-yb
        total= (abs dx)+(abs dy)

-- | Updates an object according to its location and path
updateLocation :: Point -> ObjectPath -> Point
updateLocation (x,y) (StraightPath f)           = (x-f,y)
updateLocation (x,y) (AimedPath v (fx,fy))      = (x+(fx * v), y+(fy*v))
updateLocation (x,y) (HomingPath v (fx,fy) _)   = (x+(fx * v), y+(fy*v))

-- | Turns a homingPath slightly
-- | xc is current x, xpl is player x, xpa is path x
updateHomingPath :: Point -> Point -> ObjectPath -> ObjectPath
updateHomingPath current@(xc,yc) player@(xpl,ypl) path = updatePathHelper newDirection path
    where
        newDirection = aimPathAtPoint player current path
    
updatePathHelper :: ObjectPath -> ObjectPath -> ObjectPath
updatePathHelper (AimedPath _ (ax, ay)) (HomingPath v (hx, hy) turningRate) = HomingPath v (xnew, ynew) turningRate
    where
        xnew = ((ax*turningRate) + hx) / (1+turningRate)
        ynew = ((ay*turningRate) + hy) / (1+turningRate)

-- || Game Objects  ##################################################################################################### | --

-- | This works because of the DuplicateRecordFields extension
data Bullet = Bullet {location :: Point, damagePoints :: Int, image :: Picture, path :: ObjectPath}
  
data Enemy  = Enemy {location :: Point, health :: Int, image :: Picture, path :: ObjectPath, bullet :: Bullet, shotCooldown :: Int, shotCooldownCounter :: Int, size :: Int, score :: Int} 
      
-- | The kinds of paths a bullet or enemy can follow
data ObjectPath = StraightPath Float      -- Float is x speed
    | AimedPath   Float Point             -- First Float is speed, point is x and y parts of speed
    | HomingPath  Float Point Float       -- First Float is x speed, second y speed, third has range 0..1 and is turning rate
    | SinoidPath  Float                   -- First Float is speed, second is magnitude (a in f(x) = a*sin(x))



data Player = Player { image :: Picture, location :: Point, bullet :: Bullet, shotCooldown :: Int, size :: Int, health :: Int, powerUps :: [PowerUp]}

-- || PowerUps ########################################################################################################### | --

data PowerUp = PowerUp {location :: Point, powerUpType :: PowerUpType, path :: ObjectPath, size :: Int, image :: Picture, pickedUp :: Bool}
data PowerUpType = BulletSize | BulletSpeed | BulletDamage

powerUp :: Player -> PowerUp -> Player
powerUp pl@Player{powerUps = pups, bullet = b} pup = pl {powerUps = pup : pups, bullet = (putPowerUpInBullet b pup)}

putPowerUpInBullet :: Bullet -> PowerUp -> Bullet
putPowerUpInBullet b@Bullet{path = p} PowerUp{powerUpType = BulletSpeed} = b{path = increasePathSpeed p}
putPowerUpInBullet b@Bullet{image = img} PowerUp{powerUpType = BulletSize} = b{image = increaseImageSize img}

increasePathSpeed :: ObjectPath -> ObjectPath
increasePathSpeed (StraightPath v) = StraightPath (v+5)

increaseImageSize :: Picture -> Picture 
increaseImageSize (Circle f) = Circle (f + 1.0)

data Explosion = Explosion {location :: Point, scale :: Float, countdown :: Int, velocity :: Point}



-- || Wave logic ######################################################################################################## | --

-- | stepCounter gets +1 every step, and once counter == interval, the next enemy is spawned. 
data Wave = Wave{pattern :: SpawnPattern, enemies :: [Enemy], interval :: Int, enemyCounter :: Int, stepCounter :: Int, totalEnemies :: Int, key :: SpecialKey}
   
-- | Should be in -1..1 range. Spawns n enemies on 1/points of the screen height at the right of the screen 
data SpawnPattern = SpawnPattern [Float]

-- | Evaluates a wave and returns the correct Enemy in the correct spot
nextEnemy :: Wave -> Enemy
nextEnemy Wave{enemyCounter = n, enemies = enemies, pattern = (SpawnPattern np)} = testEnemy{location = (799, (450*(np!!patternIndex)))}
    where
        enemyIndex   = (length enemies) `mod` n
        patternIndex = n `mod` (length np)
       
waveNeedsSpawn :: Wave -> Bool
waveNeedsSpawn Wave{interval = interval, stepCounter = stepCounter} = interval == stepCounter

-- || Menu data ######################################################################################################### | --
data Button = Button {location :: Point, size :: Point, text :: String, switchto :: GameState, key :: Key}

data OnScreenText = OnScreenText {location :: Point, scale :: Float, text :: String}

-- || Objects ########################################################################################################### | --
-- | These are actual objects with values filled in | --
spawnPattern1   = SpawnPattern [-0.5, 0.0, 0.5]
testEnemy       = Enemy {health = 1, image = color black (ThickCircle 5.0 5.0), path = StraightPath (-3.0), bullet = testBulletAimed, shotCooldown = 30, shotCooldownCounter = 0, score = 1}
testBullet      = Bullet {damagePoints = 1, image = Circle 2.0, path = StraightPath (-5.0)}
testBulletAimed = Bullet {damagePoints = 1, image = Circle 2.0, path = AimedPath 5.0 (10.0,1.0)}
testPlayer      = Player { health = 10, image = color black (ThickCircle 5.0 10.0),  location = (0.0, 0.0), bullet = testBullet, shotCooldown = 10, size = 10}
testWave        = Wave {pattern = spawnPattern1, enemies = [testEnemy], interval = 30, enemyCounter = 1, stepCounter = 0, totalEnemies = 5}
testExplosion   = Explosion { scale = 100.0, countdown = 300, velocity = (0.0,0.0)}
testPowerUp     = PowerUp {location = (799.0, 0), path = StraightPath (5.0), size = 30, powerUpType = BulletSize, image = square, pickedUp = False}
testButton      = Button {location = (-225, 0), size = (550, 100), text = "Press S to play", switchto = playingState, key = Char 's'}
menuText        = OnScreenText {location = (-600, 300), scale = 1.0, text = "Janky Haskell game"}
beginState      = MenuState {buttons = [testButton], pressedKeys = [], screensize = screenSize, text = [menuText]}
playingState    = PlayingState{player = testPlayer, pressedKeys = [], enemies = [], friendlyBullets = [], enemyBullets = [], waves = [], explosions = [], score = 0, powerUps = [], screensize = screenSize}


-- | Pictures!
squareSize = 5
square = Polygon [(-squareSize,-squareSize),(squareSize,-squareSize),(squareSize,squareSize),(-squareSize,squareSize),(-squareSize,-squareSize)]


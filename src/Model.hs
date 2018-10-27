

-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- | This object contains all gameObjects. Is changed every tick by Controller, and drawn every tick by View
data GameState = GameState {
                   player :: Player
                 , pressedKeys :: [Key]
                 , enemies :: [Enemy]
                 , friendlyBullets :: [Bullet]
                 , enemyBullets :: [Bullet]
                 }


-- | This works because of the DuplicateRecordFields extension
data Bullet = Bullet {location :: Point, damage :: Int, image :: Picture, speed :: Int}

testBullet = Bullet {damage = 1, image = Circle 1.0, speed = 5}
enemyBullet = Bullet {damage = 1, image = circle 1.0, speed = -3}

data Enemy  = Enemy  {location :: Point, health :: Int, image :: Picture, speed :: Int, bullet :: Bullet} 
testEnemy = Enemy {location = (600, 0), health = 5, image = Arc 130.0 230.0 20.0, speed = 0, bullet = enemyBullet}


-- class Draw a where
    -- Draw :: a ->
    
data Player = Player { image :: Picture, location :: Point}
testPlayer = Player { image = ThickCircle 1.0 20.0,  location = (-450.0, 0.0)}

beginState = GameState { player = testPlayer, pressedKeys = [], enemies = [testEnemy], friendlyBullets = [], enemyBullets = []}

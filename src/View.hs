

-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

--viewGameState :: GameState -> Picture



-- | Once we get more gameObjects, we use a Pictures object, like this:
-- | Pictures[picture1, picture2, picture3]
-- | This itself is a Picture, so we can return it from viewPure
viewPure :: GameState -> Picture
viewPure GameState {player =Player {image = img, location = (x,y)}, friendlyBullets = pBullets, enemyBullets = enemyBullets, enemies = enemies, explosions = explosions} 
    = Pictures ((translate x y img) : (drawBullets pBullets) ++ (drawBullets enemyBullets) ++ (drawExplosions explosions) ++ (drawList 0.0 explosions) ++ (drawEnemies enemies))
            -- Player picture         Bullets

-- | Draw length of a list with circles
drawList :: Float -> [a] -> [Picture]
drawList _ []     = []
-- drawlist n [x]    = (translate (-500.0) (n-500.0) (Circle 30.0)) : []
drawList n (x:xs) = (translate (-400.0) (400.0-n) (Circle 30.0)) : (drawList (n+30.0) xs)


            
-- | Puts all images from the bullets in a list of pictures
drawBullets :: [Bullet] -> [Picture]
drawBullets [] = []
drawBullets ((Bullet {image = img, location = (x,y)}):xs) =  (translate x y img) : (drawBullets xs)

-- | Puts all images from the enemies in a list of pictures
drawEnemies :: [Enemy] -> [Picture]
drawEnemies [] = []
drawEnemies ((Enemy{image = img, location = (x,y)}):xs) = (translate x y img) : (drawEnemies xs)

-- | EXPLOSION!!!!!
drawExplosions :: [Explosion] -> [Picture]
drawExplosions [] = []
drawExplosions (Explosion {countdown = timer, scale = s, location = (x, y)} : xs)
  | timer < 0   = drawExplosions xs
  | timer < 25  = translate x y (color orange (ThickCircle 1.0 5.0))  : drawExplosions xs
  | timer < 50  = translate x y (Color orange (ThickCircle 1.0 7.5))  : drawExplosions xs
  | timer < 75  = translate x y (color orange (ThickCircle 1.0 10.0)) : drawExplosions xs
  | timer < 100 = translate x y (Color orange (ThickCircle 1.0 12.5)) : drawExplosions xs
  | timer < 125 = translate x y (color orange (ThickCircle 1.0 15.0)) : translate x y (color yellow (ThickCircle 0.1 5.0))   : drawExplosions xs
  | timer < 150 = translate x y (Color orange (ThickCircle 1.0 20.0)) : translate x y (color yellow (ThickCircle 0.15 10.0)) : drawExplosions xs
  | timer < 175 = translate x y (color orange (ThickCircle 1.0 25.0)) : translate x y (color yellow (ThickCircle 0.25 15.0)) : drawExplosions xs
  | timer < 200 = translate x y (color orange (ThickCircle 1.0 30.0)) : translate x y (color yellow (ThickCircle 17.5 17.5)) : drawExplosions xs
  | timer < 225 = translate x y (color orange (ThickCircle 1.0 25.0)) : translate x y (color yellow (ThickCircle 0.5 15.0))  : drawExplosions xs
  | timer < 250 = translate x y (color orange (ThickCircle 1.0 20.0)) : translate x y (color yellow (ThickCircle 1.0 10.0))  : drawExplosions xs
  | timer < 275 = translate x y (color orange (ThickCircle 1.0 15.0)) : translate x y (color yellow (ThickCircle 1.0 5.0))   : drawExplosions xs
  | timer < 300 = translate x y (color orange (ThickCircle 1.0 10.0)) : drawExplosions xs
  | otherwise   = drawExplosions xs

drawThings :: (Draw a) => [a] -> [Picture]
drawThings [] = []
drawThings (thing:xs) = (draw thing) : (drawThings xs)

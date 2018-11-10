

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
viewPure MenuState {buttons = buttons, text = list}
    = Pictures ((drawThings buttons) ++ (drawThings list))
viewPure PlayingState {player =Player {image = img, location = (x,y) , health = health}, friendlyBullets = pBullets, enemyBullets = enemyBullets, enemies = enemies,powerUps = powerUps, explosions = explosions, score = score} 
    = Pictures ((translate x y img) : (drawScore score) : (drawThings pBullets) ++ (drawThings enemyBullets) ++ (drawThings explosions) ++ (drawThings enemies) ++ (drawThings powerUps))
viewPure PausedState {text = list}
    = Pictures (drawThings list)
            -- Player picture         Bullets
            -- : (drawLocation (x,y))

-- | Draw length of a list with circles
drawListLength :: [a] -> Picture
-- drawlist n [x]    = (translate (-500.0) (n-500.0) (Circle 30.0)) : []
drawListLength ilst = (translate (-600.0) (300.0) (Text ("Length: " ++ (show (length ilst)))))

drawLocation :: Point -> Picture
drawLocation (x,y) = (translate (-600.0) (300.0) (Text ((show x) ++ ","++ (show y))))

drawScore :: Int -> Picture
drawScore n = (translate (-600.0) (300.0) (Text (show n)))
            
-- | Turn a list of Drawable objects into a list of pictures
drawThings :: (Draw a) => [a] -> [Picture]
drawThings [] = []
drawThings (thing:xs) = (draw thing) : (drawThings xs)


-- || Draw Instances ##################################################################################################### | --
instance Draw OnScreenText where
    draw OnScreenText {location = (x, y), scale = scale, text = text} = translate x y (Scale scale scale (Text text))
instance Draw Button where 
    draw Button {location = (x, y), size = (width, height), text = text} = translate x y (Pictures (color blue (Polygon [(0, height), (width, height), (width, -height/2), (0, -height/2)]) : [Scale 0.5 0.5 (Text text)]))
instance Draw Bullet where
    draw Bullet {image = img, location = (x,y)} = translate x y img
instance Draw Enemy where
    draw Enemy {image = img, location = (x,y)}  = (translate x y img)
instance Draw PowerUp where
    draw PowerUp {image = img, location = (x,y)} = (translate x y img)
instance Draw Explosion where
    draw Explosion {countdown = timer, scale = s, location = (x, y)}
        | timer < 0   = Blank
        | timer < 25  = translate x y (color orange (ThickCircle 1.0 5.0)) 
        | timer < 50  = translate x y (Color orange (ThickCircle 1.0 7.5)) 
        | timer < 75  = translate x y (color orange (ThickCircle 1.0 10.0)) 
        | timer < 100 = translate x y (Color orange (ThickCircle 1.0 12.5))
        | timer < 125 = Pictures (translate x y (color orange (ThickCircle 1.0 15.0)) : translate x y (color yellow (ThickCircle 0.1 5.0)):[])
        | timer < 150 = Pictures (translate x y (Color orange (ThickCircle 1.0 20.0)) : translate x y (color yellow (ThickCircle 0.15 10.0)):[])
        | timer < 175 = Pictures (translate x y (color orange (ThickCircle 1.0 25.0)) : translate x y (color yellow (ThickCircle 0.25 15.0)):[])
        | timer < 200 = Pictures (translate x y (color orange (ThickCircle 1.0 30.0)) : translate x y (color yellow (ThickCircle 17.5 17.5)):[])
        | timer < 225 = Pictures (translate x y (color orange (ThickCircle 1.0 25.0)) : translate x y (color yellow (ThickCircle 0.5 15.0)):[])
        | timer < 250 = Pictures (translate x y (color orange (ThickCircle 1.0 20.0)) : translate x y (color yellow (ThickCircle 1.0 10.0)):[])
        | timer < 275 = Pictures (translate x y (color orange (ThickCircle 1.0 15.0)) : translate x y (color yellow (ThickCircle 1.0 5.0)):[]) 
        | timer < 300 = translate x y (color orange (ThickCircle 1.0 10.0))
        | otherwise   = Blank
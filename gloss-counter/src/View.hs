-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

drawMenu :: Picture
drawMenu = color white $ Pictures [translate (-100) 50  $ scale 0.3 0.3 $ text "ASTEROIDS", translate (-180) (-25)  $ scale 0.15 0.15 $ text "Press ENTER / SPACE to Start Playing"]

drawGame :: GameState -> Picture
drawGame gstate = color white $ Pictures $ [ drawShip (ship gstate) ] ++ map drawBullet (bullets gstate) ++ map drawAsteroid (asteroids gstate)

drawShip :: Ship -> Picture
drawShip s = 
  let (x, y) = shipPos s
      shipShape = lineLoop [ (-10, -10), (0, 15), (10, -10) ]
  in translate x y $ rotate (heading s) $ color red shipShape

drawBullet :: Bullet -> Picture
drawBullet b =
  let (x,y) = bulletPos b
  in translate x y $ color yellow $ circleSolid 2.5

drawAsteroid :: Asteroid -> Picture
drawAsteroid ast =
  let (x, y) = astPos ast
      radius = case astSize ast of
                 Large  -> 30.0
                 Medium -> 20.0
                 Small  -> 10.0
  in translate x y $ color white $ circle radius

drawPaused :: Picture
drawPaused = color white $ Pictures [translate (-100) 50  $ scale 0.3 0.3 $ text "PAUSED", translate (-180) (-25)  $ scale 0.15 0.15 $ text "Press `P` to Resume Playing", translate (-180) (-75)  $ scale 0.15 0.15 $ text "Press `B` to Go To Main Menu"]

drawGameOver :: Picture
drawGameOver = color red $ Pictures [translate (-100) 50  $ scale 0.3 0.3 $ text "GAME OVER"]

viewPure :: GameState -> Picture
viewPure gstate = case screen gstate of
  Menu     -> drawMenu
  Playing  -> drawGame gstate
  Paused   -> drawPaused
  GameOver -> drawGameOver
  _ -> blank
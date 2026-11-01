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
drawGame gstate = color white $ Pictures [drawShip (ship gstate)]

drawShip :: Ship -> Picture
drawShip s = 
  let (x, y) = shipPos s
      shipShape = lineLoop [ (-10, -10), (0, 15), (10, -10) ]
  in translate x y $ rotate (heading s) $ color white shipShape

drawPaused :: Picture
drawPaused = color white $ Pictures [translate (-100) 50  $ scale 0.3 0.3 $ text "PAUSED", translate (-180) (-25)  $ scale 0.15 0.15 $ text "Press `P` to Resume Playing", translate (-180) (-75)  $ scale 0.15 0.15 $ text "Press `B` to Go To Main Menu"]

viewPure :: GameState -> Picture
viewPure gstate = case screen gstate of
  Menu   -> drawMenu
  Playing -> drawGame gstate
  Paused  -> drawPaused
  _ -> blank
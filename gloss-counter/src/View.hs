-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

drawMenu :: Picture
drawMenu = color white $ Pictures [translate (-100) 50  $ scale 0.3 0.3 $ text "ASTEROIDS", translate (-180) (-25)  $ scale 0.15 0.15 $ text "Press ENTER / SPACE to Start Playing"]

drawGame :: Picture
drawGame = color white $ Pictures [translate (-100) 50  $ scale 0.3 0.3 $ text "GAMING"]

drawPaused :: Picture
drawPaused = color white $ Pictures [translate (-100) 50  $ scale 0.3 0.3 $ text "PAUSED", translate (-180) (-25)  $ scale 0.15 0.15 $ text "Press `P` to Resume Playing", translate (-180) (-75)  $ scale 0.15 0.15 $ text "Press `B` to Go To Main Menu"]

viewPure :: GameState -> Picture
viewPure gstate = case screen gstate of
  Menu   -> drawMenu
  Playing -> drawGame
  Paused  -> drawPaused
  _ -> blank
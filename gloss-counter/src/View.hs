-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

drawMenu :: Picture
drawMenu = color white $ Pictures [translate (-100) 50  $ scale 0.3 0.3 $ text "ASTEROIDS", translate (-180) (-25)  $ scale 0.15 0.15 $ text "Press ENTER / SPACE to Start Playing"]

viewPure :: GameState -> Picture
viewPure gstate = case screen gstate of
  Menu   -> drawMenu
  Game -> color green (text (show "1"))
  Paused  -> translate 100 50 (rotate 45 (scale 2 2 (color green (text (show "c")))))
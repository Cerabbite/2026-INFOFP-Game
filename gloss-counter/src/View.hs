module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = drawShip (ship gstate)

shipOutline :: [Point]
shipOutline = [(0, 10), (-10, -10), (10, -10)]

drawShip :: Ship -> Picture
drawShip s = translate x y . rotate (shipDir s) . color white $ lineLoop shipOutline
  where (x, y) = shipPos s
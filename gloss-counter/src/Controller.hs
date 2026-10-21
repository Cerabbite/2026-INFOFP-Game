module Controller where

import Model
import Player
import Graphics.Gloss.Interface.IO.Game

step :: Float -> GameState -> IO GameState
step dt = return . stepPure dt

stepPure :: Float -> GameState -> GameState
stepPure dt gstate = case screen gstate of
  Playing -> gstate { ship        = updateShip dt (ship gstate)
                    , elapsedTime = elapsedTime gstate + dt }
  _       -> gstate

input :: Event -> GameState -> IO GameState
input e = return . inputPure e

inputPure :: Event -> GameState -> GameState
inputPure (EventKey key keyState _ _) gstate =
  gstate { ship = handleKey key keyState (ship gstate) }
inputPure _ gstate = gstate

handleKey :: Key -> KeyState -> Ship -> Ship
handleKey (SpecialKey KeyUp)    Down s = s { isThrust = True }
handleKey (SpecialKey KeyUp)    Up   s = s { isThrust = False }
handleKey (SpecialKey KeyLeft)  Down s = s { turnDir = TurnLeft }
handleKey (SpecialKey KeyRight) Down s = s { turnDir = TurnRight }
-- only stop turning if the released key is the one we are turning with
handleKey (SpecialKey KeyLeft)  Up   s | turnDir s == TurnLeft  = s { turnDir = NoTurn }
handleKey (SpecialKey KeyRight) Up   s | turnDir s == TurnRight = s { turnDir = NoTurn }
handleKey _ _ s = s
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

isThrustKey :: Key -> Bool
isThrustKey (SpecialKey KeyUp) = True
isThrustKey (Char 'w') = True
isThrustKey _ = False

isLeftKey :: Key -> Bool
isLeftKey (SpecialKey KeyLeft) = True
isLeftKey (Char 'a') = True
isLeftKey _ = False

isRightKey :: Key -> Bool
isRightKey (SpecialKey KeyRight) = True
isRightKey (Char 'd') = True
isRightKey _ = False

inputPure :: Event -> GameState -> GameState

-- Manage Screens
inputPure (EventKey (SpecialKey KeyEnter) Down _ _) gstate = case screen gstate of
  Menu -> gstate { screen = Playing }
  _       -> gstate
inputPure (EventKey (SpecialKey KeySpace) Down _ _) gstate = case screen gstate of
  Menu -> gstate { screen = Playing }
  _       -> gstate
inputPure (EventKey (Char 'p') Down _ _) gstate = case screen gstate of
  Playing -> gstate { screen = Paused }
  Paused  -> gstate { screen = Playing }
  _       -> gstate
inputPure (EventKey (Char 'b') Down _ _) gstate = case screen gstate of
  Paused  -> gstate { screen = Menu }
  _       -> gstate

-- Manage Movement Input
inputPure (EventKey k Down _ _) gstate
  | isThrustKey k = let s = ship gstate in gstate { ship = s { isThrusting = True } }
  | isLeftKey k   = let s = ship gstate in gstate { ship = s { turnDir = TurnLeft } }
  | isRightKey k  = let s = ship gstate in gstate { ship = s { turnDir = TurnRight } }

inputPure (EventKey k Up _ _) gstate
  | isThrustKey k = let s = ship gstate in gstate { ship = s { isThrusting = False } }
  | isLeftKey k   = let s = ship gstate in gstate { ship = s { turnDir = NoTurn } }
  | isRightKey k  = let s = ship gstate in gstate { ship = s { turnDir = NoTurn } }


inputPure _ gstate = gstate

input :: Event -> GameState -> IO GameState
input e gstate = return (inputPure e gstate)

stepPure :: Float -> GameState -> GameState
stepPure dt gstate = case screen gstate of
  Playing ->
    let updatedShip = moveShip (applyDrag (applyThrust (applyTurn (ship gstate) dt) dt) dt) dt
    in gstate { elapsedTime = elapsedTime gstate + dt, ship = updatedShip }
  _ -> gstate

step :: Float -> GameState -> IO GameState
step dt gstate = return (stepPure dt gstate)
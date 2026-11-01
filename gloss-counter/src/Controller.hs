module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

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

-- Manage Movement Input Arrows
inputPure (EventKey (SpecialKey KeyUp) Down _ _) gstate =
  let s = ship gstate in gstate { ship = s { isThrusting = True } }
inputPure (EventKey (SpecialKey KeyLeft) Down _ _) gstate =
  let s = ship gstate in gstate { ship = s { turnDir = TurnLeft } }
inputPure (EventKey (SpecialKey KeyRight) Down _ _) gstate =
  let s = ship gstate in gstate { ship = s { turnDir = TurnRight } }

inputPure (EventKey (SpecialKey KeyUp) Up _ _) gstate =
  let s = ship gstate in gstate { ship = s { isThrusting = False } }
inputPure (EventKey (SpecialKey KeyLeft) Up _ _) gstate =
  let s = ship gstate in gstate { ship = s { turnDir = NoTurn } }
inputPure (EventKey (SpecialKey KeyRight) Up _ _) gstate =
  let s = ship gstate in gstate { ship = s { turnDir = NoTurn } }

-- Manage Movement Input WASD
inputPure (EventKey (Char 'w') Down _ _) gstate =
  let s = ship gstate in gstate { ship = s { isThrusting = True } }
inputPure (EventKey (Char 'a') Down _ _) gstate =
  let s = ship gstate in gstate { ship = s { turnDir = TurnLeft } }
inputPure (EventKey (Char 'd') Down _ _) gstate =
  let s = ship gstate in gstate { ship = s { turnDir = TurnRight } }

inputPure (EventKey (Char 'w') Up _ _) gstate =
  let s = ship gstate in gstate { ship = s { isThrusting = False } }
inputPure (EventKey (Char 'a') Up _ _) gstate =
  let s = ship gstate in gstate { ship = s { turnDir = NoTurn } }
inputPure (EventKey (Char 'd') Up _ _) gstate =
  let s = ship gstate in gstate { ship = s { turnDir = NoTurn } }

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
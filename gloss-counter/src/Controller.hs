module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

inputPure :: Event -> GameState -> GameState
inputPure (EventKey (SpecialKey KeyEnter) Down _ _) gstate = case screen gstate of
  Menu -> gstate { screen = Game }
  _       -> gstate
inputPure (EventKey (SpecialKey KeySpace) Down _ _) gstate = case screen gstate of
  Menu -> gstate { screen = Game }
  _       -> gstate
inputPure (EventKey (Char 'p') Down _ _) gstate = case screen gstate of
  Game -> gstate { screen = Paused }
  Paused  -> gstate { screen = Game }
  _       -> gstate
inputPure (EventKey (Char 'b') Down _ _) gstate = case screen gstate of
  Paused  -> gstate { screen = Menu }
  _       -> gstate

inputPure _ gstate = gstate

input :: Event -> GameState -> IO GameState
input e gstate = return (inputPure e gstate)

step :: Float -> GameState -> IO GameState
step _ gstate = return gstate
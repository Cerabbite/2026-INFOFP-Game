-- | This module contains the data types
--   which represent the state of the game
module Model where

data Screen = Menu | Playing | Paused | GameOver | HighScores
  deriving(Eq, Show)

data GameState = GameState {
                  screen  :: Screen
                 , elapsedTime :: Float
                 } deriving(Show)

initialState :: GameState
initialState = GameState Menu 0
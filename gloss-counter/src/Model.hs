-- | This module contains the data types
--   which represent the state of the game
module Model where

import System.Random (StdGen)

type Vector = (Float, Float)

data Screen = Menu | Playing | Paused | GameOver | HighScores
  deriving (Eq, Show)

data TurnDir = TurnLeft | TurnRight | NoTurn
  deriving (Eq, Show)

data Ship = Ship 
  { shipPos      :: Vector
  ,  shipVel      :: Vector
  ,  shipDir      :: Float
  ,  shotCooldown :: Float
  ,  lives        :: Int
  ,  invulnerable :: Float
  ,  isThrust     :: Bool
  ,  turnDir      :: TurnDir
  }

data GameState = GameState
  { screen      :: Screen
  , ship        :: Ship
  , elapsedTime :: Float
  , rng         :: StdGen
  }

initialShip :: Ship
initialShip = Ship
  { shipPos       = (0, 0)
  , shipVel       = (0, 0)
  , shipDir       = 0
  , shotCooldown  = 0
  , lives         = 3
  , invulnerable  = 0
  , isThrust      = False
  , turnDir       = NoTurn

  }

initial :: StdGen -> GameState
initial gen = GameState
  { screen      = Playing   -- later: Menu
  , ship        = initialShip
  , elapsedTime = 0
  , rng         = gen
  }
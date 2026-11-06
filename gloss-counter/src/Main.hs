module Main where

import System.Random (getStdGen)

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do 
    gen <- getStdGen
    scores <- loadHighScores
    playIO (InWindow "Astroids" (600, 600) (200, 200)) -- Or FullScreen
              black            -- Background color
              60               -- Frames per second
              (initialState gen scores)     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
module Main where

import Controller
import Model
import View
import Player
import Asteroids
import Classes 
import Constants
import UFO
import Highscore

import Graphics.Gloss.Interface.IO.Game
import System.Random (newStdGen)   -- swap for mkStdGen 42 while debugging

main :: IO ()
main = do
  gen <- newStdGen
  playIO (InWindow "Asteroids" (windowWidth, windowHeight) (0, 0))
         black
         fps
         (initial gen)
         view
         input
         step
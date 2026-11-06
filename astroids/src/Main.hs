module Main where

import System.Random (getStdGen)

import Controller
import Model
import View
import Constants
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)

main :: IO ()
main = do
    gen <- getStdGen
    scores <- loadHighScores
    (w, h) <- getScreenSize
    let k = fromIntegral (min w h) / 600     -- scale 600x600 play area
    playIO FullScreen
           black
           60
           (initialState gen scores)
           (fmap (scale k k) . view)
           input
           step
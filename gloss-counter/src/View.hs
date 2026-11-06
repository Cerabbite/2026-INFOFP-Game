-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Constants
import Data.List (sortOn)
import Data.Ord (Down(..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- | Top level view function converting GameState to IO Picture for Gloss
view :: GameState -> IO Picture
view = return . viewPure

-- | Renders the initial main menu screen with title and navigation controls
drawMenu :: Picture
drawMenu = color white $ Pictures [translate (-100) 50  $ scale 0.3 0.3 $ text "ASTEROIDS", translate (-180) (-25)  $ scale 0.15 0.15 $ text "Press ENTER / SPACE to start playing",translate (-160) (-50)  $ scale 0.15 0.15 $ text "Press `H` to see your HighScores" ]

-- | Renders the active gameplay screen and everything that must be drawn for the game including ships, projectiles, asteroids, explosions, lives, and score
drawGame :: GameState -> Picture
drawGame gstate = color white $ Pictures $
     [ drawShip red (ship gstate) ]
  ++ map render (bullets gstate)
  ++ map render (asteroids gstate)
  ++ map render (explosions gstate)
  ++ map (drawShip green) (ufos gstate)
  ++ [ drawLives (lives gstate), drawScore (score gstate) ]

-- | Standard triangular polygon shape representing the player's ship
shipShape :: Picture
shipShape = lineLoop [ (-10, -10), (0, 15), (10, -10) ]

-- | Renders the remaining lives as small red ship icons aligned at the top-left of the screen
drawLives :: Int -> Picture
drawLives n = Pictures
  [ translate (-270 + fromIntegral i * 25) 270 $ scale 0.8 0.8 $ color red shipShape
  | i <- [0 .. n - 1] ]

-- | Renders the player's current score centered at the top of the screen
drawScore :: Int -> Picture
drawScore n =
  let toStringVar = show n
      size        = 0.2
      wdth        = fromIntegral (length toStringVar) * 50 * size
      letterwdth  = 5 * 50 * 0.1
  in Pictures
       [ translate (-letterwdth / 2) 275 $ scale 0.1 0.1 $ color white $ text "SCORE"
       , translate (-wdth / 2)  240 $ scale size size $ color white $ text toStringVar ]

-- | Renders a ship (player or UFO) given a color, applying position, rotation, and a blinking visual effects when invulnerable
drawShip :: Color -> Ship -> Picture
drawShip c s
  | invulnTime s > 0 && not visible = blank
  | otherwise = translate x y $ rotate (heading s) $ color c shipShape
  where
    (x, y)  = shipPos s
    visible = even (floor (invulnTime s * 10) :: Int)

-- | Type class for game entities that can be converted directly into a Gloss Picture
class Renderable a where
  render :: a -> Picture

-- | Renders bullets with colours differentiated by ownership (player vs UFO)
instance Renderable Bullet where
  render b =
    let (x, y) = getPos b
        c = case bulletOwner b of
              UFOBullet    -> blue
              PlayerBullet -> yellow
    in translate x y $ color c $ circleSolid (getRadius b)

-- | Renders asteroids as white outlined circles according to their collision radius
instance Renderable Asteroid where
  render ast =
    let (x, y) = getPos ast
    in translate x y $ color white $ circle (getRadius ast)

-- | Renders explosion animations as expanding orange circles
instance Renderable Explosion where
  render explosion =
    let (x, y) = expPos explosion
        -- Expand the circle as expTimeLeft counts down to 0
        r      = (explosionTimer - expTimeLeft explosion) * 40 * expScale explosion
    in translate x y $ color orange $ circle r

-- | Renders the pause menu overlay with option instructions.
drawPaused :: Picture
drawPaused = color white $ Pictures [
  translate (-100) 50  $ scale 0.3 0.3 $ text "PAUSED",
  translate (-180) (-25)  $ scale 0.15 0.15 $ text "Press `P` to resume playing",
  translate (-180) (-75)  $ scale 0.15 0.15 $ text "Press `B` to Go To Main Menu", 
  translate (-180) (-100) $ scale 0.15 0.15 $ text "Press `S` to save game", 
  translate (-180) (-125) $ scale 0.15 0.15 $ text "Press `L` to load saved game"]

-- | Renders the Game Over screen highlighting final score and the players highscore
drawGameOver :: GameState -> Picture
drawGameOver gstate = 
  let currentScore = score gstate
      hs = if null (highScores gstate) 
             then currentScore 
             else max currentScore (maximum (map hsScore (highScores gstate)))
  in color red $ Pictures 
       [ translate (-125) 50   $ scale 0.3 0.3   $ text "GAME OVER"
       , translate (-80)  0    $ scale 0.15 0.15 $ text ("SCORE: " ++ show currentScore)
       , translate (-100) (-30) $ scale 0.15 0.15 $ text ("HIGH SCORE: " ++ show hs)
       , translate (-180) (-80) $ scale 0.15 0.15 $ text "Press ENTER / SPACE to restart"
       , translate (-210) (-100) $ scale 0.15 0.15 $ text "Press ESCAPE / BACKSPACE to return home"
       ]

-- | Renders the High Scores leaderboard screen with formatted timestamps sorted from highest to lowest scores in the form
-- | 1. SCORE TIME
drawHighScores :: GameState -> Picture
drawHighScores gstate = 
  let sortedScores = sortOn (Down . hsScore) (highScores gstate)
      formatTimestamp ts = formatTime defaultTimeLocale "%H:%M %d/%m/%Y" (posixSecondsToUTCTime (fromIntegral ts))
      formatEntry idx hs = show idx ++ ". " ++ show (hsScore hs) ++ " - " ++ formatTimestamp (hsTimestamp hs)
      renderedScores = zipWith 
        (\i hs -> translate (-250) (200 - fromIntegral i * 30) $ scale 0.15 0.15 $ text (formatEntry i hs)) 
        [1..] 
        sortedScores
      title = translate (-270) 250 $ scale 0.3 0.3 $ text "HIGHSCORES"
      ret = translate (140) 280 $ scale 0.10 0.10 $ text "ESCAPE to return home"
      
  in color white $ Pictures (title : ret : renderedScores)

-- | Dlegates rendering to the apropriate screen drawer based on current screen mode
viewPure :: GameState -> Picture
viewPure gstate = case screen gstate of
  Menu     -> drawMenu
  Playing  -> drawGame gstate
  Paused   -> drawPaused
  GameOver -> drawGameOver gstate
  HighScores -> drawHighScores gstate
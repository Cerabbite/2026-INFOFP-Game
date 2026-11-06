-- | This module handles user inputs, game loop updates, collision detection, spawning logic, and persistent saving/loading of game states and high scores.
module Controller where

import Model
import Constants
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.List (partition)
import Data.Aeson (encode, decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Exception (catch, IOException)

-- | File name/path for storing JSON high score records
highScoreFile :: FilePath
highScoreFile = "highscores.json"

-- | Error handler for file IO operations. Returns an empty ByteString on failure
handleFileError :: IOException -> IO BS.ByteString
handleFileError _ = return BS.empty

-- | Records a new (high) score with the current timestamp, saves it to disk, and returns the updated highscores list
saveHighScore :: Int -> IO [HighScore]
saveHighScore currentScore = do
  posixTime <- getPOSIXTime
  let newEntry = HighScore { hsTimestamp = round posixTime, hsScore = currentScore }
  existingScores <- loadHighScores
  let updatedScores = newEntry : existingScores
  BSL.writeFile highScoreFile (encode updatedScores)
  return updatedScores

-- | Reads highscores from file and decodes the JSON payload. Returns an empty list on failure
loadHighScores :: IO [HighScore]
loadHighScores = do
  strictBytes <- catch (BS.readFile highScoreFile) handleFileError
  let existingBytes = BSL.fromStrict strictBytes
  return $ case decode existingBytes of
             Just xs -> xs
             Nothing -> []

-- | Checks if the given key corresponds to "Return to Main Menu / Back" action
isReturnHomeKey :: Key -> Bool
isReturnHomeKey (Char '\b')        = True
isReturnHomeKey (Char '\DEL')      = True
isReturnHomeKey (SpecialKey KeyEsc) = True
isReturnHomeKey _ = False

-- | Checks if the given key corresponds to forward thrusting
isThrustKey :: Key -> Bool
isThrustKey (SpecialKey KeyUp) = True
isThrustKey (Char 'w') = True
isThrustKey _ = False

-- | Checks if the given key corresponds to turning left
isLeftKey :: Key -> Bool
isLeftKey (SpecialKey KeyLeft) = True
isLeftKey (Char 'a') = True
isLeftKey _ = False

-- | Checks if the given key corresponds to turning right
isRightKey :: Key -> Bool
isRightKey (SpecialKey KeyRight) = True
isRightKey (Char 'd') = True
isRightKey _ = False

-- | Handling keyboard event updates to the game state based on the active screen and key presses
inputPure :: Event -> GameState -> GameState
inputPure (EventKey (Char 'h') Down _ _) gstate = case screen gstate of
  Menu -> gstate { screen = HighScores }
  _       -> gstate
inputPure (EventKey (SpecialKey KeyEnter) Down _ _) gstate = case screen gstate of
  Menu -> gstate { screen = Playing }
  GameOver -> resetGame gstate Playing
  _       -> gstate
inputPure (EventKey (SpecialKey KeySpace) Down _ _) gstate = case screen gstate of
  Menu     -> gstate { screen = Playing }
  Playing  -> gstate { bullets = fireBullet PlayerBullet (ship gstate) : bullets gstate }
  GameOver -> resetGame gstate Playing
  _       -> gstate
inputPure (EventKey (Char 'p') Down _ _) gstate = case screen gstate of
  Playing -> gstate { screen = Paused }
  Paused  -> gstate { screen = Playing }
  _       -> gstate
inputPure (EventKey (Char 'b') Down _ _) gstate = case screen gstate of
  Paused  -> gstate { screen = Menu }
  _       -> gstate

inputPure (EventKey k Down _ _) gstate
  | isThrustKey k = let s = ship gstate in gstate { ship = s { isThrusting = True } }
  | isLeftKey k   = let s = ship gstate in gstate { ship = s { turnDir = TurnRight } }
  | isRightKey k  = let s = ship gstate in gstate { ship = s { turnDir = TurnLeft } }
  | isReturnHomeKey k = resetGame gstate Menu

inputPure (EventKey k Up _ _) gstate
  | isThrustKey k = let s = ship gstate in gstate { ship = s { isThrusting = False } }
  | isLeftKey k   = let s = ship gstate in gstate { ship = s { turnDir = NoTurn } }
  | isRightKey k  = let s = ship gstate in gstate { ship = s { turnDir = NoTurn } }


inputPure _ gstate = gstate

-- | Main IO input handler processing save/load triggers or delegating to pure input handling
input :: Event -> GameState -> IO GameState
input (EventKey (Char 's') Down _ _) gs = saveGame gs >> return gs
input (EventKey (Char 'l') Down _ _) gs = loadGame gs
input e gs = return (inputPure e gs)

-- | Pure time step update function progressing physics, movement, and collision resolutions
stepPure :: Float -> GameState -> GameState
stepPure dt gstate = case screen gstate of
  Playing ->
    let s0 = updateShip dt (ship gstate)
        updatedShip    = s0 { invulnTime = max 0 (invulnTime s0 - dt) }
        updatedBullets = updateBullets (bullets gstate) dt
        (movedUFOs, ufoShots) = unzip (map (stepUFO dt updatedShip) (ufos gstate))
        moved = gstate
          { elapsedTime = elapsedTime gstate + dt
          , ship        = updatedShip
          , bullets     = concat ufoShots ++ updatedBullets
          , ufos        = movedUFOs
          , asteroids   = map (move dt) (asteroids gstate)
          , explosions  = updateExplosions (explosions gstate) dt
          }
    in ufoSpawnStep dt . spawnStep dt . resolveUFOs . resolveCollisions $ moved
  _ -> gstate

-- | Main IO time step function executing pure updates and persisting high scores upon game over
step :: Float -> GameState -> IO GameState
step dt gstate = do
  let nextState = stepPure dt gstate
  if screen gstate == Playing && screen nextState == GameOver
    then do
      newScores <- saveHighScore (score nextState)
      return nextState { highScores = newScores }
    else return nextState

-- | Detects and resolves collisions between bullets, asteroids and the players ship
resolveCollisions :: GameState -> GameState
resolveCollisions gs =
  let playerBullets = [ b | b <- bullets gs, bulletOwner b == PlayerBullet ]
      otherBullets  = [ b | b <- bullets gs, bulletOwner b /= PlayerBullet ]

      -- which asteroids were shot and which bullets hit something
      (shot, intact) = partition (\a -> any (`collidesWith` a) playerBullets) (asteroids gs)
      spentBullets   = [ b | b <- playerBullets, not (any (collidesWith b) (asteroids gs)) ]

      -- split shot asteroids, threading random gen
      (fragments, rng') = foldr
        (\a (acc, g) -> let (new, g') = splitAsteroid g a in (new ++ acc, g'))
        ([], rng gs) shot

      newExplosions = [ Explosion (astPos a) 1 0.5 | a <- shot ]

      earnedScore = sum [ asteroidPoints (astSize a) | a <- shot ]

      s = ship gs
      shipDies = invulnTime s <= 0 && any (collidesWith s) intact
      livesLeft 
        |shipDies  = lives gs - 1
        |otherwise = lives gs

      newShip
        |shipDies  = s { invulnTime = respawnInvuln }
        |otherwise = s
      deathExplosion = [ Explosion (shipPos s) 1 0.5 | shipDies ]
      
      newScreen
        | shipDies && livesLeft <= 0 = GameOver 
        | otherwise                  = screen gs

  in gs { bullets    = spentBullets ++ otherBullets
        , asteroids  = intact ++ fragments
        , explosions = deathExplosion ++ newExplosions ++ explosions gs
        , ship       = newShip
        , lives      = livesLeft
        , rng        = rng'
        , score      = score gs + earnedScore
        , screen     = newScreen}

-- | Reduces player lives and updates game state when the players ship is destroyed
killShip :: Bool -> GameState -> GameState
killShip False gs = gs
killShip True  gs
  | livesLeft <= 0 = hit { screen = GameOver }
  | otherwise      = hit
  where
    livesLeft = lives gs - 1
    s         = ship gs
    hit = gs { lives      = livesLeft
             , ship       = s { invulnTime = respawnInvuln }
             , explosions = Explosion (shipPos s) 1 0.5 : explosions gs }

-- | Periodically spawns new asteroids along the screen edges up to maximum allowed capacity
spawnStep :: Float -> GameState -> GameState
spawnStep dt gs
  | t > 0                                 = gs { spawnTimer = t }
  | length (asteroids gs) >= maxAsteroids = gs { spawnTimer = spawnInterval }
  | otherwise =
      let (a, g') = randomEdgeAsteroid (rng gs)
      in gs { asteroids = a : asteroids gs
            , rng        = g'
            , spawnTimer = spawnInterval }
  where t = spawnTimer gs - dt

-- | Resolves interactions between UFOs, player bullets, enemy bullets and the players ship
resolveUFOs :: GameState -> GameState
resolveUFOs gs =
  killShip hurt gs
       { ufos       = alive
       , bullets    = [ b | b <- mine,   not (any (collidesWith b) (ufos gs)) ]
                   ++ [ b | b <- theirs, not (shotAtMe b) ]
       , score      = score gs + ufoPoints * length (filter shotDown dead)
       , explosions = [ Explosion (shipPos u) 1 0.5 | u <- dead ] ++ explosions gs
       }
  where
      s              = ship gs
      vulnerable     = invulnTime s <= 0
      (mine, theirs) = partition ((== PlayerBullet) . bulletOwner) (bullets gs)

      shotDown u = any (`collidesWith` u) mine
      shotAtMe b = vulnerable && collidesWith b s

      (dead, alive) = partition shotDown (ufos gs)
      hurt          = any shotAtMe theirs

-- | Periodically spawns UFO enemies based on timer intervals and max limits
ufoSpawnStep :: Float -> GameState -> GameState
ufoSpawnStep dt gs
  | t > 0                       = gs { ufoTimer = t }
  | length (ufos gs) >= maxUFOs = gs { ufoTimer = ufoSpawnInterval }
  | otherwise =
      let (u, g') = spawnUFO (rng gs)
      in gs { ufos = u : ufos gs, rng = g', ufoTimer = ufoSpawnInterval }
  where t = ufoTimer gs - dt

-- | Serializes current GameState to json
saveGame :: GameState -> IO ()
saveGame gs = BSL.writeFile "savegame.json" (encode gs)

-- | Deserializes GameState from json or returns unchanged state on error
loadGame :: GameState -> IO GameState
loadGame gs = do
  bytes <- catch (BS.readFile "savegame.json") handleFileError
  case decode (BSL.fromStrict bytes) of
    Just saved -> return saved
    Nothing    -> return gs
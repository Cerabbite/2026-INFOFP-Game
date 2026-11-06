module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.List (partition)
import Data.Aeson (encode, decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Exception (catch, IOException)

highScoreFile :: FilePath
highScoreFile = "highscores.json"

handleFileError :: IOException -> IO BS.ByteString
handleFileError _ = return BS.empty

saveHighScore :: Int -> IO [HighScore]
saveHighScore currentScore = do
  posixTime <- getPOSIXTime
  let newEntry = HighScore { hsTimestamp = round posixTime, hsScore = currentScore }
  existingScores <- loadHighScores
  let updatedScores = newEntry : existingScores
  BSL.writeFile highScoreFile (encode updatedScores)
  return updatedScores

loadHighScores :: IO [HighScore]
loadHighScores = do
  strictBytes <- catch (BS.readFile highScoreFile) handleFileError
  let existingBytes = BSL.fromStrict strictBytes
  return $ case decode existingBytes of
             Just xs -> xs
             Nothing -> []

isReturnHomeKey :: Key -> Bool
isReturnHomeKey (Char '\b')        = True
isReturnHomeKey (Char '\DEL')      = True
isReturnHomeKey (SpecialKey KeyEsc) = True
isReturnHomeKey _ = False

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

input :: Event -> GameState -> IO GameState
input e gstate = return (inputPure e gstate)

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
          , asteroids   = map (moveAsteroid dt) (asteroids gstate)
          , explosions  = updateExplosions (explosions gstate) dt
          }
    in ufoSpawnStep dt . spawnStep dt . resolveUFOs . resolveCollisions $ moved
  _ -> gstate

step :: Float -> GameState -> IO GameState
step dt gstate = do
  let nextState = stepPure dt gstate
  if screen gstate == Playing && screen nextState == GameOver
    then do
      saveHighScore (score nextState)
      return nextState
    else return nextState

resolveCollisions :: GameState -> GameState
resolveCollisions gs =
  let playerBullets = [ b | b <- bullets gs, bulletOwner b == PlayerBullet ]
      otherBullets  = [ b | b <- bullets gs, bulletOwner b /= PlayerBullet ]

      -- which asteroids were shot and which bullets hit something
      (shot, intact) = partition (\a -> any (`bulletHitsAsteroid` a) playerBullets) (asteroids gs)
      spentBullets   = [ b | b <- playerBullets, not (any (bulletHitsAsteroid b) (asteroids gs)) ]

      -- split shot asteroids, threading random gen
      (fragments, rng') = foldr
        (\a (acc, g) -> let (new, g') = splitAsteroid g a in (new ++ acc, g'))
        ([], rng gs) shot

      newExplosions = [ Explosion (astPos a) 1 0.5 | a <- shot ]

      earnedScore = sum [ asteroidPoints (astSize a) | a <- shot ]

      s = ship gs
      shipDies = invulnTime s <= 0 && any (shipHitsAsteroid s) intact
      livesLeft 
        |shipDies  = lives gs - 1
        |otherwise = lives gs

      newShip
        |shipDies  = s { invulnTime = respawnInvuln }
        |otherwise = s
      deathExplosion = [ Explosion (shipPos s) 1 0.5 | shipDies ]

  in gs { bullets    = spentBullets ++ otherBullets
        , asteroids  = intact ++ fragments
        , explosions = deathExplosion ++ newExplosions ++ explosions gs
        , ship       = newShip
        , lives      = livesLeft
        , rng        = rng'
        , score      = score gs + earnedScore
        , screen     = if shipDies && livesLeft <= 0 then GameOver else screen gs }
        
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

--Asteroid spawner controller
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

resolveUFOs :: GameState -> GameState
resolveUFOs gs =
  killShip hurt gs
       { ufos       = alive
       , bullets    = [ b | b <- mine,   not (any (bulletHitsShip b) (ufos gs)) ]
                   ++ [ b | b <- theirs, not (shotAtMe b) ]
       , score      = score gs + ufoPoints * length (filter shotDown dead)
       , explosions = [ Explosion (shipPos u) 1 0.5 | u <- dead ] ++ explosions gs
       }
  where
      s            = ship gs
      vulnerable     = invulnTime s <= 0
      (mine, theirs) = partition ((== PlayerBullet) . bulletOwner) (bullets gs)

      shotDown u = any (`bulletHitsShip` u) mine
      shotAtMe b = vulnerable && bulletHitsShip b s

      (dead, alive) = partition shotDown (ufos gs)
      hurt          = any shotAtMe theirs

ufoSpawnStep :: Float -> GameState -> GameState
ufoSpawnStep dt gs
  | t > 0                       = gs { ufoTimer = t }
  | length (ufos gs) >= maxUFOs = gs { ufoTimer = ufoSpawnInterval }
  | otherwise =
      let (u, g') = spawnUFO (rng gs)
      in gs { ufos = u : ufos gs, rng = g', ufoTimer = ufoSpawnInterval }
  where t = ufoTimer gs - dt
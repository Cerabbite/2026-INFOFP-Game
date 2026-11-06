{-# LANGUAGE DeriveGeneric #-}

-- | This module defimes the data models, state definitions, physics updates, spawning algorithm, AI behavior, and typeclasses for collision and movement
module Model where

import System.Random (StdGen, randomR, random, mkStdGen)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Constants

-- | JSON serialization instances for the standard random generator
instance ToJSON StdGen where
  toJSON g = toJSON (fst (random g :: (Int, StdGen)))

instance FromJSON StdGen where
  parseJSON v = fmap mkStdGen (parseJSON v)

-- | 2D vector (x, y) coordinates
type Vector = (Float , Float)

-- | Represents all the possible active screens of the game
data Screen = Menu | Playing | Paused | GameOver | HighScores
  deriving (Eq, Show, Generic)
instance ToJSON Screen
instance FromJSON Screen

-- | Directional turning intent for the ships
data TurnDir = TurnLeft | TurnRight | NoTurn
  deriving (Eq, Show, Generic)
instance ToJSON TurnDir
instance FromJSON TurnDir

-- | Saved highscore record containing an POSIX timestamp and numerical score
data HighScore = HighScore
  { hsTimestamp :: Integer
  , hsScore     :: Int
  } deriving (Show, Generic)
instance ToJSON HighScore
instance FromJSON HighScore

-- | Entity representing a space ship (Player or UFO)
data Ship = Ship
  { shipPos :: Vector
  , shipVel :: Vector
  , heading :: Float
  , isThrusting :: Bool
  , turnDir :: TurnDir
  , shootCooldown :: Float
  , invulnTime :: Float
  } deriving (Show, Generic)
instance ToJSON Ship
instance FromJSON Ship

-- | Identifies if a bullet was fired by the player or an enemy UFO
data BulletOwner = PlayerBullet | UFOBullet 
  deriving (Eq, Show, Generic)
instance ToJSON BulletOwner
instance FromJSON BulletOwner

-- | Projectile record tracking position, velocity, remaining lifespan, and ownership
data Bullet = Bullet
  { bulletPos :: Vector
  , bulletVel :: Vector
  , bulletLifeTime :: Float
  , bulletOwner :: BulletOwner
  } deriving (Show, Generic)
instance ToJSON Bullet
instance FromJSON Bullet

-- | Asteroid size possibilities
data AsteroidSize = Large | Medium | Small 
  deriving (Eq, Show, Generic)
instance ToJSON AsteroidSize
instance FromJSON AsteroidSize

-- | Asteroid data entity tracking the position, velocity and size class defined aboce
data Asteroid = Asteroid
  { astPos :: Vector
  , astVel :: Vector
  , astSize :: AsteroidSize
  } deriving (Show, Generic)
instance ToJSON Asteroid
instance FromJSON Asteroid

-- | Visual explosion effect tracking position, scaling factor, and duration
data Explosion = Explosion
  { expPos :: Vector
  , expScale :: Float
  , expTimeLeft :: Float
  } deriving (Show, Generic)
instance ToJSON Explosion
instance FromJSON Explosion

-- | Default starting configuration for the player ship
initialShip :: Ship
initialShip = Ship
  { shipPos = (0, 0)
  , shipVel = (0, 0)
  , heading = 0
  , isThrusting = False
  , turnDir = NoTurn
  , shootCooldown = 0
  , invulnTime = 3
  }

-- | Top level state record encapsulating everything of the game like entities, metrics, timers and random generator
data GameState = GameState {
                  screen       :: Screen
                 , elapsedTime :: Float
                 , ship        :: Ship
                 , lives       :: Int
                 , bullets     :: [Bullet]
                 , asteroids   :: [Asteroid]
                 , explosions  :: [Explosion]
                 , rng         :: StdGen
                 , score       :: Int
                 , spawnTimer :: Float
                 , highScores :: [HighScore]
                 , ufos :: [Ship]
                 , ufoTimer :: Float
                 } deriving(Show, Generic)
instance ToJSON GameState
instance FromJSON GameState

-- | Constructs the initial game state with fresh entities, timer defaults and high score records
initialState :: StdGen -> [HighScore] -> GameState
initialState initialRng loadedScores = 
  let (asts, newRng) = generateAsteroids 5 initialRng
  in GameState 
       { screen      = Menu 
       , lives       = startingLives
       , elapsedTime = 0 
       , ship        = initialShip 
       , bullets     = []
       , asteroids   = asts
       , explosions  = []
       , rng         = newRng
       , score       = initialScore
       , spawnTimer  = spawnInterval
       , highScores  = loadedScores
       , ufos        = []
       , ufoTimer    = ufoSpawnInterval
       }

-- | Resets gameplay entities, timers and score while retaining the highscores history and transitions screen mode to a screen that is supplied to the function
resetGame :: GameState -> Screen -> GameState
resetGame gs screen = 
  let (asts, newRng) = generateAsteroids 5 (rng gs)
  in gs 
       { screen      = screen
       , lives       = startingLives
       , elapsedTime = 0 
       , ship        = initialShip 
       , bullets     = []
       , asteroids   = asts
       , explosions  = []
       , rng         = newRng
       , score       = initialScore
       , spawnTimer  = spawnInterval
       , ufos        = []
       ,ufoTimer     = ufoSpawnInterval
       }

-- | Returns points awarded for destroying an asteroid based on its size
asteroidPoints :: AsteroidSize -> Int
asteroidPoints Large  = 25
asteroidPoints Medium = 50
asteroidPoints Small  = 100

-- | Updates a ships heading based on active turn input and delta time
applyTurn :: Ship -> Float -> Ship
applyTurn s dt = case turnDir s of
  TurnLeft  -> s { heading = heading s + turnSpeed * dt }
  TurnRight -> s { heading = heading s - turnSpeed * dt }
  NoTurn    -> s

-- | Calculates thrust acceleration vectors along the ships heading and applies them to the ship velocity
applyThrust :: Ship -> Float -> Ship
applyThrust s dt
  | isThrusting s =
      let rad = heading s * (pi / 180.0)
          ax  = sin rad * thrustAccel * dt
          ay  = cos rad * thrustAccel * dt
          (vx, vy) = shipVel s
      in s { shipVel = (vx + ax, vy + ay) }
  | otherwise = s

-- | Applies velocity drag decay over time to simulate ambient friction
applyDrag :: Ship -> Float -> Ship
applyDrag s dt =
  let (vx, vy) = shipVel s
      decay    = dragCoeff ** (dt * 60.0)
  in s { shipVel = (vx * decay, vy * decay) }

-- | Wraps continuous coordinates around screen boundary limits
wrap :: Vector -> Vector
wrap (x, y) = (wrapCoord x, wrapCoord y)
  where
    wrapCoord c
      | c > screenRadius  = c - (2 * screenRadius)
      | c < -screenRadius = c + (2 * screenRadius)
      | otherwise         = c

-- | Constructs and launches a bullet from a ship's position along its heading direction
fireBullet :: BulletOwner -> Ship -> Bullet
fireBullet owner s =
  let (x, y) = shipPos s
      rad    = heading s * (pi / 180.0)
      vx = sin rad * bulletSpeed
      vy = cos rad * bulletSpeed
  in Bullet
       { bulletPos      = (x, y)
       , bulletVel      = (vx, vy)
       , bulletLifeTime = bulletLifetime
       , bulletOwner    = owner
       }

-- | Advances bullet positions and filters out expired projectiles
updateBullets :: [Bullet] -> Float -> [Bullet]
updateBullets bs dt =
  [ move dt (b { bulletLifeTime = bulletLifeTime b - dt })
  | b <- bs
  , bulletLifeTime b - dt > 0
  ]

-- | Recursively generates a specified number of initial random large asteroids
generateAsteroids :: Int -> StdGen -> ([Asteroid], StdGen)
generateAsteroids 0 gen = ([], gen)
generateAsteroids n gen =
  let (ast, gen1)      = randomAsteroid gen
      (rest, finalGen) = generateAsteroids (n - 1) gen1
  in (ast : rest, finalGen)

-- | Decrements remaining lifespan of explosion effects and drops expired instances
updateExplosions :: [Explosion] -> Float -> [Explosion]
updateExplosions exps dt =
  [ e { expTimeLeft = expTimeLeft e - dt } | e <- exps, expTimeLeft e - dt > 0 ]

-- | Physical collision radiuses associated with asteroid sizes
asteroidRadius :: AsteroidSize -> Float
asteroidRadius Large  = 30
asteroidRadius Medium = 20
asteroidRadius Small  = 10

-- | Physical collision radiuses for ship and bullet entities
shipRadius, bulletRadius :: Float
shipRadius   = 10
bulletRadius = 2.5

-- | Total duration for explosion animation lifecycle
explosionTimer :: Float
explosionTimer = 0.5

-- | Entities that occupy space and can collide
class Collidable a where
  getPos    :: a -> Vector
  getRadius :: a -> Float

  -- | Collision checker uses overlap of circular collision bound
  collidesWith :: Collidable b => a -> b -> Bool
  collidesWith a b = circlesOverlap (getPos a, getRadius a) (getPos b, getRadius b)

-- | Entities that move through velocity updates and position wrapping
class Collidable a => Movable a where
  getVel :: a -> Vector
  setPos :: Vector -> a -> a

  -- | Default movement step over the time delta dt
  move :: Float -> a -> a
  move dt obj =
    let (x, y)   = getPos obj
        (vx, vy) = getVel obj
    in setPos (wrap (x + vx * dt, y + vy * dt)) obj

-- | Collidable and Movable instances for Ship
instance Collidable Ship where
  getPos    = shipPos
  getRadius = const shipRadius

instance Movable Ship where
  getVel = shipVel
  setPos pos s = s { shipPos = pos }

-- | Collidable and Movable instances for Bullet
instance Collidable Bullet where
  getPos    = bulletPos
  getRadius = const bulletRadius

instance Movable Bullet where
  getVel = bulletVel
  setPos pos b = b { bulletPos = pos }

-- | Collidable and Movable instances for Asteroid
instance Collidable Asteroid where
  getPos    = astPos
  getRadius = asteroidRadius . astSize

instance Movable Asteroid where
  getVel = astVel
  setPos pos a = a { astPos = pos }

-- | Computes shortest delta distance for wrapped screen coordinates
wrapDelta :: Float -> Float
wrapDelta d
  | d >  screenRadius = d - 2 * screenRadius
  | d < -screenRadius = d + 2 * screenRadius
  | otherwise         = d

-- | Checks overlap between two circular bounding areas
circlesOverlap :: (Vector, Float) -> (Vector, Float) -> Bool
circlesOverlap ((x1, y1), r1) ((x2, y2), r2) =
  let dx = wrapDelta (x2 - x1)
      dy = wrapDelta (y2 - y1)
  in dx * dx + dy * dy <= (r1 + r2) * (r1 + r2)

-- | Determines next smaller size category for destroyed asteroid
smaller :: AsteroidSize -> Maybe AsteroidSize
smaller Large  = Just Medium
smaller Medium = Just Small
smaller Small  = Nothing

-- | Splits hit asteroids into two smaller fragments, or removes them if they are already at its smallest size
splitAsteroid :: StdGen -> Asteroid -> ([Asteroid], StdGen)
splitAsteroid g a = case smaller (astSize a) of
  Nothing -> ([], g)
  Just sz ->
    let (ang, g1) = randomR (0, 2 * pi) g
        (spd, g2) = randomR (30, 80) g1
        v@(vx, vy) = (cos ang * spd, sin ang * spd)
        mk vel = a { astSize = sz, astVel = vel }
    in ([mk v, mk (-vx, -vy)], g2)

-- | Generates a random initial asteroid positioned anywhere on screen with non-zero speed
randomAsteroid :: StdGen -> (Asteroid, StdGen)
randomAsteroid g =
  let (x,   g1) = randomR (-250, 250) g
      (y,   g2) = randomR (-250, 250) g1
      (ang, g3) = randomR (0, 2 * pi) g2
      (spd, g4) = randomR (60, 120) g3
  in ( Asteroid { astPos  = (x, y)
                , astVel  = (cos ang * spd, sin ang * spd)
                , astSize = Large }
     , g4 )

-- | Interval in seconds between continuous asteroid spawns
spawnInterval :: Float
spawnInterval = 5

-- | Maximum number of asteroids allowed simultaneously before spawning stops
maxAsteroids :: Int
maxAsteroids = 10

-- | Spawns a new large asteroid entering from a random screen boundary
randomEdgeAsteroid :: StdGen -> (Asteroid, StdGen)
randomEdgeAsteroid g =
  let (pos, g1) = randomEdgePos g
      (ang, g2) = randomR (0, 2 * pi) g1
      (spd, g3) = randomR (20, 60) g2
  in ( Asteroid { astPos  = pos
                , astVel  = (cos ang * spd, sin ang * spd)
                , astSize = Large }
     , g3 )

-- | Updates ship orientation, velocity (thrust + drag), and position for frame step dt
updateShip :: Float -> Ship -> Ship
updateShip dt s = move dt (applyDrag (applyThrust (applyTurn s dt) dt) dt)

-- | Picks a random coordinate located directly along one of the 4 outer screen edges
randomEdgePos :: StdGen -> (Vector, StdGen)
randomEdgePos g =
  let (side, g1) = randomR (0 :: Int, 3) g
      (t,    g2) = randomR (-screenRadius, screenRadius) g1
      pos = case side of
              0 -> (-screenRadius, t)
              1 -> ( screenRadius, t)
              2 -> (t, -screenRadius)
              _ -> (t,  screenRadius)
  in (pos, g2)

-- | AI tracking logic governing UFO turn steering and velocity thrusting toward the player
ufoAI :: Ship -> Ship -> Ship
ufoAI player u =
  let (ux, uy) = shipPos u
      (px, py) = shipPos player
      dx       = wrapDelta (px - ux)           -- shortest way, also across the screen edge
      dy       = wrapDelta (py - uy)
      hx       = sin (heading u * pi / 180)    -- the direction the UFO nose points
      hy       = cos (heading u * pi / 180)
      side     = hx * dy - hy * dx             -- < 0: player is clockwise of the nose
      ahead    = hx * dx + hy * dy             -- > 0: player is in front of the nose
      (vx, vy) = shipVel u
      turn | side < 0  = TurnLeft              
           | otherwise = TurnRight
  in u { turnDir = turn, isThrusting = vx * vx + vy * vy < ufoSpeed * ufoSpeed }

-- | Steps UFO AI positioning and handles firing bullets when attack cooldown timer expires
stepUFO :: Float -> Ship -> Ship -> (Ship, [Bullet])
stepUFO dt player u =
  let u' = updateShip dt (ufoAI player u)
      cd = shootCooldown u' - dt
  in case () of
       _ | cd <= 0   -> (u' { shootCooldown = ufoShootInterval }, [fireBullet UFOBullet u'])
         | otherwise -> (u' { shootCooldown = cd }, [])

-- | Constructs a new UFO entity positioned along a screen boundary
spawnUFO :: StdGen -> (Ship, StdGen)
spawnUFO g =
  let (pos, g') = randomEdgePos g
  in (initialShip { shipPos = pos, invulnTime = 0, shootCooldown = ufoShootInterval }, g')

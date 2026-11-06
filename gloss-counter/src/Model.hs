{-# LANGUAGE DeriveGeneric #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

-- TODO
-- [x] Make astroids move from start
-- [x] Score counting
-- [x] High score page
-- [WIP] Store scores to computer
-- [x] Add space ships
-- [WIP] optional requirments
-- [] design document aanpassen in overleaf

import System.Random (StdGen, randomR)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

--Constants:
screenRadius :: Float
screenRadius = 300 

startingLives :: Int
startingLives = 3

respawnInvuln :: Float
respawnInvuln = 3

turnSpeed :: Float
turnSpeed = 180.0

thrustAccel :: Float
thrustAccel = 250.0

dragCoeff :: Float
dragCoeff = 0.98

bulletSpeed :: Float
bulletSpeed = 400.0

bulletLifetime :: Float
bulletLifetime = 2.0

initialScore :: Int
initialScore = 0

ufoSpeed, ufoShootInterval, ufoSpawnInterval :: Float
ufoSpeed         = 40     --maximum
ufoShootInterval = 2
ufoSpawnInterval = 15

maxUFOs, ufoPoints :: Int
maxUFOs   = 2
ufoPoints = 333

data Screen = Menu | Playing | Paused | GameOver | HighScores
  deriving(Eq, Show)

type Vector = ( Float , Float )
data TurnDir = TurnLeft | TurnRight | NoTurn
  deriving ( Eq , Show )

data HighScore = HighScore
  { hsTimestamp :: Integer
  , hsScore     :: Int
  } deriving (Show, Generic)
instance ToJSON HighScore
instance FromJSON HighScore

data Ship = Ship
  { shipPos :: Vector
  , shipVel :: Vector
  , heading :: Float
  , isThrusting :: Bool
  , turnDir :: TurnDir
  , shootCooldown :: Float
  , invulnTime :: Float
  } deriving ( Show )

data BulletOwner = PlayerBullet | UFOBullet
  deriving ( Eq , Show )

data Bullet = Bullet
  { bulletPos :: Vector
  , bulletVel :: Vector
  , bulletLifeTime :: Float
  , bulletOwner :: BulletOwner
  } deriving ( Show )

data AsteroidSize = Large | Medium | Small
  deriving ( Eq , Show )

data Asteroid = Asteroid
  { astPos :: Vector
  , astVel :: Vector
  , astSize :: AsteroidSize
  } deriving ( Show )

data Explosion = Explosion
  { expPos :: Vector
  , expScale :: Float
  , expTimeLeft :: Float
  } deriving ( Show )

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
                 } deriving(Show)

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

asteroidPoints :: AsteroidSize -> Int
asteroidPoints Large  = 25
asteroidPoints Medium = 50
asteroidPoints Small  = 100

applyTurn :: Ship -> Float -> Ship
applyTurn s dt = case turnDir s of
  TurnLeft  -> s { heading = heading s + turnSpeed * dt }
  TurnRight -> s { heading = heading s - turnSpeed * dt }
  NoTurn    -> s

applyThrust :: Ship -> Float -> Ship
applyThrust s dt
  | isThrusting s =
      let rad = heading s * (pi / 180.0)
          ax  = sin rad * thrustAccel * dt
          ay  = cos rad * thrustAccel * dt
          (vx, vy) = shipVel s
      in s { shipVel = (vx + ax, vy + ay) }
  | otherwise = s

applyDrag :: Ship -> Float -> Ship
applyDrag s dt =
  let (vx, vy) = shipVel s
      decay    = dragCoeff ** (dt * 60.0)
  in s { shipVel = (vx * decay, vy * decay) }

wrap :: Vector -> Vector
wrap (x, y) = (wrapCoord x, wrapCoord y)
  where
    wrapCoord c
      | c > screenRadius  = c - (2 * screenRadius)
      | c < -screenRadius = c + (2 * screenRadius)
      | otherwise         = c


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

updateBullets :: [Bullet] -> Float -> [Bullet]
updateBullets bs dt =
  [ move dt (b { bulletLifeTime = bulletLifeTime b - dt })
  | b <- bs
  , bulletLifeTime b - dt > 0
  ]

generateAsteroids :: Int -> StdGen -> ([Asteroid], StdGen)
generateAsteroids 0 gen = ([], gen)
generateAsteroids n gen =
  let (ast, gen1)      = randomAsteroid gen
      (rest, finalGen) = generateAsteroids (n - 1) gen1
  in (ast : rest, finalGen)
updateExplosions :: [Explosion] -> Float -> [Explosion]
updateExplosions exps dt =
  [ e { expTimeLeft = expTimeLeft e - dt } | e <- exps, expTimeLeft e - dt > 0 ]

asteroidRadius :: AsteroidSize -> Float
asteroidRadius Large  = 30
asteroidRadius Medium = 20
asteroidRadius Small  = 10

shipRadius, bulletRadius :: Float
shipRadius   = 10
bulletRadius = 2.5

explosionTimer :: Float
explosionTimer = 0.5

--Typeclasses and instances start here:

-- Entities that occupy space and can collide.
class Collidable a where
  getPos    :: a -> Vector
  getRadius :: a -> Float

  --Collsion checker uses overalap of the circles
  collidesWith :: Collidable b => a -> b -> Bool
  collidesWith a b = circlesOverlap (getPos a, getRadius a) (getPos b, getRadius b)

--Entities that move
class Collidable a => Movable a where
  getVel :: a -> Vector
  setPos :: Vector -> a -> a

  --Default movement step over the time delta dt.
  move :: Float -> a -> a
  move dt obj =
    let (x, y)   = getPos obj
        (vx, vy) = getVel obj
    in setPos (wrap (x + vx * dt, y + vy * dt)) obj

-- Instances ship
instance Collidable Ship where
  getPos    = shipPos
  getRadius = const shipRadius

instance Movable Ship where
  getVel = shipVel
  setPos pos s = s { shipPos = pos }

-- Instances bullet
instance Collidable Bullet where
  getPos    = bulletPos
  getRadius = const bulletRadius

instance Movable Bullet where
  getVel = bulletVel
  setPos pos b = b { bulletPos = pos }

-- Instances asteroid
instance Collidable Asteroid where
  getPos    = astPos
  getRadius = asteroidRadius . astSize

instance Movable Asteroid where
  getVel = astVel
  setPos pos a = a { astPos = pos }

-- ahortest distance wraparound
wrapDelta :: Float -> Float
wrapDelta d
  | d >  screenRadius = d - 2 * screenRadius
  | d < -screenRadius = d + 2 * screenRadius
  | otherwise         = d

-- squared distance, niet sqrt
circlesOverlap :: (Vector, Float) -> (Vector, Float) -> Bool
circlesOverlap ((x1, y1), r1) ((x2, y2), r2) =
  let dx = wrapDelta (x2 - x1)
      dy = wrapDelta (y2 - y1)
  in dx * dx + dy * dy <= (r1 + r2) * (r1 + r2)

--asteroid progression
smaller :: AsteroidSize -> Maybe AsteroidSize
smaller Large  = Just Medium
smaller Medium = Just Small
smaller Small  = Nothing

--asteroid halveert, als asteroid kleinst is dan verdwijnt die
splitAsteroid :: StdGen -> Asteroid -> ([Asteroid], StdGen)
splitAsteroid g a = case smaller (astSize a) of
  Nothing -> ([], g)
  Just sz ->
    let (ang, g1) = randomR (0, 2 * pi) g
        (spd, g2) = randomR (30, 80) g1
        v@(vx, vy) = (cos ang * spd, sin ang * spd)
        mk vel = a { astSize = sz, astVel = vel }
    in ([mk v, mk (-vx, -vy)], g2)

--Makes asteroids move from the start instead of standstil
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


spawnInterval :: Float
spawnInterval = 5      -- seconds between spawns

maxAsteroids :: Int
maxAsteroids = 10 --maximum asteroids at start

randomEdgeAsteroid :: StdGen -> (Asteroid, StdGen)
randomEdgeAsteroid g =
  let (pos, g1) = randomEdgePos g
      (ang, g2) = randomR (0, 2 * pi) g1
      (spd, g3) = randomR (20, 60) g2
  in ( Asteroid { astPos  = pos
                , astVel  = (cos ang * spd, sin ang * spd)
                , astSize = Large }
     , g3 )

updateShip :: Float -> Ship -> Ship
updateShip dt s = move dt (applyDrag (applyThrust (applyTurn s dt) dt) dt)

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

--AI of the ufo
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

stepUFO :: Float -> Ship -> Ship -> (Ship, [Bullet])
stepUFO dt player u =
  let u' = updateShip dt (ufoAI player u)
      cd = shootCooldown u' - dt
  in if cd <= 0
       then (u' { shootCooldown = ufoShootInterval }, [fireBullet UFOBullet u'])
       else (u' { shootCooldown = cd }, [])

spawnUFO :: StdGen -> (Ship, StdGen)
spawnUFO g =
  let (pos, g') = randomEdgePos g
  in (initialShip { shipPos = pos, invulnTime = 0, shootCooldown = ufoShootInterval }, g')


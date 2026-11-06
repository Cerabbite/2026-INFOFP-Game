module Constants where

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
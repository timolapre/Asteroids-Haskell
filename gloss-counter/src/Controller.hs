-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

{-moveStep = 5

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    do x <- return (returnX gstate)
       y <- return (returnY gstate)
       newNumber <- case gstate of GameState (ShowCircle _ _ r) _ -> return (r+0)
                                   GameState (ShowNothing) _ -> return 0
       return $ GameState (ShowCircle x y newNumber) 0
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }-}

-- | Handle user input
input :: Event -> GameState -> IO GameState
input event gstate = case state gstate of
                                Menu -> menuInput event gstate
                                Running -> runningInput event gstate
                                GameOver -> undefined
                                Paused -> pausedInput event gstate

menuInput :: Event -> GameState -> IO GameState
menuInput event gstate = case event of
                                (EventKey (SpecialKey KeySpace) Down _ _) -> return gstate {state = Running}
                                _ -> return gstate

pausedInput :: Event -> GameState -> IO GameState
pausedInput event gstate = case event of
                                (EventKey (Char 'p') Down _ _) -> return gstate {state = Running}
                                _ -> return gstate

newBullet :: Float -> Float -> Float -> Object
newBullet x y dir = Bullet {x=x, y=y, size=10, dir = dir}

runningInput :: Event -> GameState -> IO GameState
runningInput event gstate = case event of
                        (EventKey (Char 'w') Down _ _) -> do
                                                                return gstate {cntrls = (cntrls gstate) {forward = True}}
                        (EventKey (Char 's') Down _ _) -> do
                                                                return gstate {cntrls = (cntrls gstate) {backward = True}}
                        (EventKey (Char 'a') Down _ _) -> do
                                                                return gstate {cntrls = (cntrls gstate) {left = True}}
                        (EventKey (Char 'd') Down _ _) -> do
                                                                return gstate {cntrls = (cntrls gstate) {right = True}}
                        (EventKey (SpecialKey KeySpace) Down _ _) -> do
                                                                return gstate {objects = objects gstate ++ [newBullet (x (objects gstate!!0)) (y (objects gstate!!0)) (dir (objects gstate!!0))]}
                        (EventKey (Char 'w') Up _ _) -> do
                                                                return gstate {cntrls = (cntrls gstate) {forward = False}}
                        (EventKey (Char 's') Up _ _) -> do
                                                                return gstate {cntrls = (cntrls gstate) {backward = False}}
                        (EventKey (Char 'a') Up _ _) -> do
                                                                return gstate {cntrls = (cntrls gstate) {left = False}}
                        (EventKey (Char 'd') Up _ _) -> do
                                                                return gstate {cntrls = (cntrls gstate) {right = False}}
                        (EventKey (Char 'p') Up _ _) -> do
                                                                return gstate {state = Paused}
                        _ -> return gstate


movePlayer :: Object -> Input -> Object
movePlayer obj inpt = do
                        let o1 = if left inpt then Controller.rotate obj (-1) else obj
                        let o2 = if right inpt then Controller.rotate o1 1 else o1
                        let direction = dir o2
                        let Vec2(vx,vy) = speed o2
                        let inc = vx / 60
                        let dx = if forward inpt && lngt (vx,vy) < 2 then vx + 0.1 * sin(direction * pi/180) - inc else vx - inc
                        let inc = vy / 60
                        let dy = if forward inpt && lngt (vx,vy) < 2 then vy + 0.1 * cos(direction * pi/180) - inc else vy - inc
                        o2{x = x o2 + dx, y = y o2 + dy, speed = Vec2(dx,dy)}
                      where lngt (vx,vy) = sqrt(vx*vx + vy*vy)

rotate :: Object -> Float -> Object
rotate obj n = obj {dir = (dir obj) + n}

moveDir :: Object -> Float -> Float -> Object
moveDir obj dir n = obj {y = (y obj) + n * cos(dir * pi/180), x = (x obj) + n * sin(dir * pi/180)}

randomNumber :: Float -> Float -> IO Float
randomNumber low high = randomRIO(low,high)

step :: Float -> GameState -> IO GameState
step secs gstate = case state gstate of
                      Menu -> menuStep secs gstate
                      Running -> runningStep secs gstate
                      GameOver -> gameoverStep secs gstate
                      Paused -> pausedStep secs gstate

newAsteroid :: IO Object
newAsteroid = do
                let posoffset = 0
                randPos <- randomNumber 0 3
                randSize <- randomNumber 20 50
                randDir <- randomNumber (-45) 45
                case round randPos of
                      0 -> do randX <- randomNumber (-384+posoffset) (384-posoffset)
                              return Asteroid {x = randX, y = (-256-30), size = randSize, dir = randDir}
                      1 -> do randY <- randomNumber (-256+posoffset) (256-posoffset)
                              return Asteroid {x = (-384-30), y = randY, size = randSize, dir = randDir + (randPos*90)}
                      2 -> do randX <- randomNumber (-384+posoffset) (384-posoffset)
                              return Asteroid {x = randX, y = (256+30), size = randSize, dir = randDir + (randPos*90)}
                      3 -> do randY <- randomNumber (-256+posoffset) (256-posoffset)
                              return Asteroid {x = (384+30), y = randY, size = randSize, dir = randDir + (randPos*90)}

newAst :: Object
newAst = Asteroid {x = 0, y = 0, size = 50, dir = 0}

menuStep :: Float -> GameState -> IO GameState
menuStep secs gstate = return gstate

runningStep :: Float -> GameState -> IO GameState
runningStep secs gstate = do
                            let objs = objects gstate
                            let player = movePlayer (objs!!0) (cntrls gstate)
                            let newAsteroids = concat [splitAsteroid (moveDir obj (dir obj) 0.7) (getBullets(objects gstate)) | obj <- getAsteroids(objects gstate), abs (x obj) <= 668, abs (y obj) <= 412]
                            let newBullets = [moveDir obj (dir obj) 5 | obj <- getBullets(objects gstate), abs (x obj) <= 668, abs (y obj) <= 412, collideList (getAsteroids (objects gstate)) obj == False]
                            let newAliens = [moveDir obj (dir (newAlienDirection obj player)) 0.5 | obj <- getAliens(objects gstate), abs (x obj) <= 668, abs (y obj) <= 412]
                            case elapsedTime gstate + secs >= 0.75 of
                              True -> do
                                    newast <- newAsteroid
                                    return $ (gstate {elapsedTime = 0, objects = player : newAsteroids ++ newBullets ++ newAliens ++ [newast]})
                              _ -> do
                                    return $ (gstate {elapsedTime = elapsedTime gstate + secs, objects = player : newAsteroids ++ newBullets ++ newAliens})

gameoverStep :: Float -> GameState -> IO GameState
gameoverStep secs gstate = return gstate

pausedStep :: Float -> GameState -> IO GameState
pausedStep secs gstate = return gstate

collide :: Object -> Object -> Bool
collide obj1@Player{} obj2@Asteroid{} | (x obj2 - x obj1)^2 + (y obj1 - y obj2)^2 <= (size obj1 + size obj2)^2 = True
                                      | otherwise = False
collide obj1@Asteroid{} obj2@Bullet{} | (x obj2 - x obj1)^2 + (y obj1 - y obj2)^2 <= (size obj1 + size obj2)^2 = True
                                      | otherwise = False
collide obj1@Bullet{} obj2@Asteroid{} | (x obj2 - x obj1)^2 + (y obj1 - y obj2)^2 <= (size obj1 + size obj2)^2 = True
                                      | otherwise = False
collide _ _ = False

checkCollision :: Object -> Object -> [Object] -> [Object]
checkCollision obj1 obj2 list   | collide obj1 obj2 = list ++ [newAst]
                                | otherwise = list

collideList :: [Object] -> Object -> Bool
collideList list obj = elem True [collide x obj | x <- list]

splitAsteroid :: Object -> [Object] -> [Object]
splitAsteroid obj list  | collideList list obj && size obj > 30 = [obj {dir = dir obj + 45, size = size obj-15}, obj {dir = dir obj - 45, size = size obj-15}]
                        | collideList list obj && size obj <= 30 = []
                        | otherwise = [obj]


newAlienDirection :: Object -> Object -> Object
newAlienDirection alien player  | x alien-x player < 0 = alien{dir = 180+270-atan((y alien-y player)/(x alien-x player))* 180/pi}
                                | otherwise = alien{dir = 270-atan((y player-y alien)/(x player-x alien))* 180/pi}



-- \/\/\/ Get Objects from onscreen list \/\/\/
getAsteroids :: [Object] -> [Object]
getAsteroids list = [x | x <- list, isAsteroid x]

isAsteroid :: Object -> Bool
isAsteroid Asteroid{} = True
isAsteroid _ = False

getBullets :: [Object] -> [Object]
getBullets list = [x | x <- list, isBullet x]

isBullet :: Object -> Bool
isBullet Bullet{} = True
isBullet _ = False

getAliens :: [Object] -> [Object]
getAliens list = [x | x <- list, isAlien x]

isAlien :: Object -> Bool
isAlien AlienShip{} = True
isAlien _ = False

getPlayers :: [Object] -> [Object]
getPlayers list = [x | x <- list, isPlayer x]

isPlayer :: Object -> Bool
isPlayer Player{} = True
isPlayer _ = False

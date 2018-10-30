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
newBullet x y dir = Bullet {x=x, y=y, size=10, colour=bulletColor, dir = dir}

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
                        let o3 = if forward inpt then moveDir o2 (dir o2) 1 else o2
                        let o4 = if backward inpt then moveDir o3 (dir o3) (-1) else o3
                        o4

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
                              return Asteroid {x = randX, y = (-256-30), size = randSize, colour = asteroidColor, dir = randDir}
                      1 -> do randY <- randomNumber (-256+posoffset) (256-posoffset)
                              return Asteroid {x = (-384-30), y = randY, size = randSize, colour = asteroidColor, dir = randDir + (randPos*90)}
                      2 -> do randX <- randomNumber (-384+posoffset) (384-posoffset)
                              return Asteroid {x = randX, y = (256+30), size = randSize, colour = asteroidColor, dir = randDir + (randPos*90)}
                      3 -> do randY <- randomNumber (-256+posoffset) (256-posoffset)
                              return Asteroid {x = (384+30), y = randY, size = randSize, colour = asteroidColor, dir = randDir + (randPos*90)}

newAst :: Object
newAst = Asteroid {x = 0, y = 0, size = 50, colour = asteroidColor, dir = 0}

menuStep :: Float -> GameState -> IO GameState
menuStep secs gstate = return gstate

runningStep :: Float -> GameState -> IO GameState
runningStep secs gstate = do
                            let objs = objects gstate
                            let player = movePlayer (objs!!0) (cntrls gstate)
                            let moveast = [moveDir obj (dir obj) 1 | obj <- tail (objects gstate), abs (x obj) <= 668, abs (y obj) <= 412] --, collide ((objects gstate)!!0) obj == False]
                            case elapsedTime gstate + secs >= 0.75 of
                              True -> do
                                    newast <- newAsteroid
                                    return $ (gstate {elapsedTime = 0, objects = player : moveast ++ [newast]})
                              _ -> do
                                    return $ (gstate {elapsedTime = elapsedTime gstate + secs, objects = player : moveast})

gameoverStep :: Float -> GameState -> IO GameState
gameoverStep secs gstate = return gstate

pausedStep :: Float -> GameState -> IO GameState
pausedStep secs gstate = return gstate

collide :: Object -> Object -> Bool
collide obj1 obj2 | (x obj2 - x obj1)^2 + (y obj1 - y obj2)^2 <= (size obj1 + size obj2)^2 = True
                  | otherwise = False

checkCollision :: Object -> Object -> [Object] -> [Object]
checkCollision obj1 obj2 list   | collide obj1 obj2 = list ++ [newAst]
                                | otherwise = list

getObjects :: [Object] -> String -> [Object]
getObjects list name = 
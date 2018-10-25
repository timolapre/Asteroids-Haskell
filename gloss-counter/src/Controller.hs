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
input event gstate = case event of
                        (EventKey (Char 'w') Down _ _) -> do
                                                                  let p = move (objects gstate !! 0) North 10
                                                                  let newlist = tail (objects gstate)
                                                                  return gstate {objects = p : newlist}
                        (EventKey (Char 's') Down _ _) -> do   
                                                                  let p = move (objects gstate !! 0) South 10
                                                                  let newlist = tail (objects gstate)
                                                                  return gstate {objects = p : newlist}
                        (EventKey (Char 'd') Down _ _) -> do
                                                                  let p = move (objects gstate !! 0) East 10
                                                                  let newlist = tail (objects gstate)
                                                                  return gstate {objects = p : newlist}
                        (EventKey (Char 'a') Down _ _) -> do
                                                                  let p = move (objects gstate !! 0) West 10
                                                                  let newlist = tail (objects gstate)
                                                                  return gstate {objects = p : newlist}
                        _ -> return gstate

move :: Object -> Direction -> Float -> Object
move obj dir n = case dir of
                    North -> obj {y = (y obj) + n}
                    South -> obj {y = (y obj) - n}
                    East -> obj {x = (x obj) + n}
                    West -> obj {x = (x obj) - n}

moveDir :: Object -> Float -> Float -> Object
moveDir obj dir n = obj {y = (y obj) + n * cos(dir * pi/180), x = (x obj) + n * sin(dir * pi/180)}

randomNumber :: Float -> Float -> IO Float
randomNumber low high = randomRIO(low,high)

step :: Float -> GameState -> IO GameState
step secs gstate = case state gstate of
                      Menu -> undefined
                      Running -> runningstep secs gstate
                      GameOver -> undefined
                      Paused -> undefined

posoffset = 60

newAsteroid :: IO Object
newAsteroid = do
                randPos <- randomNumber 0 3
                randSize <- randomNumber 20 50
                randDir <- randomNumber (-45) 45
                case round randPos of
                      0 -> do randX <- randomNumber (-384+posoffset) (384-posoffset)
                              return Asteroid {x = randX, y = (-512-30), size = randSize, colour = asteroidColor, dir = randDir}
                      1 -> do randY <- randomNumber (-256+posoffset) (256-posoffset)
                              return Asteroid {x = (-384-30), y = randY, size = randSize, colour = asteroidColor, dir = randDir + (randPos*90)}
                      2 -> do randX <- randomNumber (-384+posoffset) (384-posoffset)
                              return Asteroid {x = randX, y = (512+30), size = randSize, colour = asteroidColor, dir = randDir + (randPos*90)}
                      3 -> do randY <- randomNumber (-256+posoffset) (256-posoffset)
                              return Asteroid {x = (384+30), y = randY, size = randSize, colour = asteroidColor, dir = randDir + (randPos*90)}
                      
                      

runningstep :: Float -> GameState -> IO GameState
runningstep secs gstate | elapsedTime gstate + secs >= 0.75
                        = do
                            newast <- newAsteroid
                            let moveast = [moveDir obj (dir obj) 1 | obj <- tail (objects gstate), abs (x obj) <= (768+50), abs (y obj) <= (512+50)]
                            return $ (gstate {elapsedTime = 0, objects = (objects gstate)!!0 : moveast ++ [newast]})
                        | otherwise
                        = do
                            let moveast = [moveDir obj (dir obj) 1 | obj <- tail (objects gstate), abs (x obj) <= (768+50), abs (y obj) <= (512+50)]
                            return $ (gstate {elapsedTime = elapsedTime gstate + secs, objects = (objects gstate)!!0 : moveast})
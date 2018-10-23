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
                        (EventKey (Char 'w') Down _ _) | elapsedTime gstate > 0.1 -> do
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
step secs gstate = do
                  rand <- randomNumber 0 360
                  let addast = [Asteroid {x = 0, y = 0, size = 30, colour = asteroidColor, dir = rand}]
                  let newast = [moveDir obj (dir obj) 1 | obj <- tail (objects gstate), abs (x obj) <= 768, abs (y obj) <= 512]
                  return (gstate {elapsedTime = (elapsedTime gstate) + secs, objects = (objects gstate)!!0 : newast ++ addast})
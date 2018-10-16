-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random


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
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'w') Down _ _) gstate
  = gstate { infoToShow = (ShowCircle (returnX gstate) (returnY gstate +10) (returnR gstate))}
inputKey (EventKey (Char 'a') Down _ _) gstate
  = gstate { infoToShow = (ShowCircle (returnX gstate -10) (returnY gstate) (returnR gstate))}
inputKey (EventKey (Char 's') Down _ _) gstate
  = gstate { infoToShow = (ShowCircle (returnX gstate) (returnY gstate -10) (returnR gstate))}
inputKey (EventKey (Char 'd') Down _ _) gstate
  = gstate { infoToShow = (ShowCircle (returnX gstate +10) (returnY gstate) (returnR gstate))}
inputKey _ gstate = gstate -- Otherwise keep the same


-- shit made by moi wat denk beter kan
returnX :: GameState -> Float
returnX g@(GameState (ShowCircle x _ _) _) = x

returnY :: GameState -> Float
returnY g@(GameState (ShowCircle _ y _) _) = y

returnR :: GameState -> Float
returnR g@(GameState (ShowCircle _ _ r) _) = r
-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowCircle  {x :: Float, y :: Float, size :: Float}
                | ShowANumber Int
                | ShowAChar   Char

data Enemy = Player InfoToShow | Asteroid InfoToShow | AlienShip InfoToShow
data Direction = Upm | Rightm | Downm | Leftm

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 0.1

data GameState = GameState {
                   infoToShow :: InfoToShow
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState (ShowCircle 0 0 100) 0
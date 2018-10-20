-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case state gstate of
                    Running -> showRunState gstate
                    GameOver -> undefined
                    Paused -> undefined

showRunState gstate = case objects gstate of
                        x -> pictures (map toPicture x) 

toPicture :: Objects -> Picture
toPicture object = case object of
                Player info -> Color (colour info) (translate (x info) (y info) (circle (size info)))
                Asteroid info -> Color (colour info) (translate (x info) (y info) (circle (size info)))
                AlienShip info -> Color (colour info) (translate (x info) (y info) (circle (size info)))
                Bullet info -> Color (colour info) (translate (x info) (y info) (circle (size info)))

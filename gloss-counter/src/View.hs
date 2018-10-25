-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case state gstate of
                    Menu -> undefined
                    Running -> showRunState gstate
                    GameOver -> undefined
                    Paused -> undefined

showRunState gstate = case objects gstate of
                        x -> pictures (map toPicture x) 

toPicture :: Object -> Picture
toPicture object = case object of
                Player x y size color -> Color color (translate x y (circle size))
                Asteroid x y size color dir -> Color color (translate x y (circle size))
                AlienShip x y size color -> Color color (translate x y (circle size))
                Bullet x y size color -> Color color (translate x y (circle size))

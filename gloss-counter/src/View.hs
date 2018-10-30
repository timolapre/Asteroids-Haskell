-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case state gstate of
                    Menu -> showMenuState gstate
                    Running -> showRunState gstate
                    GameOver -> undefined
                    Paused -> showPausedState gstate

showRunState gstate = case objects gstate of
                        x -> pictures (map toPicture x)

showMenuState gstate = pictures (map toPicture menuState)

showPausedState gstate = pictures (map toPicture (objects gstate) ++ (map toPicture pausedState))

toPicture :: Object -> Picture
toPicture object = case object of
                Player x y size dir -> Color playerColor $ translate x y $ rotate dir $ Polygon [(size/2,-size/2),(-size/2,-size/2),(0,size/1.5)]
                Asteroid x y size dir -> Color asteroidColor $ translate x y $ ThickCircle size 3
                AlienShip x y size dir -> Color alienColor $ translate x y $ Polygon [(size/2,size/2),(size/2,-size/2),(-size/2,-size/2),(-size/2,size/2)]
                Bullet x y size dir -> Color bulletColor $ translate x y $ circle size
                Tekst x y string -> color textColor $ translate x y $ Scale 0.3 0.3 $ Text string

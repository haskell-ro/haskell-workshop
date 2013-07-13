`code.hs`
```
{-# LANGUAGE RecordWildCards #-}

module Main where

import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Pacman
import System.Posix.Unistd
import Control.Monad
import Data.IORef

data GlobalData = GD
  { ix :: Int
  , imgs :: [((Int, Int), Image)]
  }

main = do
  initGUI
  window <- windowNew
  window `on` deleteEvent $ liftIO mainQuit >> return False
  imgs <- populate window
  widgetShowAll window
  ref <- newIORef $ GD 0 imgs
  timeoutAdd (update ref) 500
  mainGUI

populate w = do
  windowSetDefaultSize w 50 50
  mainVBox <- vBoxNew False 0
  btn <- buttonNewWithLabel "Start game"
  boxPackStart mainVBox btn PackNatural 5
  table <- tableNew 11 7 True
  images <- mapM (attachImg table "imgs/empty.png") [(y, x) | x <- [0 .. 10], y <- [0 .. 6]]
  displayTableElements images
  boxPackEnd mainVBox table PackRepel 5
  w `containerAdd` mainVBox
  return images

displayTableElements images = do
  mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $ filter (\((x, y), _) -> x == 1 && y > 0 && y < 10) images
  mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $ filter (\((x, y), _) -> x == 5 && y > 0 && y < 10) images
  mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $ filter (\((x, y), _) -> y == 1 && x > 0 && x < 6) images
  mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $ filter (\((x, y), _) -> y == 5 && x > 0 && x < 6) images
  mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $ filter (\((x, y), _) -> y == 9 && x > 0 && x < 6) images

attachImg tb path (x, y) = do
  img <- imageNewFromFile path
  tableAttach tb img x (x + 1) y (y + 1) [] [] 1 1
  return ((x, y), img)

runGame images pac_pos ghost_pos = do
  mapM_ (flip imageSetFromFile "imgs/pacman.png" . snd) $ filter (\((x, y), _) -> x == fromInteger (snd pac_pos) && y == fromInteger (fst pac_pos)) images
  mapM_ (flip imageSetFromFile "imgs/ghost.png" . snd) $ filter (\((x, y), _) -> x == fromInteger (snd ghost_pos) && y == fromInteger (fst ghost_pos)) images

redraw imgs ix = do
  let (pp, gp, c) = movement_list !! ix
  displayTableElements imgs
  runGame imgs pp gp

update ref = do
  GD{..} <- readIORef ref
  redraw imgs ix
  writeIORef ref $ GD (ix + 1) imgs
  return True
```

`Pacman.hs`
```
module Pacman where

pac0 = [(1,5),(1,4),(1,3),(1,2),(1,1),(2,1),(3,1),(4,1),(5,1),(5,2),(5,3),
        (5,4),(5,5),(6,5),(7,5),(8,5),(9,5),(9,4),(9,3),(9,2),(9,1)]

ghost0 = [(9,1),(9,2),(9,3),(9,4),(9,5),(8,5),(7,5),(6,5),(5,5),(4,5),(3,5),
        (2,5),(1,5),(1,4),(1,3),(1,2),(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),
        (7,1),(8,1)]

pac1 = pac0 ++ (reverse $ init pac0)
ghost1 = ghost0 ++ (reverse $ init ghost0)

pac = x where x = pac1 ++ x
ghost = x where x = ghost1 ++ x
drop_interval = 7
drops = [ f x | x <- [1..] ] where
  f x = if x `mod` drop_interval == 0 then
      1
      else 0

game_list = zipWith func pac ghost where
    func pac_pos ghost_pos =
        if pac_pos == ghost_pos then
            -1
            else  1

result = takeWhile (>= 0) game_list
final = 0 : [(sum final) + x | x <- result]

drops2 = zipWith f ghost drops where
    f x y = if y == 1 then x
      else (0,0)

len = length result
all_pac = take len pac
all_drops = take len drops2

drop_list = map (+(-1)) $ iterate (+drop_interval) drop_interval
score = [(head $ drop x all_drops) `elem` (drop x all_pac) | x <- drop_list]
score_int = [ f x | x <- score] where
  f x 
    | x == True = 1
    | otherwise = 0 


movement_list = zip3 pac ghost drops 

first (x,_,_) = x
second (_,y,_) = y
third (_,_,z) = z
```



module Main where

import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Control.Concurrent

data IORState = IORScons
  { score :: Int
	, pacmanStream :: [(Int, Int)]
        , ghostStream :: [(Int, Int)]
	, dangerousStream :: [(Int, Int)]
	}

main = do
  initGUI
  window <- windowNew
  window `on` deleteEvent $ liftIO mainQuit >> return False
  populate window
  widgetShowAll window
  mainGUI

populate w = do
  windowSetDefaultSize w 50 50
  mainVBox <- vBoxNew False 0
  btn <- buttonNewWithLabel "Start game"
  boxPackStart mainVBox btn PackNatural 5
  table <- tableNew 11 7 True
  --displayTableElements table 8 (head pacman) (head pacman)
  movement table 0 pacman ghost
  boxPackEnd mainVBox table PackRepel 5
  w `containerAdd` mainVBox

{-
  displayTableElements tb = do
  images <- mapM (attachImg tb "imgs/empty.png") [(y, x) | x <- [0 .. 10], y <- [0 .. 6]]
  mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $ filter (\((x, y), _) -> x == 1 && y > 0 && y < 10) images
  mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $ filter (\((x, y), _) -> x == 5 && y > 0 && y < 10) images
  mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $ filter (\((x, y), _) -> y == 1 && x > 0 && x < 6) images
  mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $ filter (\((x, y), _) -> y == 5 && x > 0 && x < 6) images
  mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $ filter (\((x, y), _) -> y == 9 && x > 0 && x < 6) images
  return ()
-}

gameOver time pman gh = (pman == gh && time `mod` 8 == 0)

movement table time pacs ghosts = do	
  displayTableElements table time (head pacs) (head ghosts)
  --if (gameOver time (head pacs) (head ghosts)) then
   -- return()
  --else
   -- movement table (time + 1) (tail pacs) (tail ghosts)

displayTableElements tb time pman gh = do
  images <- mapM (attachImg tb "imgs/empty.png") [(y, x) | x <- [0 .. 10], y <- [0 .. 6]]
  mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $ filter (\((x, y), _) -> x == 1 && y > 0 && y < 10) images
  mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $ filter (\((x, y), _) -> x == 5 && y > 0 && y < 10) images
  mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $ filter (\((x, y), _) -> y == 1 && x > 0 && x < 6) images
  mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $ filter (\((x, y), _) -> y == 5 && x > 0 && x < 6) images
  mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $ filter (\((x, y), _) -> y == 9 && x > 0 && x < 6) images
  mapM_ (flip imageSetFromFile "imgs/pacman.png" .snd) $ filter (\((x,y), _) -> (y, x) == pman) images
  if (time `mod` 8 == 0) then
    ( if (pman == gh) then
	mapM_ (flip imageSetFromFile "imgs/skull.png" .snd) $ filter (\((x, y), _) -> (y, x) == gh) images
      else
        mapM_ (flip imageSetFromFile "imgs/danger.png" .snd) $ filter (\((x, y), _) -> (y, x) == gh) images)
  else
    ( if (time `mod` 7 == 0) then 
        mapM_ (flip imageSetFromFile "imgs/fruit.png" .snd) $ filter (\((x, y), _) -> (y, x) == gh) images
      else
        mapM_ (flip imageSetFromFile "imgs/ghost.png" .snd) $ filter (\((x, y), _) -> (y, x) == gh) images )
  return ()
  
attachImg tb path (x, y) = do
  img <- imageNewFromFile path
  tableAttach tb img x (x + 1) y (y + 1) [] [] 1 1
  return ((x, y), img)

pacmanpos  = [(1, x) | x <- reverse[1..5]] ++
          [(y, 1) | y <- [2..4]] ++
          [(5, x) | x <- [1..5]] ++
          [(y, 5) | y <- [6..8]] ++
          [(9, x) | x <- reverse[1..5]]
auxpacman = take ((length pacmanpos) * 2 - 2) (pacmanpos ++ tail (reverse pacmanpos))

pacman = auxpacman ++ pacman

ghostpos = [(y, 1) | y <- reverse [1..9]] ++
        [(1, x) | x <- [2..4]] ++
        [(y, 5) | y <- [1..9]] ++
        [(9, x) | x <- reverse[2..4]]
 
ghost = ghostpos ++ ghost


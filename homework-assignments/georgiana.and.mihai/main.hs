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

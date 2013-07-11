module Main where

import Graphics.Rendering.Cairo
import Graphics.UI.Gtk

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
  displayTableElements table
  boxPackEnd mainVBox table PackRepel 5
  w `containerAdd` mainVBox

displayTableElements tb = do
  images <- mapM (attachImg tb "imgs/empty.png") [(y, x) | x <- [0 .. 10], y <- [0 .. 6]]
  mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $ filter (\((x, y), _) -> x == 1 && y > 0 && y < 10) images
  mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $ filter (\((x, y), _) -> x == 5 && y > 0 && y < 10) images
  mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $ filter (\((x, y), _) -> y == 1 && x > 0 && x < 6) images
  mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $ filter (\((x, y), _) -> y == 5 && x > 0 && x < 6) images
  mapM_ (flip imageSetFromFile "imgs/road.png" . snd) $ filter (\((x, y), _) -> y == 9 && x > 0 && x < 6) images
  return ()

attachImg tb path (x, y) = do
  img <- imageNewFromFile path
  tableAttach tb img x (x + 1) y (y + 1) [] [] 1 1
  return ((x, y), img)


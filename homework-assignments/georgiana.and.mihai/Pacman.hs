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

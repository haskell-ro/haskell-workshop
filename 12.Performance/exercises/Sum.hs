import Data.List (foldl')

mySum [] = 0
mySum (x : xs) = x + mySum xs

mySum2 = go 0
  where
  go acc []       = acc
  go acc (x : xs) = go (x + acc) xs

mySum3 = foldl (+) 0

mySum4 = go 0
  where
  go acc []       = acc
  go acc (x : xs) = seq (x + acc) $ go (x + acc) xs

mySum5 = foldl' (+) 0

n = 1000000
main = print $ mySum5 [1..n]


fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

betterFib n
  | n == 0    = 0
  | otherwise = go (0,1) 1
  where
  go (f0, f1) n1 = if n == n1
    then f1
    else go (f1, f0 + f1) (n1 + 1)

n = 30
main = print $ fib n

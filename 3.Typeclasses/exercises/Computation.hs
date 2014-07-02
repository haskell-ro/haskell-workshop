
-- let comp m = m >>= return . (2 +) >>= return . (* 2)

computation :: Num a => Maybe a -> Maybe a
computation m = do
  x <- m
  let y = 2 + x
  return $ y * 2

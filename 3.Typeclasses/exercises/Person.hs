data Person = P
  { pnc :: String
  , name :: String
  , age :: Int
  , married :: Bool
  } deriving Show

instance Eq Person where
  p1 == p2 = pnc p1 == pnc p2

data Person2 a = P2
  { pnc2 :: a
  , name2 :: String
  , age2 :: Int
  , married2 :: Bool
  } deriving Show

instance Eq a => Eq (Person2 a) where
  p1 == p2 = pnc2 p1 == pnc2 p2 

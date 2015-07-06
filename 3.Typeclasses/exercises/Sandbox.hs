import Data.Function
import Data.Maybe

data Container a = Empty | Holding a

instance Show a => Show (Container a) where
  show Empty        = "[ ]"
  show (Holding x)  = "[ " ++ show x ++ " ]"

instance Eq a => Eq (Container a) where
  Empty     ==  Empty     = True
  Holding x == Holding y  = x == y
  _         ==  _         = False

instance Functor Container where
  fmap _ Empty        = Empty
  fmap f (Holding x)  = Holding $ f x




data Person = P
  { pnc :: String
  , name :: String
  , age :: Int
  , married :: Bool
  } deriving Show

instance Eq Person where
  (==) = (==) `on` pnc

class YesNo a where
  yesno :: a -> Bool

instance YesNo Bool where
  yesno = id

instance YesNo [a] where
  yesno = not . null

instance YesNo (Maybe a) where
  yesno = isJust

-- if yesno x then ... else ...

computation :: Num a => Maybe a -> Maybe a
computation m = do
  x <- m
  let y = x + 2
  return $ y * 5

cart :: [(Int, Int)]
cart = [ (x, y) | x <- [1,2,3], y <- [1,2,3] ]

cart' :: [(Int, Int)]
cart' = do
  x <- [1,2,3]
  y <- [1,2,3]
  return (x, y)

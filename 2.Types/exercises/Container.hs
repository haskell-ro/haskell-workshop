import Data.Either

data Container a = Empty | Holding a

isEmpty :: Container a -> Bool
isEmpty Empty = True
isEmpty _     = False

getVal :: Container a -> b -> Either a b
getVal (Holding x) _ = Left x
getVal Empty y       = Right y

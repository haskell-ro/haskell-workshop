data Container a = Empty | Holding a
  deriving (Show, Eq)

--instance Show (Container a) where
--  show Empty       = "[ ]"
--  show (Holding x) = "[ " ++ "x" ++ " ]"

instance Functor Container where
  fmap _ Empty = Empty
  fmap f (Holding x) = Holding $ f x

f :: a -> a
f x = x

add :: Int -> Int -> Int
add x y = x + y

plustwo :: Int -> Int
plustwo = add 2

data Colour = Red | Green | Blue

data Person =
    Male String Int
  | Female String String Int

data Container a = Empty | Holding a
  deriving (Show, Eq)

isEmpty :: Container a -> Bool
isEmpty Empty   = True
isEmpty _       = False

place :: Container a -> a -> Container a
place Empty x = Holding x
place _ _     = error "Container full"

data N = Zero | Succ N

data List a = EmptyL | ConsL a (List a)

data BinaryTree a =
    BTEmpty
  | BTNode (BinaryTree a) a (BinaryTree a)
newtype Nat = MkNat Int
  deriving Show

toNat :: Int -> Nat
toNat n
  | n >= 0    = MkNat n
  | otherwise = error "Not a natural number"

data Person2 = P
  { name :: String
  , address :: String
  , nationality :: String
  , age :: Int
  , numberOfChildren :: Int
  , father :: Person2
  , mother :: Person2
  }

data RoseTree a =
    RTLeaf a
  | RTNode a [RoseTree a]
  deriving (Show, Eq)

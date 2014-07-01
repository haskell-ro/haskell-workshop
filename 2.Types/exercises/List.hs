data List a = EmptyL | ConsL a (List a)

myHead :: List a -> Maybe a
myHead EmptyL       = Nothing
myHead (ConsL x _) = Just x

main :: IO ()
main = undefined

data Person =
    Male
    { maleName :: String
    , maleAge :: Int
    }
  | Female
    { femaleName :: String
    , femaleMaidenName :: String
    , femaleAge :: Int
    }

john :: Person
john = Male "John Smith" 42

lisa :: Person
lisa = Female "Lisa Simpson" "Lisa Simpson" 8

john2 = ("John Smith", 42)

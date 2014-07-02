class YesNo a where
  yesno :: a -> Bool

instance YesNo Bool where
  yesno = id

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno = not . null

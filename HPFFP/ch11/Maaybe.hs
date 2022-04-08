isJust :: Maybe a -> Bool
isJust Nothing = False 
isJust _ = True 

isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee n _ Nothing = n
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr (flip go) (Just []) 
    where go :: Maybe [a] -> Maybe a -> Maybe [a]
          go Nothing _ = Nothing 
          go _ Nothing = Nothing
          go (Just xs) (Just y) = Just (y:xs)

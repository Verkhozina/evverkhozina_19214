 append :: [a] -> a -> [a]
 append [] y = [y]
 append (x:xs) y = x:append xs y

 map_foldl :: (a -> b) -> [a] -> [b]
 map_foldl f [] = []
 map_foldl f xs = foldl (\xs x -> append xs (f x)) [] xs

 map_foldr :: (a -> b) -> [a] -> [b]
 map_foldr f [] = []
 map_foldr f xs = foldr (\x xs -> (f x):xs) [] xs

 map_foldl :: (a -> b) -> [a] -> [b]
 map_foldl f [] = []
 map_foldl f xs = reverse (foldl (\xs x -> (f x):xs) [] xs)

 map_foldr :: (a -> b) -> [a] -> [b]
 map_foldr f [] = []
 map_foldr f xs = foldr (\x xs -> (f x):xs) [] xs 

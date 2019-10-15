 head' :: [a] -> a
 head' [] = error "The list is empty."
 head' (x:_) = x

 last' :: [a] -> a
 last' [] = error "The list is empty."
 last' [x] = x
 last' (x:xs) = last xs
 
 tail' :: [a] -> [a]
 tail' [] = error "The list is empty."
 tail' (_:xs) = xs
 
 init' :: [a] -> [a]
 init' [] = error "The list is empty."
 init' [x] = []
 init' (x:xs) = x:(init xs)
 
 reverse'' :: [a] -> [a] -> [a]
 reverse'' [] ys = ys
 reverse'' (x:xs) ys = reverse'' xs (x:ys)

 reverse' :: [a] -> [a]  
 reverse' [] = []
 reverse' xs = reverse'' xs []
 
 length' :: [a] -> Int
 length' [] = 0
 length' (x:xs) = 1 + length xs
 
 null' :: [a] -> Bool
 null' [] = True
 null' (_:_) = False

 map' :: (a -> b) -> [a] -> [b]
 map' f [] = []
 map' f (x:xs) = f x : map f xs

 append' :: [a] -> a -> [a]
 append' [] x = [x]
 append' xs x = reverse' (x:(reverse' xs))

 concat' :: [a] -> [a] -> [a]
 concat' xs [] = xs
 concat' xs (y:ys) = concat' (append' xs y) ys

 drop' :: (Eq a, Num a) => a -> [b] -> [b]
 drop' 0 xs = xs
 drop' _ [] = []
 drop' n (x:xs) = drop' (n-1) xs

 take'' :: (Eq a, Num a) => a -> [b] -> [b] -> [b]
 take'' 0 xs ys = ys
 take'' _ [] ys = ys
 take'' n (x:xs) ys = take'' (n-1) xs (x:ys)

 take' :: (Eq a, Num a) => a -> [b] -> [b]
 take' 0 xs = []
 take' n xs = reverse' (take'' n xs [])
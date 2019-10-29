 --Тут не хватает filter test и zip,
 --потому что остались вопросы по их реализации.
 
 get' :: [b] -> Integer -> b
 get' (x:xs) 0 = x
 get' (x:xs) n = get' xs (n-1)
 
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

 reverse' :: [a] -> [a]
 reverse' xs = reverse'' [] xs
     where 
         reverse'' ys [] = ys
         reverse'' ys (x:xs) = reverse'' (x:ys) xs
 
 length' :: [a] -> Integer
 length' xs = length'' xs 0
     where
         length'' [] y = y
         length'' (x:xs) y = length'' xs (y+1) 

 append' :: [a] -> a -> [a]
 append' [] y = [y]
 append' (x:xs) y = x:append' xs y

 concat' :: [a] -> [a] -> [a]
 concat' [] ys = ys
 concat' (x:xs) ys = x:concat' xs ys

 drop' :: Integer -> [b] -> [b]
 drop' 0 xs = xs
 drop' _ [] = []
 drop' n (x:xs) = drop' (n-1) xs

 take' :: Integer -> [b] -> [b]
 take' 0 xs = []
 take' n (x:xs) = x:(take' (n-1) xs)
 
 splitAt' :: Integer -> [b] -> ([b], [b])
 splitAt' n xs = splitAt'' n [] xs
     where
         splitAt'' 0 ys xs = (ys, xs)
         splitAt'' n ys (x:xs) = splitAt'' (n-1) (append' ys x) xs
 
 null' :: [a] -> Bool
 null' [] = True
 null' (_:_) = False

 elem' :: Eq a => [a] -> a -> Bool
 elem' xs y = elem'' 0 xs y
     where
         elem'' 0 [] y = False
         elem'' 1 _ y = True
         elem'' n (x:xs) y = if x == y then elem'' 1 xs y 
                             else elem'' 0 xs y  

 --filter test'

 map' :: (a -> b) -> [a] -> [b]
 map' f [] = []
 map' f (x:xs) = f x : map f xs

 --zip'

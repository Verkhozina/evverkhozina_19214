 -- Пока поддерживает только цифры.
 -- Будет доработано. 
 
 step :: Int -> Int -> Int
 step a 0 = 1
 step a b = a * (step a (b-1))
 
 toDecimal :: Int -> [Char] -> [Char]
 toDecimal fb s = show (toDec fb 0 (read s :: Int))
     where
         toDec fb n 0 = 0
         toDec fb n s  = ((step fb n) * (s `mod` 10)) + (toDec fb (n+1) (s `div` 10))
 
 fromDecimal :: Int -> [Char] -> [Char] 
 fromDecimal tb s = show (fromDec tb [] (read s :: Int))
     where
         fromDec tb ys 0 = ys
         fromDec tb ys s = fromDec tb ((s `mod` tb):ys) (s `div` tb)
 
 convertFromTo :: Int -> Int -> [Char] -> [Char]
 convertFromTo fb tb s = fromDecimal tb (toDecimal fb s)

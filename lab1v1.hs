 step :: Int -> Int -> Int
 step a 0 = 1
 step a b = a * (step a (b-1))
 
 toDecimal :: Int -> [Char] -> [Char]
 toDecimal fb s = show (toDec fb 0 (reverse s))
     where
         toDec fb n [] = 0
         toDec fb n (x:s) = ((step fb n) * (read [x] :: Int)) + (toDec fb (n+1) s)
 
 fromDecimal :: Int -> [Char] -> [Char] 
 fromDecimal tb s = show (fromDec tb [] (read s :: Int))
     where
         fromDec tb ys 0 = ys
         fromDec tb ys s = fromDec tb ((s `mod` tb):ys) (s `div` tb)
 
 convertFromTo :: Int -> Int -> [Char] -> [Char]
 convertFromTo fb tb s = fromDecimal tb (toDecimal fb s)
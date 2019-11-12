   
 import Data.Char

 -- Возведение в степень:
 
 step :: Int -> Int -> Int
 step a 0 = 1
 step a b = a * (step a (b-1))

 -- Функции для системы с основанием 1:
 
 toTuring :: [Char] -> Int -> [Char]
 toTuring ys 0 = ('1':ys)
 toTuring ys s = toTuring ('1':ys) (s-1)

 fromTuring :: [Char] -> Int -> Int
 fromTuring [] n = n
 fromTuring (x:s) n = if x=='1' then fromTuring s (n+1)
                      else error "Invalid input"

 -- Перевод из букв и обратно:
 
 charToInt :: Char -> Int
 charToInt a = if (ord a) > 47 && (ord a) < 58  then (ord a) - 48
          else if (ord a) > 96 && (ord a) < 123 then (ord a) - 87
          else if (ord a) > 64 && (ord a) < 91  then (ord a) - 29
          else error "Invalid input"

 intToChar :: Int -> Char
 intToChar a = if a > -1 && a < 10 then chr (a + 48)
          else if a > 9  && a < 36 then chr (a + 87)
          else if a > 35 && a < 62 then chr (a + 29)
          else error "Invalid input"
 
 -- Собственно фунции: 

 toDecimal :: Int -> [Char] -> [Char]
 toDecimal 1  s = show (fromTuring s (-1))
 toDecimal fb s = if (fb < 62 && fb > 1) then show (toDec fb 0 (reverse s))
                  else error "Invalid input"
   where
     toDec fb n [] = 0
     toDec fb n (x:s) = if (charToInt x) >= fb then error "Invalid input"
                        else((step fb n) * (charToInt x)) + (toDec fb (n+1) s)
 
 fromDecimal :: Int -> [Char] -> [Char] 
 fromDecimal 1  s = toTuring [] (read s :: Int)
 fromDecimal tb s = if (tb < 62 && tb > 1) then fromDec tb [] (read s :: Int)
                    else error "Invalid input"
   where
     fromDec tb ys 0 = ys
     fromDec tb ys s = fromDec tb ((intToChar (s `mod` tb)):ys) (s `div` tb)
         
 convertFromTo :: Int -> Int -> [Char] -> [Char]
 convertFromTo fb tb s = fromDecimal tb (toDecimal fb s)

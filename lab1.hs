 --Остался вопрос по штрафу:
 --Достаточно ли будет добалять минус к числу,
 --чтобы оно считалось отрицательным?
 
 import Data.Char

 -- Функции для системы с основанием 1:
 
 toTuring :: [Char] -> Int -> [Char]
 toTuring ys s = (replicate s '1') ++ "1"

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
 toDecimal fb s = if (fb < 62 && fb > 1) then show ((foldl (\x y -> (x + (charToInt y)) * fb) 0 s) `div` fb)
                  else error "Invalid input"
 
 fromDecimal :: Int -> [Char] -> [Char] 
 fromDecimal 1  s = toTuring [] (read s :: Int)
 fromDecimal tb s = if (tb < 62 && tb > 1) then fromDec tb [] (read s :: Int)
                    else error "Invalid input"
   where
     fromDec tb ys 0 = ys
     fromDec tb ys s = fromDec tb ((intToChar (s `mod` tb)):ys) (s `div` tb)
         
 convertFromTo :: Int -> Int -> [Char] -> [Char]
 convertFromTo fb tb s = fromDecimal tb (toDecimal fb s)

 -- Остались вопросы касательно двух последних функций.

 data Complex a = Complex {
     re :: a,
     im :: a
 }

 instance Show a => Show (Complex a) where
     show (Complex re im) = (show re) ++ " + i" ++ (show im)

 instance Eq a => Eq (Complex a) where
     (Complex re1 im1) == (Complex re2 im2) = (re1 == re2) && (im1 == im2)

 -- Пусть сравниваются по действительной части:
 instance Ord a => Ord (Complex a) where
     compare (Complex re1 im1) (Complex re2 im2) = compare re1 re2

 instance Functor Complex where
     fmap f (Complex re im) = Complex (f re) (f im)

 data QState a = QState {
     complex :: Complex a,
     str :: String
 }

 instance Show a => Show (QState a) where
     show (QState comp str) = (show comp) ++ " | " ++ str

 instance Eq a => Eq (QState a) where
     (QState comp1 str1) == (QState comp2 str2) = (comp1 == comp2) && (str1 == str2)

 instance Ord a => Ord (QState a) where
     compare (QState comp1 str1) (QState comp2 str2) = compare comp1 comp2 

 instance Functor QState where
     fmap f (QState comp str) = QState (fmap f comp) str

 type Qubit a = [QState a]

 toList :: Qubit a -> [Complex a]
 toList [] = []
 toList ((QState comp str):qub) = comp:(toList qub)

 toLabelList :: Qubit a -> [String]
 toLabelList [] = []
 toLabelList ((QState comp str):qub) = str:(toLabelList qub)

 fromList :: [Complex a] -> [String] -> Qubit a
 fromList [] _ = []
 fromList _ [] = []
 fromList (c:comp) (s:str) = (QState c s):(fromList comp str)

 toPairList :: Qubit a -> [(Complex a,String)]
 toPairList [] = []
 toPairList ((QState comp str):qub) = (comp,str):(toPairList qub)

 fromPairList :: [(Complex a,String)] -> Qubit a
 fromPairList [] = []
 fromPairList ((comp,str):cs) = (QState comp str):(fromPairList cs)
 
 --scalarProduct :: Qubit a -> Qubit a -> a
 --scalarProduct [] = []
 --scalarProduct = 
 
 --entagle :: Qubit a -> Qubit a -> Qubit a
 --entagle [] = [] 
 --entagle ((QState comp str):qub) = (QState () ()):(entagle qub) 

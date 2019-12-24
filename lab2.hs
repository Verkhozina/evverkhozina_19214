
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
 
 instance Num a => Num (Complex a) where
     negate (Complex re im) = Complex re (negate im)
     (+) (Complex re1 im1) (Complex re2 im2) = Complex (re1+re2) (im1+im2)
     (*) (Complex re1 im1) (Complex re2 im2) = Complex (re1*re2) (im1*im2)
     fromInteger x = Complex (fromInteger x) 0
     abs           = fmap abs
     signum        = fmap signum

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
 toList qub = map (\(QState comp str) -> comp) qub

 toLabelList :: Qubit a -> [String]
 toLabelList qub = map (\(QState comp str) -> str) qub

 fromList :: [Complex a] -> [String] -> Qubit a
 fromList comp str = map (\(x,y) -> QState x y) (zip comp str)    

 toPairList :: Qubit a -> [(Complex a,String)]
 toPairList qub = map (\(QState comp str) -> (comp, str)) qub

 fromPairList :: [(Complex a,String)] -> Qubit a
 fromPairList xs = map (\(x,y) -> QState x y) xs 
 
 scalarProduct :: Num a => Qubit a -> Qubit a -> a
 scalarProduct q1 q2 = foldr (+) 0 (map (\((QState c1 s1), (QState c2 s2)) -> (prod c1 c2)) (zip q1 q2))
     where
         prod (Complex re1 im1) (Complex re2 im2) = (re1 * re2) + (im1 * im2)
 
 entagle :: Num a => Qubit a -> Qubit a -> Qubit a
 entagle qub1 qub2 = map (\((QState c1 s1), (QState c2 s2)) -> QState (prod c1 c2) (s1 ++ s2)) (zip qub1 qub2)
     where
         prod (Complex re1 im1) (Complex re2 im2) = Complex (re1 * re2) (im1 * im2)

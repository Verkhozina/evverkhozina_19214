import Data.Char
import Data.List

data HashTable k v = HashTable {
    elements :: [[(k,v)]],
    size :: Int,
    fullness :: Int
} deriving Show

step :: Int -> Int -> Int
step a 0 = 1
step a b = a * (step a (b-1))

defaultHashTable :: HashTable k v
defaultHashTable = HashTable [[],[],[],[],[],[],[],[],[],[]] 10 0

hashF :: Show k => k -> Int
hashF k = (hashF' (show k) 0) `mod` 100
    where
        hashF' [] _ = 0
        hashF' (x:xs) n = (ord x)*(step 31 n) + hashF' xs (n+1)

enlarge :: (Show k, Eq k) => HashTable k v -> Int -> HashTable k v
enlarge (HashTable kv s f) size = HashTable (kv++(replicate size [])) (s+size) f

insertHash :: (Show k, Eq k) => k -> v -> HashTable k v -> HashTable k v
insertHash key val (HashTable kv s f)| s == f || (hashF key) > (s - 1) = insertHash key val (enlarge (HashTable kv s f) ((hashF key) - s + 1))
                                     | otherwise = HashTable (left ++ [(key,val):(kv !! (hashF key))] ++ (tail right)) s (f+1)
    where
        (left, right) = splitAt (hashF key) kv

fromList :: (Show k, Eq k) => [(k,v)] -> HashTable k v  
fromList xs = foldr (\(k,v) xs -> insertHash k v xs) defaultHashTable xs

clear :: HashTable k v -> HashTable k v
clear _ = defaultHashTable

erase :: (Show k, Eq k, Eq v) => HashTable k v -> k -> HashTable k v
erase (HashTable kv s f) key | (kv !! (hashF key)) == [] = error "Empty element"
                             | otherwise = HashTable (left ++ [[]] ++ (tail right)) s (f-1)
    where
        (left, right) = splitAt (hashF key) kv

contains :: (Show k, Eq k, Eq v) => HashTable k v -> k -> Bool
contains (HashTable [] s f) key = False
contains (HashTable kv s f) key = if (kv !! (hashF key)) == [] then False else True

at :: (Show k, Eq k, Eq v) => HashTable k v -> k -> [(k,v)]
at (HashTable kv s f) key = if (contains (HashTable kv s f) key) then (kv !! (hashF key))
                            else error "No value"

sizeHash :: (Show k, Eq k) => HashTable k v -> Int
sizeHash (HashTable kv s f) = f

empty :: (Show k, Eq k, Eq v) => HashTable k v -> Bool
empty (HashTable _ _ f) = if f == 0 then True else False

--let x = fromList [(12,"qwe"), (5,"hey"), (666,"say10"), (0,"oh")]
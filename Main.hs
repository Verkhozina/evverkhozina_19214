import System.Random
import System.IO.Unsafe
import System.IO                                       

data State = State {
 fl :: Int,
 hp :: Int,
 b :: Int,
 m :: Int,
 items :: [Int]
} deriving Show;

changeElem :: [Int] -> Int -> Int -> [Int]
changeElem xs ind elem = (take ind xs) ++ [elem] ++ (drop (ind + 1) xs)

startState :: State
startState = State 1 3 0 0 [0,0,0] :: State

floors :: [Int]
floors = [3,1,0,2,0,0,0,0,0,0,2,4,1,0,0,1,0,0,2,5,0]

numToLoc :: Int -> String
numToLoc loc | loc == 1 = "first classroom"
             | loc == 2 = "second classroom"
             | loc == 3 = "third classroom"
             | loc == 4 = "first club"
             | loc == 5 = "second club"
             | loc == 6 = "storage room"
             | loc == 7 = "restroom"

checkWin :: State -> Bool
checkWin (State fl hp b m (x:xs)) | xs == [] && x /= 0 = True
                                  | x == 0 = False
                                  | otherwise = checkWin (State fl hp b m xs)
win :: IO ()    
win = do
    putStrLn "         _____\n       /       \\\n      |  X   X  |\n      |         |\n __ \\ |\\  ,,,  /| / __\n __\\| | | ''' | | |/__\n    \\_|  \\   /  |_/\n"
    putStrLn "Yay! You've done the exile ritual and the monster has gone forever!\n"
    putStrLn "Press 1 to play again"
    putStrLn "Press any other key to quit\n"
    y <- getLine
    let x = read y :: Int
    if (x == 1) then curFloor startState floors
    else return ()
    return ()

lose :: IO ()
lose = do
    putStrLn "Unfortunately, the wounds were fatal.\n\nW A S T E D\n"
    putStrLn "Press 1 to try again"
    putStrLn "Press any other key to quit\n"
    y <- getLine
    let x = read y :: Int
    if (x == 1) then curFloor startState floors
    else return ()
    return ()

useMedKit :: State -> [Int] -> IO ()
useMedKit (State fl hp b m i) floors = do
    if (m == 0) then do
        putStrLn "You have no medicines!\n"
        curFloor (State fl hp b m i) floors
    else if (hp /= 3) then do
             putStrLn "You've just used a medkit.\n"
             curFloor (State fl (hp + 1) b (m - 1) i) floors
         else do
             putStrLn "You've just wasted a medkit.\n"
             curFloor (State fl hp b (m - 1) i) floors
    return ()

meetMonster :: State -> [Int] -> Int -> IO ()
meetMonster (State fl hp b m i) floors loc = do    
    putStrLn "What are you going to do?\n"
    putStrLn "1: use flashlight"
    putStrLn "2: try to escape\n"
    y <- getLine
    putStrLn ""
    let x = read y :: Int
    if (x == 1) then do
        if (b > 0) then do
            putStrLn "Hooray! It's gone!\n"
            inspectTheLoc (State fl hp (b - 1) m i) floors loc
            return ()
        else do
            putStrLn "Your flashlight is out of batteries!\n"
            meetMonster (State fl hp b m i) floors loc
            return ()
    else if (x == 2) then do
        luck <- randomRIO (1, 3 :: Int)
        if (luck == 1) then do
            putStrLn "Lucky you! You escaped unharmed.\n"
            curFloor (State fl hp b m i) floors
            return ()
        else do
            putStrLn "Oh, no! The monster managed to hurt you!\n"
            if (hp == 1) then do lose
            else curFloor (State fl (hp - 1) b m i) floors
            return ()
    else if (x == 0) then return ()
    else do
        putStrLn "Unknown command. Try again.\n"
        meetMonster (State fl hp b m i) floors loc
        return ()

inspectTheLoc :: State -> [Int] -> Int -> IO ()
inspectTheLoc (State fl hp b m i) floors loc = do
    let locNum = (fl - 1) * 7 + loc - 1 :: Int
    let locSt = floors !! locNum :: Int
    if (locSt == 1) then do
        putStrLn "You've found a battery!\n"
        curFloor (State fl hp (b+2) m i) (changeElem floors locNum 0)
    else if (locSt == 2) then do
        putStrLn "You've found a medkit!\n"
        curFloor (State fl hp b (m+1) i) (changeElem floors locNum 0)
    else if (locSt == 3) then do
        putStrLn "     __________\n    / Occult  /\\\n   / science / /\n  /   book  / /\n /_________/ /\n \\/ /______\\/\n /_/\n"
        putStrLn "You've found a spell book!\n"
        curFloor (State fl hp b m (changeElem i 0 1)) (changeElem floors locNum 0)
    else if (locSt == 4) then do
        putStrLn "You've found a bucket of a red paint!\n"
        putStrLn "   _________\n  /\\ \\ \\ \\ \\\\\n |\\_\\_\\_\\_\\_/|\n |           |\n | Red Paint |\n \\           /\n  \\_________/\n"
        curFloor (State fl hp b m (changeElem i 1 1)) (changeElem floors locNum 0)
    else if (locSt == 5) then do
        putStrLn "You've found a rose!\n"
        putStrLn "    _ _ _ _ _ _\n   /_/ _ _ _ \\_\\\n  / / / /^\\ \\ \\ \\\n  \\_\\ \\_\\v/_/ /_/\n   \\_\\_ _ _ _/_/\n  __    //\n  \\ \\  //__\n   \\_\\/// /\n     ///_/\n    //\n   ''\n"
        curFloor (State fl hp b m (changeElem i 2 1)) (changeElem floors locNum 0)
    else do
        putStrLn "There's nothing here.\n"
        curFloor (State fl hp b m i) floors
    return ()    

curFloor :: State -> [Int] -> IO ()
curFloor (State fl hp b m i) floors = do
    putStrLn "------------------------------------------------------------------\n"
    putStrLn ("You're at the " ++ (show fl) ++ " floor. Where to go?\n")
    putStrLn "1: first classroom"
    putStrLn "2: second classroom"
    putStrLn "3: third classroom"
    putStrLn "4: first club"
    putStrLn "5: second club"
    putStrLn "6: storage room"
    putStrLn "7: restroom"
    putStrLn "8: downstairs"
    putStrLn "9: upstairs\n"
    y <- getLine
    putStrLn ""
    let x = read y :: Int
    if (elem x [1, 2, 3, 4, 5, 6, 7]) then do
             putStrLn "------------------------------------------------------------------\n"
             putStrLn ("You're at the " ++ (numToLoc x) ++ " on the " ++ (show fl) ++ " floor.\n")
             monster <- randomRIO (1, 3 :: Int)
             if (monster == 1) then do
                 putStrLn "         _____\n       /       \\\n      | (O) (O) |\n      |   ,,,   |\n __ \\ |\\  | |  /| / __\n __\\| | | | | | | |/__\n    \\_|  \\'''/  |_/\n"
                 putStrLn "Oh, no! Here's a monster!\n" 
                 meetMonster (State fl hp b m i) floors x
                 return ()
             else do
                 inspectTheLoc (State fl hp b m i) floors x
                 return ()
    else if (x == 8) then do
             if (fl == 2 || fl == 3) then do
                 curFloor (State (fl-1) hp b m i) floors
                 return ()
             else do
                 putStrLn "You're at the minimum floor.\n"
                 curFloor (State fl hp b m i) floors
                 return ()
    else if (x == 9) then do
             if (fl == 1 || fl == 2) then do
                 curFloor (State (fl+1) hp b m i) floors
                 return ()
             else do
                 putStrLn "You're at the maximum floor.\n"
                 curFloor (State fl hp b m i) floors
                 return ()
    else if (x == 10) then do
             putStrLn ((show (State fl hp b m i)) ++ "\n")
             curFloor (State fl hp b m i) floors
             return ()
    else if (x == 11) then do 
             if (checkWin (State fl hp b m i)) then do
                 win
                 return ()
             else do
                 putStrLn "Not enough items.\n"
                 curFloor (State fl hp b m i) floors
                 return ()
    else if (x == 101) then do useMedKit (State fl hp b m i) floors
    else if (x == 1010) then return ()
    else do
        putStrLn "Unknown command. Try again.\n"
        curFloor (State fl hp b m i) floors
        return ()

main :: IO ()  
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "------------------------------------------------------------------\n"
    putStrLn "Welcome to the Nightmare game!\n"
    putStrLn "At every step of the game you'll need to type a propert number to move further."
    putStrLn "Your goal is to cast out a monster from your abandoned school."
    putStrLn "To do that you'll need to find three items throughout the school.\n"
    putStrLn "Type 10 to see your current state."
    putStrLn "Type 11 to perform an exile ritual."
    putStrLn "Type 101 to use a medkit.\n"
    putStrLn "Type 1010 to quit the game immediately.\n"
    putStrLn "Have fun!\n"
    curFloor startState floors
    return ()
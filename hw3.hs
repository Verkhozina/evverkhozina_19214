--Сначала я хотела реализовать всё на списках, но в процессе вспомнила, что в хаскеле нет переменных.
--Таким образом я пока что не понимаю, как реализовать работу с хранилищем музыки и пользователей,
--если фактически не могу менять ни у чего значений.


 data Track = Track {
     name :: [Char],
     album :: [Char],
     group :: [Char]
 } deriving (Show, Eq)

 data User = User {
     login :: [Char],
     password :: [Int],
     fav :: [Track]
 } deriving (Show, Eq)

 existingMusic :: [Track]
 existingMusic = [Track "The Greatest Show" "Panic At The Disco" "Original Motion Picture Soundtrack", Track "The Nobodies" "Marilyn Manson" "Holy Wood", Track "Feeling Good" "Muse" "Origin of Symmetry"]
 
 searchByName :: String -> [Track]
 searchByName x = search x existingMusic []
     where
         search x [] [] = error "Nothing found"
         search x [] ys = ys
         search x (Track n g a:xs) ys = if x == n then search x xs (Track n g a:ys) else search x xs ys
 
 searchByGroup :: String -> [Track]
 searchByGroup x = search x existingMusic []
     where
         search x [] [] = error "Nothing found"
         search x [] ys = ys
         search x (Track n g a:xs) ys = if x == g then search x xs (Track n g a:ys) else search x xs ys

 searchByAlbum :: String -> [Track]
 searchByAlbum x = search x existingMusic []
     where
         search x [] [] = error "Nothing found"
         search x [] ys = ys
         search x (Track n g a:xs) ys = if x == a then search x xs (Track n g a:ys) else search x xs ys
 
 addToFav :: Track -> User -> [Track]
 addToFav t (User name age fav) = (t:fav)

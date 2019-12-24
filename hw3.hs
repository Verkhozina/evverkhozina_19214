-- Можно было бы ещё поупражняться с поиском,
-- но по большей части он был бы однотипен

 type Name = String
 type Track = Name

 data Playlist = Playlist {
     playlistName :: Name,
     tracks :: [Track]
 } deriving (Show, Eq)

 type Album = Playlist

 data Artist = Artist {
     artistName :: Name,
     albums :: [Album]
 } deriving (Show, Eq)

 data User = User {
     login :: String,
     password :: [Int],
     fav :: Playlist
 } deriving (Show, Eq)

 search :: Track -> [Track] -> [Track]
 search t ts = filter (\x -> if x == t then True else False) ts

 playlistSearch :: Track -> [Playlist] -> [(Name, Track)]
 playlistSearch t [] = []
 playlistSearch t ((Playlist n ts):ps) = if (elem t ts)
                                         then ((n, t)):(playlistSearch t ps)
                                         else playlistSearch t ps

 globalSearch :: Track -> [Artist] -> [(Name, Track)]
 globalSearch t [] = []
 globalSearch t ((Artist n ps):as) = if (playlistSearch t ps /= [])
                                        then ((n, t)):(globalSearch t as)
                                        else globalSearch t as

 addToFav :: Track -> User -> Playlist
 addToFav t (User name age (Playlist n ts)) = Playlist n (t:ts)

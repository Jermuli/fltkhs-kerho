module Tietorakenteet where

--TODO: lisää tietorakenteita muokkaavat ja käsittelevät funktiot sekä relaatiot, sekä selvitä onko relaatiot edes tarpeellisia
--tässä tapauksessa
data Jasen = Jasen { nimi :: String
                   , hetu :: String
                   , katuosoite :: String
                   , postinumero :: Int
                   , kotipuhelin :: Int
                   , tyopuhelin :: Int
                   , autopuhelin :: Int
                   , liittymisvuosi :: Int
                   , jasenmaksu :: Double
                   , maksettu :: Double
                   , lisatieto :: String
                   , harrastukset :: [Harrastus]} deriving (Show)

data Harrastus = Harrastus { laji :: String
                           , aloitusvuosi :: Int
                           , tuntiaViikossa :: Double} deriving (Show)
                            
kerho = [] :: [Jasen]


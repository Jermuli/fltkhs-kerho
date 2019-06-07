module Tietorakenteet where

--TODO: lisää tietorakenteita muokkaavat ja käsittelevät funktiot sekä relaatiot
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
                   , lisatieto :: String} deriving (Show)

data Harrastus = Harrastus { laji :: String
                           , aloitusvuosi :: Int
                           , tuntiaViikossa :: Double} deriving (Show)
                            
type Kerho = [Jasen]
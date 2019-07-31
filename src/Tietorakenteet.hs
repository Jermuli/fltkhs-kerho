module Tietorakenteet where

--TODO: lisää tietorakenteita muokkaavat ja käsittelevät funktiot sekä relaatiot, sekä selvitä onko relaatiot edes tarpeellisia
--tässä tapauksessa
data Jasen = Jasen { nimi :: Maybe String
                   , hetu :: Maybe String
                   , katuosoite :: Maybe String
                   , postinumero :: Maybe Int
                   , kotipuhelin :: Maybe Int
                   , tyopuhelin :: Maybe Int
                   , autopuhelin :: Maybe Int
                   , liittymisvuosi :: Maybe Int
                   , jasenmaksu :: Maybe Double
                   , maksettu :: Maybe Double
                   , lisatieto :: Maybe String
                   , harrastukset :: [Harrastus]} deriving (Show)

data Harrastus = Harrastus { laji :: String
                           , aloitusvuosi :: Int
                           , tuntiaViikossa :: Double} deriving (Show)
                            
data Kerho = Kerho { kerhonNimi :: Maybe String
                   , jasenet :: [Jasen]}

valittuKerho :: Kerho
valittuKerho = Kerho Nothing []

valittuJasen :: Jasen
valittuJasen = Jasen Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing []

--lisaaJasen :: Jasen -> Kerho
--lisaaJasen uusijasen = valittuKerho nimi xs ++ uusijasen
--
--poistaJasen :: Jasen -> Kerho
--
--muokkaaJasen :: Jasen -> Kerho
--
--lisaaHarrastus :: Harrastus -> Jasen
--
--poistaHarrastus :: Harrastus -> Jasen
--
--muokkaaHarrastus :: Harrastus -> Jasen

--lataaKerho :: IO String -> Kerho
--lataaKerho tiedostoNimi = case tiedostoNimi of
--                            []  -> Kerho ([], [])
--                            _   -> Kerho (tiedostoNimi, lueTiedosto)
--    where 
--    lueTiedosto = --TODO lisää tiedoston lukeminen

--tallennaKerho :: Kerho -> IO
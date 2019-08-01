module Tietorakenteet where
import qualified Data.Text as T

--TODO: lisää tietorakenteita muokkaavat ja käsittelevät funktiot sekä relaatiot, sekä selvitä onko relaatiot edes tarpeellisia
--tässä tapauksessa
data Jasen = Jasen { nimi :: Maybe T.Text
                   , hetu :: Maybe T.Text
                   , katuosoite :: Maybe T.Text
                   , postinumero :: Maybe Int
                   , postiosoite :: Maybe T.Text
                   , kotipuhelin :: Maybe Int
                   , tyopuhelin :: Maybe Int
                   , autopuhelin :: Maybe Int
                   , liittymisvuosi :: Maybe Int
                   , jasenmaksu :: Maybe Double
                   , maksettu :: Maybe Double
                   , lisatieto :: Maybe T.Text
                   , harrastukset :: [Harrastus]} deriving (Show)

data Harrastus = Harrastus { laji :: Maybe T.Text
                           , aloitusvuosi :: Maybe Int
                           , tuntiaViikossa :: Maybe Double} deriving (Show)
                            
data Kerho = Kerho { kerhonNimi :: Maybe T.Text
                   , jasenet :: [Jasen]}

--valittuKerho :: IO T.Text -> IO Kerho
--valittuKerho <- lataaKerho tiedostoNimi  -- = Kerho Nothing []
--return()

--valittuJasen :: Jasen
--valittuJasen = Jasen Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing []

lisaajasen :: Jasen -> Kerho -> Kerho
lisaajasen uusijasen kerho = Kerho (kerhonNimi kerho) ((jasenet kerho) ++ [uusijasen])
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

--lataaKerho :: IO T.Text -> IO Kerho
--lataaKerho tiedostoNimi = case tiedostoNimi of
--                            []  -> Kerho ([], [])
--                            _   -> Kerho (tiedostoNimi, lueTiedosto)
--    where 
--    lueTiedosto = --TODO lisää tiedoston lukeminen

--tallennaKerho :: Kerho -> IO
module Tietorakenteet where
import Data.List
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
                   , harrastukset :: [Harrastus]} deriving (Eq, Show)

data Harrastus = Harrastus { laji :: Maybe T.Text
                           , aloitusvuosi :: Maybe Int
                           , tuntiaViikossa :: Maybe Double} deriving (Eq, Show)
                            
data Kerho = Kerho { kerhonNimi :: Maybe T.Text
                   , jasenet :: [Jasen]}

--valittuKerho :: IO T.Text -> IO Kerho
--valittuKerho <- lataaKerho tiedostoNimi  -- = Kerho Nothing []
--return()

--valittuJasen :: Jasen
--valittuJasen = Jasen Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing []

lisaajasen :: Jasen -> Kerho -> Kerho
lisaajasen uusijasen kerho = Kerho (kerhonNimi kerho) ((jasenet kerho) ++ [uusijasen])

poistaJasen :: Jasen -> [Jasen] -> [Jasen]
poistaJasen poistettava jasenet = case jasenet of
                                    []  -> []
                                    x:xs   -> case x == poistettava of
                                                True    -> xs
                                                False   -> x:(poistaJasen poistettava xs)

muokkaaJasen :: Jasen -> Jasen -> [Jasen] -> [Jasen]
muokkaaJasen muokattava uudettiedot jasenet = case jasenet of
                                                []  ->  []
                                                x:xs -> case x == muokattava of
                                                            True    -> uudettiedot:xs
                                                            False   -> x:(muokkaaJasen muokattava uudettiedot xs)
lisaaHarrastus :: Harrastus -> Jasen -> Jasen
lisaaHarrastus harrastus jasen = Jasen (nimi jasen) (hetu jasen) (katuosoite jasen) (postinumero jasen) (postiosoite jasen) (kotipuhelin jasen) (tyopuhelin jasen) (autopuhelin jasen) (liittymisvuosi jasen) (jasenmaksu jasen) (maksettu jasen) (lisatieto jasen) ((harrastukset jasen) ++ [harrastus])

poistaHarrastus :: Harrastus -> Jasen -> Jasen
poistaHarrastus poistettava jasen = Jasen (nimi jasen) (hetu jasen) (katuosoite jasen) (postinumero jasen) (postiosoite jasen) (kotipuhelin jasen) (tyopuhelin jasen) (autopuhelin jasen) (liittymisvuosi jasen) (jasenmaksu jasen) (maksettu jasen) (lisatieto jasen) (delete poistettava (harrastukset jasen))

--muokkaaHarrastus :: Harrastus -> Jasen

--lataaKerho :: IO T.Text -> IO Kerho
--lataaKerho tiedostoNimi = case tiedostoNimi of
--                            []  -> Kerho ([], [])
--                            _   -> Kerho (tiedostoNimi, lueTiedosto)
--    where 
--    lueTiedosto = --TODO lisää tiedoston lukeminen

--tallennaKerho :: Kerho -> IO
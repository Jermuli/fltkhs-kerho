module Tietorakenteet where
import Data.List
import Data.Char
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
instance Ord Jasen where
    compare (Jasen nimi1 _ _ _ _ _ _ _ _ _ _ _ _) (Jasen nimi2 _ _ _ _ _ _ _ _ _ _ _ _) | nimi1 > nimi2 = GT
                                                                                        | nimi1 < nimi2 = LT
                                                                                        | nimi1 == nimi2 = EQ

validoiHetu :: T.Text -> Bool
validoiHetu he = let unpacked = T.unpack he
                   in case length unpacked of
                        11  -> case (tarkistaTotuudet (map isNumber (take 6 unpacked))) && ((unpacked !! 6) == '-') of
                                True    -> case ((read (take 2 unpacked)) <= 31) && ((read (take 2 (drop 2 unpacked))) <= 12) of
                                                True    -> True
                                                False   -> False
                                False   -> False
                        _   -> False

tarkistaTotuudet :: [Bool] -> Bool
tarkistaTotuudet [] = True
tarkistaTotuudet (x:xs) = case x == True of
                            True    -> tarkistaTotuudet xs
                            False   -> False
--valittuKerho :: IO T.Text -> IO Kerho
--valittuKerho <- lataaKerho tiedostoNimi  -- = Kerho Nothing []
--return()

--valittuJasen :: Jasen
--valittuJasen = Jasen Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing []
--
lisaaJasen :: Jasen -> Kerho -> Kerho
lisaaJasen uusijasen kerho = Kerho (kerhonNimi kerho) ((jasenet kerho) ++ [uusijasen])

poistaJasen :: Int -> Kerho -> Kerho
poistaJasen paikka kerho = Kerho (kerhonNimi kerho) (poistaJasenApu paikka (jasenet kerho))

poistaJasenApu :: Int -> [Jasen] -> [Jasen]
poistaJasenApu paikka []        = []
poistaJasenApu paikka (x:xs)    = case paikka of
                                            0   -> xs
                                            _   -> (x:(poistaJasenApu (paikka - 1) xs))

sortKerho :: Kerho -> Kerho
sortKerho kerho = Kerho (kerhonNimi kerho) (sort (jasenet kerho))

muokkaaJasen :: Jasen -> Int -> Kerho -> Kerho
muokkaaJasen uudettiedot paikka kerho = Kerho (kerhonNimi kerho) (muokkaaJasenApu uudettiedot paikka (jasenet kerho))

muokkaaJasenApu :: Jasen -> Int -> [Jasen] -> [Jasen]
muokkaaJasenApu uudettiedot paikka []       = []
muokkaaJasenApu uudettiedot paikka (x:xs)   = case paikka of
                                                0   -> (uudettiedot:xs)
                                                _   -> (x:(muokkaaJasenApu uudettiedot (paikka - 1) xs))
--testaa
jasenTekstiksi :: Show a => Jasen -> (Jasen -> Maybe a) -> T.Text
jasenTekstiksi jasen f = case f jasen of
                            Nothing ->  T.pack " "
                            Just x  ->  T.pack (show x) --case x of
                                        --  -> x
                                        --_           -> T.pack (show x)

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
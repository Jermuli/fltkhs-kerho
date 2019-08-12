module Tietorakenteet where
import Data.List
import Data.Char
import qualified Data.Text as T

-- Tekijä: Jere Pakkanen
-- Pvm: 12.8.2019

-- Jäsenen tietorakenne
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

-- Harrastuksen tietorakenne
data Harrastus = Harrastus { laji :: Maybe T.Text
                           , aloitusvuosi :: Maybe Int
                           , tuntiaViikossa :: Maybe Double} deriving (Eq, Show)
-- Kerhon tietorakenne                            
data Kerho = Kerho { kerhonNimi :: Maybe T.Text
                   , jasenet :: [Jasen]}

-- Jäsenten järjestäminen nimen mukaan (jäsenlistaa varten)
instance Ord Jasen where
    compare (Jasen nimi1 _ _ _ _ _ _ _ _ _ _ _ _) (Jasen nimi2 _ _ _ _ _ _ _ _ _ _ _ _) | nimi1 > nimi2 = GT
                                                                                        | nimi1 < nimi2 = LT
                                                                                        | nimi1 == nimi2 = EQ
-- Henkilötunnuksen validoiva funktio
validoiHetu :: T.Text -> Bool
validoiHetu he = let unpacked = T.unpack he
                   in case length unpacked of
                        11  -> case (tarkistaTotuudet (map isNumber (take 6 unpacked))) && ((unpacked !! 6) == '-') of
                                True    -> case ((read (take 2 unpacked)) <= 31) && ((read (take 2 (drop 2 unpacked))) <= 12) of
                                                True    -> True
                                                False   -> False
                                False   -> False
                        _   -> False

-- Tarkistaa postinumeron
validoiPostinumero :: Maybe Int -> Bool
validoiPostinumero numero = case numero of
                                Just x -> ((length $ show x) == 5)
                                _      -> False

-- Tarkistaa nimen
validoiNimi :: T.Text -> Bool
validoiNimi nimi = not ((T.strip nimi) == (T.pack ""))

-- Tarkistaa puhelinnumeron
validoiPuhelin :: Maybe Int -> Bool
validoiPuhelin numero = case numero of
                            Just x  -> ((6 <= length (show x)) && (10 >= length (show x)))
                            _       -> False

-- Tarkistaa maksetun maksun
validoiMaksu :: Maybe Double -> Maybe Double -> Bool
validoiMaksu maksettu maksu = case (maksettu, maksu) of
                                    (Just x, Just y)    -> (maksettu <= maksu)
                                    _                   -> False
                        
-- Looginen AND listoille
tarkistaTotuudet :: [Bool] -> Bool
tarkistaTotuudet [] = True
tarkistaTotuudet (x:xs) = case x == True of
                            True    -> tarkistaTotuudet xs
                            False   -> False

-- Vaihtaa kerhon nimen                            
vaihdaKerhonNimea ::  T.Text -> Kerho -> Kerho
vaihdaKerhonNimea uusiNimi klubi = Kerho (Just uusiNimi) (jasenet klubi)

-- Lisää jäsenen kehoon
lisaaJasen :: Jasen -> Kerho -> Kerho
lisaaJasen uusijasen kerho = Kerho (kerhonNimi kerho) ((jasenet kerho) ++ [uusijasen])

-- Poistaa tietyssä paikassa olevan jäsenen kerhosta (ehkä tämän voisi korvata vaan poistamalla tietyn jäsenen kerhosta)
poistaJasen :: Int -> Kerho -> Kerho
poistaJasen paikka kerho = Kerho (kerhonNimi kerho) (poistaJasenApu paikka (jasenet kerho))

-- Rekursiivinen apufunktio jäsenen poistoon
poistaJasenApu :: Int -> [Jasen] -> [Jasen]
poistaJasenApu paikka []        = []
poistaJasenApu paikka (x:xs)    = case paikka of
                                            0   -> xs
                                            _   -> (x:(poistaJasenApu (paikka - 1) xs))

-- Laittaa kerhon jäsenet järjestykseen nimen mukaan
sortKerho :: Kerho -> Kerho
sortKerho kerho = Kerho (kerhonNimi kerho) (sort (jasenet kerho))

-- Muokkaa tietyssä tietyssä paikassa olevan jäsenen tietoja
muokkaaJasen :: Jasen -> Int -> Kerho -> Kerho
muokkaaJasen uudettiedot paikka kerho = Kerho (kerhonNimi kerho) (muokkaaJasenApu uudettiedot paikka (jasenet kerho))

-- Rekursiivinen apufunktio jäsenen muokkaukselle
muokkaaJasenApu :: Jasen -> Int -> [Jasen] -> [Jasen]
muokkaaJasenApu uudettiedot paikka []       = []
muokkaaJasenApu uudettiedot paikka (x:xs)   = case paikka of
                                                0   -> (uudettiedot:xs)
                                                _   -> (x:(muokkaaJasenApu uudettiedot (paikka - 1) xs))
-- Lisätään harrastus jäsenelle
lisaaHarrastus :: Harrastus -> Jasen -> Jasen
lisaaHarrastus harrastus jasen = Jasen (nimi jasen) (hetu jasen) (katuosoite jasen) (postinumero jasen) (postiosoite jasen) (kotipuhelin jasen) (tyopuhelin jasen) (autopuhelin jasen) (liittymisvuosi jasen) (jasenmaksu jasen) (maksettu jasen) (lisatieto jasen) ((harrastukset jasen) ++ [harrastus])

-- Poistetaan harrastus jäseneltä
poistaHarrastus :: Harrastus -> Jasen -> Jasen
poistaHarrastus poistettava jasen = Jasen (nimi jasen) (hetu jasen) (katuosoite jasen) (postinumero jasen) (postiosoite jasen) (kotipuhelin jasen) (tyopuhelin jasen) (autopuhelin jasen) (liittymisvuosi jasen) (jasenmaksu jasen) (maksettu jasen) (lisatieto jasen) (delete poistettava (harrastukset jasen))
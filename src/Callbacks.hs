{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}

module Callbacks where
import Tietorakenteet
import Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Data.IORef
import Text.Read
import Control.Monad
import System.IO.Unsafe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List hiding (sort, insert)
import Data.Function

-- TODO: Tärkeät: Korjaa harrastusten poistosta tapahtuva kaatuminen lisäämällä tarkistuksia, lisää tarkistus ladattavan tiedoston nimeen, laita kentät päivittymään paremmin
-- Pienempi prioriteetti: tee paremmat tarkistukset jäsenen infonsyöttöön, selvitä miksi redraw ei toimi joissain tapauksissa, lisää harrastusten muokkaus

-- Toistaiseksi käyttämätön 
nakyvatJasenet :: IORef [Bool]
nakyvatJasenet = unsafePerformIO $ newIORef []

-- Tietorakenne jossa säilytetään tiedot käsiteltävästä kerhosta
valittuKerho :: IORef Kerho
valittuKerho = unsafePerformIO $ newIORef (Kerho Nothing [])

--Testi jäsen
testiJasen :: Jasen 
testiJasen = Jasen (Just "terve") (Just "070995-2351") (Just "taitoniekantie") (Just 0671134) (Just "jkl") (Just 095458) (Just 084357) (Just 5487515) (Just 20) (Just 20) (Just 20) Nothing [(Harrastus (Just "taido") (Just 50) (Just 40)), (Harrastus (Just "karate") (Just 5) (Just 2)),(Harrastus (Just "aikido") (Just 30) (Just 10))]

-- Ottaa jäsenentietokentistä arvot ja tarkistaa ovatko ne oikein, jonka jälkeen päivittää tiedot tietorakenteeseen
muokkaaJasenCallback :: Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref SelectBrowser -> Ref Button -> IO ()
muokkaaJasenCallback n' h' k' pn' po' kp' tp' ap' lv' jm' mm' l' selain b' = do
    line <- getValue selain
    sisalla <- displayed selain line
    if not sisalla
        then do return ()
        else do
            nimi        <- (getValue n' )
            hetu        <- (getValue h' )
            katu        <- (getValue k' )
            postiN      <- (getValue pn')
            postiO      <- (getValue po')
            kotiP       <- (getValue kp')
            tyoP        <- (getValue tp')
            autoP       <- (getValue ap')
            liittymis   <- (getValue lv')
            jasenM      <- (getValue jm')
            maksettuM   <- (getValue mm')
            lisa        <- (getValue l' )
            
            if not ((validoiHetu hetu) && (nimi /= "") && (katu /= "") && (postiO /= "") && (postiN /= "") && (kotiP /= "") && (tyoP /= "") && (autoP /= "") && (liittymis /= "") && (jasenM /= "") && (maksettuM /= ""))
                then do
                        mapM virheet [n', k', pn', po', kp', tp', ap', lv', jm', mm']
                        case validoiHetu hetu of 
                            False   -> do   
                                            setLabelcolor h' redColor
                                            redraw h'
                            _       -> do 
                                            labelMustaksi h'
                        return ()
                else do   
                        labelMustaksi h' 
                        labelMustaksi n' 
                        labelMustaksi k' 
                        labelMustaksi pn'
                        labelMustaksi po'
                        labelMustaksi kp'
                        labelMustaksi tp'
                        labelMustaksi ap'
                        labelMustaksi lv'
                        labelMustaksi jm'
                        labelMustaksi mm'
                        kerho <- (readIORef valittuKerho)
                        indeksi <- (jasenenOikeaPaikka (lineNumberToInt line))
                        valittuJasen <- (jasenenValinta (jasenet kerho) (lineNumberToInt line))
                        modifyIORef valittuKerho (muokkaaJasen  (Jasen  (Just nimi) 
                                                                        (Just hetu)
                                                                        (Just katu)
                                                                        (Just (read (T.unpack postiN) :: Int))
                                                                        (Just postiO) 
                                                                        (Just (read (T.unpack kotiP) :: Int))
                                                                        (Just (read (T.unpack tyoP) :: Int))
                                                                        (Just (read (T.unpack autoP) :: Int))
                                                                        (Just (read (T.unpack liittymis) :: Int))
                                                                        (Just (read (T.unpack jasenM) :: Double))
                                                                        (Just (read (T.unpack maksettuM) :: Double))
                                                                        (Just lisa)
                                                                        (harrastukset valittuJasen))
                                                                        indeksi)
                        remove selain line
                        insert selain line nimi
                        modifyIORef valittuKerho sortKerho
                        haetutRivit selain


--Tarkistaa onko kenttään annettu arvo, mahdollisesti lisätään parametriksi funktio, jota käytetään arvon tarkastukseen
virheet :: Ref Input -> IO ()
virheet input = do 
                    arvo <- getValue input
                    case T.strip arvo of
                        ""  -> do   setLabelcolor input redColor
                                    redraw input
                        _   -> do return ()

-- Apufunktio, joka muuttaa labelin värin mustaksi                  
labelMustaksi :: Ref Input -> IO ()
labelMustaksi input = do
                        setLabelcolor input blackColor
                        redraw input --Jostain syystä tämäkään redraw ei toimi
-- Karsii jäsenlistalta jäsenet jotka eivät täytä hakukriteeriä
hakuCallback :: Ref SelectBrowser -> Ref Choice -> Ref Input -> IO ()
hakuCallback selain valitsin haku = do
    hakusana <- getText valitsin
    case hakusana of
        "Nimi"              -> do hakuCallbackApu selain haku (mbyToText . nimi)
        "Hetu"              -> do hakuCallbackApu selain haku (mbyToText . hetu)
        "Katuosoite"        -> do hakuCallbackApu selain haku (mbyToText . katuosoite)
        "Postinumero"       -> do hakuCallbackApu selain haku (mbyNumToText . postinumero)
        "Postiosoite"       -> do hakuCallbackApu selain haku (mbyToText . postiosoite)
        "Kotipuhelin"       -> do hakuCallbackApu selain haku (mbyNumToText . kotipuhelin)
        "Työpuhelin"        -> do hakuCallbackApu selain haku (mbyNumToText . tyopuhelin)
        "Autopuhelin"       -> do hakuCallbackApu selain haku (mbyNumToText . autopuhelin)
        "Liittymisvuosi"    -> do hakuCallbackApu selain haku (mbyNumToText . liittymisvuosi)
        "Jäsenmaksu"        -> do hakuCallbackApu selain haku (mbyNumToText . jasenmaksu)
        "Maksettu maksu"    -> do hakuCallbackApu selain haku (mbyNumToText . maksettu)
        "Lisätietoja"       -> do hakuCallbackApu selain haku (mbyToText . lisatieto)
        
-- Muuttaa maybe textin textiksi, jotta voimme laittaa ulos jotakin siinätapauksessa jos kyseinen arvo on nothing
mbyToText :: Maybe T.Text -> T.Text
mbyToText text = case text of
                    Just x  -> x
                    Nothing -> " "
-- Sama kun ylempi mutta numeroille (toimii tosin kaikelle joille Show on määritelty)
mbyNumToText :: Show a => Maybe a -> T.Text
mbyNumToText num = case num of
                    Just x  -> T.pack (show x)
                    Nothing -> " "

-- Ylläpitää nakyvatJasenet muuttujaa, joka määrittää mitkä jäsenet ovat haettuja ja kutsuu haetutRivit funktiota joka kirjoittaa kyseiset jäsenet listalle
hakuCallbackApu :: Ref SelectBrowser -> Ref Input -> (Jasen -> T.Text) -> IO ()
hakuCallbackApu selain hakusana kriteeri = do
                                        sana <- getValue hakusana
                                        if (vertailu sana (T.pack ""))
                                            then do
                                                pituus <- readIORef valittuKerho
                                                writeIORef nakyvatJasenet (replicate (length (jasenet pituus)) True)
                                                haetutRivit selain
                                            else do
                                                kerho <- readIORef valittuKerho
                                                writeIORef nakyvatJasenet (map ((vertailu sana) . kriteeri) (jasenet kerho))
                                                haetutRivit selain

-- Kirjoittaa haettujen jäsenten nimet jäsenlistaan
haetutRivit :: Ref SelectBrowser -> IO ()
haetutRivit selain = do
                        clear selain
                        kerho <- readIORef valittuKerho
                        nakyvat <- readIORef nakyvatJasenet
                        if (nakyvat == [])
                            then do
                                mapM ((add selain) . mbyToText . nimi) (jasenet kerho)
                                return ()
                            else do
                                let naytettavat = zip (jasenet kerho) nakyvat
                                mapM (haetutRivitApu selain) naytettavat
                                return ()

-- Apufunktio ylemmälle funktiolle, vertailee täyttääkö yksin jäsen hakuvaatimukset
haetutRivitApu :: Ref SelectBrowser -> (Jasen, Bool) -> IO ()
haetutRivitApu selain lista = do 
                                case ((nimi (fst lista)), (snd lista)) of
                                    (Nothing, True)     -> do 
                                                                add selain ""
                                    (Just x, True)      -> do
                                                                add selain x
                                    (_, False)          -> do
                                                                return ()

-- Luo ikkunan jolla käyttäjä voi vaihtaa kerhon nimeä
vaihdaKerhonNimiCallback :: Ref Button -> IO ()
vaihdaKerhonNimiCallback b = do
                                w <- windowNew (toSize (300,100)) Nothing (Just "Vaihda kerhon nimeä")
                                begin w
                                b <-    buttonNew
                                        (Rectangle (Position (X 100) (Y 65)) (Size (Width 100) (Height 30)))
                                        (Just "Vaihda kerhon Nimi")
                                iKohde       <- inputNew (toRectangle (100,30,190,25)) (Just "Kerhon nimi") (Just FlNormalInput)
                                setCallback b (vaihdaKerhonNimi iKohde w)
                                end w
                                showWidget w

-- Callbackfunktio joka vaihtaa kerhon nimen
vaihdaKerhonNimi :: Ref Input -> Ref Window -> Ref Button -> IO ()
vaihdaKerhonNimi input w b = do
                                uusiNimi <- getValue input
                                case T.strip uusiNimi of
                                    ""  -> do return ()
                                    x   -> do   
                                                kerho <- readIORef valittuKerho
                                                modifyIORef valittuKerho (vaihdaKerhonNimea uusiNimi)
                                                hide w
                                                
-- Tallentaa kerhon tiedot kahteen tiedostoon: "kerhon nimi".dat joka sisältää jäsenten tiedot sekä "kerhon nimi".har joka sisältää harrastusten tiedot
tallennaKerho :: Ref Button -> IO ()
tallennaKerho b = do
                kerho <- readIORef valittuKerho
                case kerhonNimi kerho of
                    Nothing     -> do return ()
                    Just x      -> do 
                                        TIO.writeFile ((T.unpack x) ++ ".dat") (tallennaJasenet (jasenet kerho))
                                        TIO.writeFile ((T.unpack x) ++ ".har") (tallennaHarrastukset (jasenet kerho))
                                        
-- Muuttaa jäsenten tiedot tekstimuotoon
tallennaJasenet :: [Jasen] -> T.Text
tallennaJasenet joukko = T.concat (map tiedotTekstiksi joukko)

-- Apufunktio tallennaJasenet funktiolle
tiedotTekstiksi :: Jasen -> T.Text
tiedotTekstiksi jasen = T.append    (T.concat ([(T.append (mbyToText (nimi jasen)) "||"), 
                                                (T.append (mbyToText (hetu jasen)) "||"), 
                                                (T.append (mbyToText (katuosoite jasen)) "||"), 
                                                (T.append (mbyNumToText (postinumero jasen)) "||"), 
                                                (T.append (mbyToText (postiosoite jasen)) "||"), 
                                                (T.append (mbyNumToText (kotipuhelin jasen)) "||"), 
                                                (T.append (mbyNumToText (tyopuhelin jasen)) "||"), 
                                                (T.append (mbyNumToText (autopuhelin jasen)) "||"), 
                                                (T.append (mbyNumToText (liittymisvuosi jasen)) "||"), 
                                                (T.append (mbyNumToText (jasenmaksu jasen)) "||"), 
                                                (T.append (mbyNumToText (maksettu jasen)) "||"), 
                                                (T.append (mbyToText (lisatieto jasen)) "||")]))
                                    (T.pack "\n")

-- Muuttaa harrastuksien tiedot tekstiksi ja lisää tietoihin jäsentä vastaavan indeksin jotta ne voidaan kirjoittaa tallennustiedostoon
tallennaHarrastukset :: [Jasen] -> T.Text
tallennaHarrastukset joukko = T.concat (map jasenenHarrastuksetTekstiksi (zip [0..((length joukko) - 1)] joukko))

-- Muuttaa yhden harrastuksen tiedot tekstiksi
jasenenHarrastuksetTekstiksi :: (Int, Jasen) -> T.Text
jasenenHarrastuksetTekstiksi (i, jasen) = T.concat (map (harrastusTekstiksi i) (harrastukset jasen))

-- Callback joka luo ikkunan jolle voidaan syöttää ladattavan harrastuksen nimi
latausIkkunaCallback :: Ref Button -> IO ()
latausIkkunaCallback b = do
                            w <- windowNew (toSize (300,100)) Nothing (Just "Avaa kerho")
                            begin w
                            b <-    buttonNew
                                    (Rectangle (Position (X 125) (Y 65)) (Size (Width 50) (Height 30)))
                                    (Just "Avaa")
                            iKohde       <- inputNew (toRectangle (100,30,190,25)) (Just "Kerhon nimi") (Just FlNormalInput)
                            setCallback b (lataaKerho iKohde)
                            end w
                            showWidget w

-- Lataa kerhon tiedot kerhon nimeä vastaavista tiedostoista .dat ja .har
lataaKerho :: Ref Input -> Ref Button -> IO ()
lataaKerho input b = do
                        ladattavanKerhonNimi <- getValue input
                        jasenTiedostonSisalto <- TIO.readFile ((T.unpack ladattavanKerhonNimi) ++ ".dat")
                        let kerhonJasenet = map tekstiJaseneksi (T.splitOn "\n" jasenTiedostonSisalto)
                        harrastusTiedostonSisalto <- TIO.readFile ((T.unpack ladattavanKerhonNimi) ++ ".har")
                        writeIORef valittuKerho (Kerho (Just ladattavanKerhonNimi) kerhonJasenet)
                        let kerhonJasenienHarrastukset = map tekstiHarrastukseksi (T.splitOn "\n" harrastusTiedostonSisalto)
                        jasenetIlmanHarrastuksia <- readIORef valittuKerho
                        lisaaKerhoonHarrastukset kerhonJasenienHarrastukset (jasenet jasenetIlmanHarrastuksia) 

-- Ottaa harrastukset ja indeksit ja asettaa ne vastaaville jäsenille
lisaaKerhoonHarrastukset :: [(Maybe Int, Harrastus)] -> [Jasen] -> IO ()
lisaaKerhoonHarrastukset harrastukset jasenet = do
                                                    mapM (kerhonHarrastukset jasenet) harrastukset
                                                    return ()

-- Lisää yhden harrastuksen kerhon jäsenille
kerhonHarrastukset :: [Jasen] -> (Maybe Int, Harrastus) -> IO ()
kerhonHarrastukset henkilot (i, harrastus) = do
                                                case i of 
                                                    Nothing     -> do return ()
                                                    Just x      -> do
                                                                    kerho <- readIORef valittuKerho
                                                                    modifyIORef valittuKerho (muokkaaJasen (lisaaHarrastus harrastus (henkilot !! x)) x)

-- Jakaa tiedostosta luettavan tekstin osiin, jotka voidaan laittaa myöhemmin harrastuksille
tekstiHarrastukseksi :: T.Text -> (Maybe Int, Harrastus)
tekstiHarrastukseksi teksti = harrastusTekstista (T.splitOn "||" teksti)

-- Apufunktio tekstitHarrastukseksi funktiolle
harrastusTekstista :: [T.Text] -> (Maybe Int, Harrastus)
harrastusTekstista teksti = (validoiInt (teksti !! 0), (Harrastus   (validoiTeksti (teksti !! 1))
                                                                    (validoiInt (teksti !! 2))
                                                                    (validoiDouble (teksti !! 3))))

-- Jakaa tekstin osiin jotka voidaan asettaa jäsenen arvoiksi
tekstiJaseneksi :: T.Text -> Jasen
tekstiJaseneksi teksti = jasenTekstista (T.splitOn "||" teksti)

-- Syö listan tekstejä ja matchaa ne vastaaviin jäsen datan tietoihin
jasenTekstista :: [T.Text] -> Jasen
jasenTekstista tekstit = Jasen  (validoiTeksti (tekstit !! 0))
                                (validoiJasenenHetu (tekstit !! 1))
                                (validoiTeksti (tekstit !! 2))
                                (validoiInt (tekstit !! 3))
                                (validoiTeksti (tekstit !! 4))
                                (validoiInt (tekstit !! 5))
                                (validoiInt (tekstit !! 6))
                                (validoiInt (tekstit !! 7))
                                (validoiInt (tekstit !! 8))
                                (validoiDouble (tekstit !! 9))
                                (validoiDouble (tekstit !! 10))
                                (validoiTeksti (tekstit !! 11))
                                []

-- Katsoo että teksti ei ole tyhjä merkkijono, jolloin datan kyseinen arvo saa arvokseen Nothing
validoiTeksti :: T.Text -> Maybe T.Text
validoiTeksti teksti = case T.strip teksti of
                                ""  -> Nothing
                                _   -> Just teksti

-- Tarkistaa että annettu hetu on kelvollinen
validoiJasenenHetu :: T.Text -> Maybe T.Text
validoiJasenenHetu teksti = case validoiHetu teksti of
                                    True    -> Just teksti
                                    _       -> Nothing
                                    
-- Tarkistaa että luettu arvo on validi kokonaisluku, tai muuten antaa kyseiselle arvoksi Nothing
validoiInt :: T.Text -> Maybe Int
validoiInt teksti = (readMaybe (T.unpack teksti)) :: Maybe Int

-- Tarkistaa että luettu arvo on validi desimaaliluku, tai muuten antaa kyseiselle arvoksi Nothing
validoiDouble :: T.Text -> Maybe Double
validoiDouble teksti = (readMaybe (T.unpack teksti)) :: Maybe Double

-- Muuttaa yhden harrastuksen sekä kyseisen harrastuksen jäsenen indeksin kerhossa tekstiksi
harrastusTekstiksi :: Int -> Harrastus -> T.Text
harrastusTekstiksi i harrastus = T.append (T.concat [(T.append (T.pack (show i)) "||"), (T.append (mbyToText (laji harrastus)) "||"), (T.append (mbyNumToText (aloitusvuosi harrastus)) "||"), (T.append (mbyNumToText (tuntiaViikossa harrastus)) "||")]) (T.pack "\n")

-- Vertaa onko teksti osa toista tekstiä, hieman turhaa ehkä koska kyseessähän on vain eri nimitys isInfixOf funktiolle, mutta järkeilin että teen siitä erillisen funktion siltä varalta jos muutan hakusysteemi
vertailu :: T.Text -> T.Text -> Bool
vertailu a b = T.isInfixOf a b

-- Poistaa jäsenen ylläpidetystä tietorakenteesta, sekä jäsenlistasta
poistaJasenCallback :: Ref SelectBrowser -> Ref Button -> IO () -- MenuItemBase
poistaJasenCallback selain b' = do
    line <- getValue selain
    sisalla <- displayed selain line
    if not sisalla
        then do return ()
        else do
            valittuJasen <- (jasenenOikeaPaikka (lineNumberToInt line))
            modifyIORef valittuKerho (poistaJasen valittuJasen)
            remove selain line


-- Valitsemalla jäsen listasta päivittää tiedonlisäyskentät sekä harrastustaulukon
valitseJasenCallback :: Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref TableRow -> Ref SelectBrowser -> IO ()
valitseJasenCallback n' h' k' pn' po' kp' tp' ap' lv' jm' mm' l' table selain = do
    line <- getValue selain
    sisalla <- displayed selain line
    case sisalla of
        False  -> return ()
        True   -> do 
                labelMustaksi h' 
                labelMustaksi n' 
                labelMustaksi k' 
                labelMustaksi pn'
                labelMustaksi po'
                labelMustaksi kp'
                labelMustaksi tp'
                labelMustaksi ap'
                labelMustaksi lv'
                labelMustaksi jm'
                labelMustaksi mm'
                kerho   <- (readIORef valittuKerho)
                valittuJasen <- (jasenenValinta (jasenet kerho) (lineNumberToInt line))
                setValue n' (case nimi valittuJasen of --jasenTekstiksi nimi ((jasenet kerho) !! (lineNumberToInt line)) 
                                    Just x  -> x
                                    Nothing -> T.pack "")
                setValue h' (case hetu valittuJasen of
                                    Just x  -> x
                                    _       -> T.pack "")
                setValue k' (case katuosoite valittuJasen of
                                    Just x  ->  x
                                    _       -> T.pack "")
                setValue pn' (case postinumero valittuJasen of
                                    Just x  -> T.pack (show x)
                                    _       -> T.pack "")
                setValue po' (case postiosoite valittuJasen  of
                                    Just x  -> x
                                    _       -> T.pack "")
                setValue kp' (case kotipuhelin valittuJasen of
                                    Just x  -> T.pack (show x)
                                    _       -> T.pack "")
                setValue tp' (case tyopuhelin valittuJasen of
                                    Just x  -> T.pack (show x)
                                    _       -> T.pack "")
                setValue ap' (case autopuhelin valittuJasen of
                                    Just x  -> T.pack (show x)
                                    _       -> T.pack "")
                setValue lv' (case liittymisvuosi valittuJasen of
                                    Just x  -> T.pack (show x)
                                    _       -> T.pack "")
                setValue jm' (case jasenmaksu valittuJasen of
                                    Just x  -> T.pack (show x)
                                    _       -> T.pack "")
                setValue mm' (case maksettu valittuJasen of
                                    Just x  -> T.pack (show x)
                                    _       -> T.pack "")
                setValue l' (case lisatieto valittuJasen of
                                    Just x  -> x
                                    _       -> T.pack "")
                                    
                writeIORef rowData (jasenenHarrastukset valittuJasen)
                readIORef rowData >>= setRows table . Rows . length
                writeIORef sortRev False
                writeIORef sortLast (-1)
                redraw table --Tämä redraw toimii 
                return ()

-- Muutetaan jäsenen harrastuksien tiedot muotoon jossa ne voidaan laittaa esille taulukkoon
jasenenHarrastukset :: Jasen -> [[T.Text]]
jasenenHarrastukset (Jasen _ _ _ _ _ _ _ _ _ _ _ _ []) = [[(T.pack ""), (T.pack ""), (T.pack "")]]
jasenenHarrastukset (Jasen _ _ _ _ _ _ _ _ _ _ _ _ xs) = map jasenenHarrastuksetApu xs

-- Yksittäisen harrastuksen muuttaminen tekstiksi
jasenenHarrastuksetApu :: Harrastus -> [T.Text]
jasenenHarrastuksetApu harrastus = case ((laji harrastus), (aloitusvuosi harrastus), (tuntiaViikossa harrastus)) of
                                            (Nothing, Nothing, Nothing)     -> [(T.pack ""),(T.pack ""),(T.pack "")]
                                            ((Just a), Nothing, Nothing)    -> [a,(T.pack ""),(T.pack "")]
                                            (Nothing, (Just b), Nothing)    -> [(T.pack ""), (T.pack (show b)), (T.pack "")]
                                            (Nothing, Nothing, (Just c))    -> [(T.pack ""), (T.pack ""), (T.pack (show c))]
                                            ((Just a), (Just b), Nothing)   -> [a,(T.pack (show b)), (T.pack "")]
                                            ((Just a), Nothing, (Just c))   -> [a,(T.pack ""), (T.pack (show c))]
                                            (Nothing, (Just b), (Just c))   -> [(T.pack ""), (T.pack (show b)), (T.pack (show c))]
                                            ((Just a), (Just b), (Just c))  -> [a, (T.pack (show b)), (T.pack (show c))]

-- Luodaan ikkuna jonka avulla käyttäjä voi lisätä uuden harrastuksen
lisaaHarrastusCallback :: Ref TableRow -> Ref SelectBrowser -> Ref Button -> IO ()
lisaaHarrastusCallback table selain b = do
                                    line <- getValue selain
                                    sisalla <- displayed selain line
                                    if not sisalla
                                        then do return ()
                                        else do
                                            w <- windowNew (toSize (400,200)) Nothing (Just "Lisää harrastus")
                                            begin w
                                            b <-    buttonNew
                                                    (Rectangle (Position (X 140) (Y 160)) (Size (Width 120) (Height 30)))
                                                    (Just "Lisää harrastus")
                                            iLaji       <- inputNew (toRectangle (100,30,250,25)) (Just "Harrastus") (Just FlNormalInput)
                                            iAloitus    <- inputNew (toRectangle (100,60,250,25)) (Just "Aloitusvuosi") (Just FlIntInput)
                                            iTuntia     <- inputNew (toRectangle (100,90,250,25)) (Just "Tuntia viikossa") (Just FlFloatInput)
                                            arvo        <- getValue selain
                                            setCallback b (lisaaHarrastusApuCallback iLaji iAloitus iTuntia (lineNumberToInt arvo) table w selain)
                                            end w
                                            showWidget w
                                    
                                    
-- Callback harrastuksen lisäämisestä avautuvaan ikkunaan
lisaaHarrastusApuCallback :: Ref Input -> Ref Input -> Ref Input -> Int -> Ref TableRow -> Ref Window -> Ref SelectBrowser -> Ref Button -> IO ()
lisaaHarrastusApuCallback laj aloitus tuntia i table window selain b = do
                                                                    line <- getValue selain
                                                                    sisalla <- displayed selain line
                                                                    if not sisalla
                                                                        then do return ()
                                                                        else do
                                                                            l <- getValue laj
                                                                            a <- getValue aloitus
                                                                            t <- getValue tuntia
                                                                            kerho <- readIORef valittuKerho
                                                                            indeksi <- (jasenenValinta (jasenet kerho) i)
                                                                            jasenenIndeksi <- (jasenenOikeaPaikka i)
                                                                            modifyIORef valittuKerho (muokkaaJasen (lisaaHarrastus (Harrastus (Just l) (Just (read (T.unpack a))) (Just (read (T.unpack t)))) indeksi) jasenenIndeksi)
                                                                            writeIORef rowData (jasenenHarrastukset indeksi)
                                                                            readIORef rowData >>= setRows table . Rows . length
                                                                            hide window
                                                                            redraw table --Jostain syystä ei toimi, jäsen valittava uudestaan jotta taulukko päivittyy

                                                                    

-- Muutetaan listan linenumber vastaamaan tietorakenteessa kyseisen henkilön indeksiä
lineNumberToInt :: LineNumber -> Int
lineNumberToInt (LineNumber x) = (read (show x)) - 1

-- Tarkistaa kuinka monta jäsentä on piilotettu haulla, ja antaa jäsenen oikean indeksin kerhosta
jasenenOikeaPaikka :: Int -> IO (Int)
jasenenOikeaPaikka i = do
                            nakyvat <- readIORef nakyvatJasenet
                            return (i + (length (filter ( == False) (take (i+1) nakyvat))))

-- Antaa korjatun jäsenen paikan kun olemme ottaneet huomioon piilotetut jäsenet, hieman turha funktio kun voimme vain valita jäsenen oikean indeksin ylemmällä funktiolla
jasenenValinta :: [Jasen] -> Int -> IO (Jasen)
jasenenValinta listaJasenista i = do
                                    nakyvat <- readIORef nakyvatJasenet
                                    return (jasenenValintaApu listaJasenista nakyvat i)

-- Apufunktio ylemälle
jasenenValintaApu :: [Jasen] -> [Bool] -> Int -> Jasen
jasenenValintaApu (x:xs) (y:ys) i = case (y, i) of
                                        (True, 0)   -> x
                                        _           -> case y of
                                                            True    -> jasenenValintaApu xs ys (i-1)
                                                            _       -> jasenenValintaApu xs ys i

-- Muuttaa TableCoordinaten kyseisen rivin indeksiksi
tcToInt :: TableCoordinate -> Int
tcToInt (TableCoordinate (Row x) y) = (read (show x))

-- Poistaa valitun harrastuksen
poistaHarrastusCallback :: Ref SelectBrowser -> Ref TableRow -> Ref Button -> IO ()
poistaHarrastusCallback selain taulu b = do
                                    rivi <- getSelection taulu
                                    tiedot <- readIORef rowData
                                    line <- getValue selain
                                    kerho <- readIORef valittuKerho
                                    let currentRow = tiedot !! (tcToInt (fst rivi))
                                    i <- jasenenOikeaPaikka (lineNumberToInt line)
                                    valittuJasen <- (jasenenValinta (jasenet kerho) i)
                                    modifyIORef valittuKerho (muokkaaJasen (poistaHarrastus (Harrastus (Just (currentRow !! 0)) (Just (read (T.unpack(currentRow !! 1)))) (Just (read (T.unpack(currentRow !! 2))))) valittuJasen) i) 
                                    writeIORef rowData (jasenenHarrastukset valittuJasen)
                                    readIORef rowData >>= setRows taulu . Rows . length
                                    writeIORef sortRev False
                                    writeIORef sortLast (-1)
                                    redraw taulu

-- Lisätään uusi jäsen oletusarvoilla tietorakenteeseen, sekä päivitetään jäsenlistaa
lisaaJasenCallback :: Ref SelectBrowser -> Ref Button -> IO ()
lisaaJasenCallback selain button = do
    modifyIORef valittuKerho (lisaaJasen (Jasen (Just (T.pack "uusi jäsen")) 
                                                (Nothing)
                                                (Nothing)
                                                (Nothing)
                                                (Nothing) 
                                                (Nothing)
                                                (Nothing)
                                                (Nothing)
                                                (Nothing)
                                                (Nothing)
                                                (Nothing)
                                                (Nothing)
                                                []))
    
    modifyIORef valittuKerho sortKerho
    --hakuCallback selain --Ref SelectBrowser -> Ref Choice -> Ref Input -> IO ()
    nakyvat <- readIORef nakyvatJasenet 
    writeIORef nakyvatJasenet (nakyvat ++ [True])
    haetutRivit selain

{----------------------------------------------------------------------------------------------------------------
Tämän viivan alapuolella oleva koodi käsittelee harrastustaulukkoa. Kyseinen koodi on otettu lähes suoraan pienin 
muutoksin fltkhs demosta "fltkhs-table-sort"
-----------------------------------------------------------------------------------------------------------------}
headers :: [T.Text]
headers = map T.pack ["Harrastus", "aloitusvuosi", "tuntia vko"]

rowFontFace :: Font
rowFontFace = helvetica

rowFontSize :: FontSize
rowFontSize = FontSize 16

margin :: Int
margin = 20

headerFontFace :: Font
headerFontFace = helveticaBold
headerFontSize :: FontSize
headerFontSize = FontSize 16

rowData :: IORef [[T.Text]]
rowData = unsafePerformIO $ newIORef ([["","",""]])

sortRev :: IORef Bool
sortRev = unsafePerformIO $ newIORef False

sortLast :: IORef Int
sortLast = unsafePerformIO $ newIORef (-1)

tableState :: TableState
tableState = TableState sortRev sortLast rowData

data TableState = TableState {
  sortReverse :: IORef Bool,
  sortLastCol :: IORef Int,
  row :: IORef [[T.Text]]
  }

eventCallback :: TableState -> Ref TableRow -> IO ()
eventCallback tableState table = do
  (Column  col') <- callbackCol table
  context' <- callbackContext table
  case context' of
   ContextColHeader -> do
     event' <- FL.event
     mouseButton' <- FL.eventButton
     case mouseButton' of
       Nothing -> return ()
       Just mb' -> if (event' == Release && mb' == Mouse_Left)
                   then do
                     sortLastCol' <- readIORef (sortLastCol tableState)
                     if (sortLastCol' == col')
                       then readIORef (sortReverse tableState) >>= writeIORef (sortReverse tableState) . toggle
                       else writeIORef (sortReverse tableState) False
                     sortReverse' <- readIORef (sortReverse tableState)
                     rowData <- readIORef (row tableState) >>= return . zip [(0 :: Int)..]
                     let sorted = sortBy (compare `on` (indexOr "" col'. snd)) rowData
                     writeIORef
                       (row tableState)
                       (if sortReverse'
                        then (reverse $ map snd sorted)
                        else map snd sorted)
                     writeIORef (sortLastCol tableState) col'
                     redraw table
                     else return ()
   _ -> return ()
  where toggle True = False
        toggle False = True

drawSortArrow :: Rectangle -> Bool -> IO ()
drawSortArrow (Rectangle (Position (X x') (Y y')) (Size (Width w') (Height h'))) sortReverse' =
  let xlft = x' + (w'-6) - 8
      xctr = x' + (w'-6) - 4
      xrit = x' + (w'-6) - 0
      ytop = y' + (truncate ((fromIntegral h' / 2) :: Double)) - 4
      ybot = y' + (truncate ((fromIntegral h' / 2) :: Double)) + 4
  in
   if sortReverse'
   then do
     flcSetColor whiteColor
     flcLine (Position (X xrit) (Y ytop)) (Position (X xctr) (Y ybot))
     flcSetColor (Color 41)
     flcLine (Position (X xlft) (Y ytop)) (Position (X xrit) (Y ytop))
     flcLine (Position (X xlft) (Y ytop)) (Position (X xctr) (Y ybot))
   else do
     flcSetColor whiteColor
     flcLine (Position (X xrit) (Y ybot)) (Position (X xctr) (Y ybot))
     flcLine (Position (X xrit) (Y ybot)) (Position (X xlft) (Y ybot))
     flcSetColor (Color 41)
     flcLine (Position (X xlft) (Y ybot)) (Position (X xctr) (Y ytop))

setIndex :: Int -> [a] -> (a -> a) -> [a]
setIndex idx' xs f =
  map
   (
    \(i,e) -> if (i == idx')
                 then f e
                 else e
   )
   (zip [0..] xs)

indexOr :: a -> Int -> [a] -> a
indexOr fallback idx xs =
  if (idx < length xs)
  then xs !! idx
  else fallback

drawCell ::  TableState -> Ref TableRow -> TableContext -> TableCoordinate -> Rectangle -> IO ()
drawCell tableState table tc (TableCoordinate (Row row') (Column col')) rectangle' =
  let (x',y',w',h') = fromRectangle rectangle'
  in do
    sortReverse' <- readIORef (sortReverse tableState)
    sortLastCol' <- readIORef (sortLastCol tableState)
    rowData <- readIORef (row tableState)
    (Columns numCols) <- getCols table
    (Rows numRows) <- getRows table
    if (row' < numRows && col' < numCols)
      then case tc of
            ContextColHeader -> do
              flcPushClip rectangle'
              flcDrawBox ThinUpBox rectangle' backgroundColor
              if (col' < 9)
                then do
                  flcSetColor blackColor
                  flcDrawInBox
                    (headers !! col')
                    (toRectangle ((x'+2),y',w',h'))
                    alignLeft
                    Nothing
                    Nothing
                  if (col' == sortLastCol')
                    then drawSortArrow rectangle' sortReverse'
                    else return ()
                else return ()
              flcPopClip
            ContextCell -> do
              flcPushClip rectangle'
              bgColor <- do
                isSelected' <- getRowSelected table (Row row')
                case isSelected' of
                  Right is' -> if is'
                               then getSelectionColor table
                               else return whiteColor
                  Left _ -> error $ "Row: " ++ (show row') ++ " is out of range."
              flcSetColor bgColor
              flcRectf rectangle'
              flcSetFont rowFontFace rowFontSize
              flcSetColor blackColor
              let currentRow = rowData !! row'
              flcDrawInBox
                (indexOr "" col' currentRow)
                (toRectangle $ (x'+2,y',w',h'))
                alignLeft
                Nothing
                Nothing
              flcSetColor light2Color
              flcRect rectangle'
              flcPopClip
            _ -> return ()
      else return ()

autowidth :: Ref TableRow -> Int -> [[T.Text]] -> IO ()
autowidth table pad rowData' = do
  flcSetFont headerFontFace headerFontSize
  mapM_
    (\(colNum, colName) -> do
        (Size (Width w') _) <- flcMeasure colName Nothing True
        setColWidth table (Column colNum) (w' + pad)
    )
    (zip [0 ..] headers)
  flcSetFont rowFontFace rowFontSize
  mapM_
    (\row' -> do
      mapM_
        (\(colIdx,col) -> do
            (Size (Width wc') _) <- flcMeasure col Nothing True
            colWidth' <- getColWidth table (Column colIdx)
            if (wc' + pad > colWidth')
              then setColWidth table (Column colIdx) (wc' + pad)
              else return ()
        )
        (zip [0..] row')
    )
    rowData'
  -- need to do { table_resized(); redraw(); }
  -- but table_resized() is unexposed.
  -- setting the row_header flag induces this.
  getRowHeader table >>= setRowHeader table

resize_window :: Ref DoubleWindow -> Ref TableRow -> IO ()
resize_window window table = do
  let width = (4 :: Int)
  (Columns numCols) <- getCols table
  colWidthTotal <- liftM sum $ mapM (getColWidth table . Column) [0..(numCols - 1)]
  let totalWidth = width + colWidthTotal + (margin * 2)
  appWidth <- FL.w
  if (totalWidth < 200 || totalWidth > appWidth)
    then return ()
    else do
      (x', y', h', _) <- fmap fromRectangle (getRectangle window)
      resize window $ toRectangle (x',y',totalWidth,h')
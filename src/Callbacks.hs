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
import Control.Monad
import System.IO.Unsafe
import qualified Data.Text as T

valittuJasen :: IORef Jasen
valittuJasen = unsafePerformIO $ newIORef (Jasen Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [])

valittuKerho :: IORef Kerho
valittuKerho = unsafePerformIO $ newIORef (Kerho Nothing [])

--TODO: poista callbackin testaus funktio ja lisää oikeat callbackit, jahka tietorakenteet ovat saatu valmiiksi
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
                            False   -> do setLabel h' "Henkilötunnus ei validi"
                            _       -> do setLabel h' "Hetu"
                        return ()
                else do   
                        setLabel h' "Hetu"
                        setLabel n' "Nimi"
                        setLabel k' "Katuosoite"
                        setLabel pn' "Postinumero"
                        setLabel po' "Postiosoite"
                        setLabel kp' "Kotipuhelin"
                        setLabel tp' "Työpuhelin"
                        setLabel ap' "Autopuhelin"
                        setLabel lv' "Liittymisvuosi"
                        setLabel jm' "Jäsenmaksu"
                        setLabel mm' "Maksettu maksu"
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
                                                                        [])
                                                                (lineNumberToInt line))
                        remove selain line
                        insert selain line nimi
                        sort selain
                        modifyIORef valittuKerho sortKerho

virheet :: Ref Input -> IO ()
virheet input = do 
                    arvo <- getValue input
                    case arvo of
                        ""  -> do setLabel input "Kenttä ei täytetty"
                        _   -> do return ()
--
--hakuCallback :: Ref SelectBrowser -> Ref Choice -> Ref Input -> IO ()
--hakuCallback selain valitsin haku = do
--    case valitsin of
--        "Nimi"              -> do
--        "Hetu"              -> do
--        "Katuosoite"        -> do
--        "Postinumero"       -> do
--        "Postiosoite"       -> do
--        "Kotipuhelin"       -> do
--        "Työpuhelin"        -> do
--        "Autopuhelin"       -> do
--        "Liittymisvuosi"    -> do
--        "Jäsenmaksu"        -> do
--        "Maksettu maksu"    -> do
--        "Lisätietoja"       -> do
poistaJasenCallback :: Ref SelectBrowser -> Ref MenuItemBase -> IO () -- MenuItemBase
poistaJasenCallback selain b' = do
    line <- getValue selain
    sisalla <- displayed selain line
    if not sisalla
        then do return ()
        else do
            modifyIORef valittuKerho (poistaJasen (lineNumberToInt line))
            remove selain line
    
valitseJasenCallback :: Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref SelectBrowser -> IO ()
valitseJasenCallback n' h' k' pn' po' kp' tp' ap' lv' jm' mm' l' selain = do
    line <- getValue selain
    sisalla <- displayed selain line
    --indeksi <- getValue selain
    case sisalla of
        False  -> return ()
        True   -> do 
                setLabel h' "Hetu"
                setLabel n' "Nimi"
                setLabel k' "Katuosoite"
                setLabel pn' "Postinumero"
                setLabel po' "Postiosoite"
                setLabel kp' "Kotipuhelin"
                setLabel tp' "Työpuhelin"
                setLabel ap' "Autopuhelin"
                setLabel lv' "Liittymisvuosi"
                setLabel jm' "Jäsenmaksu"
                setLabel mm' "Maksettu maksu"
                kerho   <- (readIORef valittuKerho)
                setValue n' (case nimi ((jasenet kerho) !!  (lineNumberToInt line)) of --jasenTekstiksi nimi ((jasenet kerho) !! (lineNumberToInt line)) 
                                    Just x  -> x
                                    Nothing -> T.pack "")
                setValue h' (case hetu ((jasenet kerho) !!  (lineNumberToInt line)) of
                                    Just x  -> x
                                    _       -> T.pack "")
                setValue k' (case katuosoite ((jasenet kerho) !!  (lineNumberToInt line)) of
                                    Just x  ->  x
                                    _       -> T.pack "")
                setValue pn' (case postinumero ((jasenet kerho) !!  (lineNumberToInt line)) of
                                    Just x  -> T.pack (show x)
                                    _       -> T.pack "")
                setValue po' (case postiosoite ((jasenet kerho) !!  (lineNumberToInt line)) of
                                    Just x  -> x
                                    _       -> T.pack "")
                setValue kp' (case kotipuhelin ((jasenet kerho) !!  (lineNumberToInt line)) of
                                    Just x  -> T.pack (show x)
                                    _       -> T.pack "")
                setValue tp' (case tyopuhelin ((jasenet kerho) !!  (lineNumberToInt line)) of
                                    Just x  -> T.pack (show x)
                                    _       -> T.pack "")
                setValue ap' (case autopuhelin ((jasenet kerho) !!  (lineNumberToInt line)) of
                                    Just x  -> T.pack (show x)
                                    _       -> T.pack "")
                setValue lv' (case liittymisvuosi ((jasenet kerho) !!  (lineNumberToInt line)) of
                                    Just x  -> T.pack (show x)
                                    _       -> T.pack "")
                setValue jm' (case jasenmaksu ((jasenet kerho) !!  (lineNumberToInt line)) of
                                    Just x  -> T.pack (show x)
                                    _       -> T.pack "")
                setValue mm' (case maksettu ((jasenet kerho) !!  (lineNumberToInt line)) of
                                    Just x  -> T.pack (show x)
                                    _       -> T.pack "")
                setValue l' (case lisatieto ((jasenet kerho) !!  (lineNumberToInt line)) of
                                    Just x  -> x
                                    _       -> T.pack "")
                return ()


                
lineNumberToInt :: LineNumber -> Int
lineNumberToInt (LineNumber x) = (read (show x)) - 1

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
    add selain ("uusi jäsen")
    sort selain
    modifyIORef valittuKerho sortKerho
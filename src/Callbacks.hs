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
import System.IO.Unsafe
import qualified Data.Text as T

valittuJasen :: IORef Jasen
valittuJasen = unsafePerformIO $ newIORef (Jasen Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [])

valittuKerho :: IORef Kerho
valittuKerho = unsafePerformIO $ newIORef (Kerho Nothing [])

--TODO: poista callbackin testaus funktio ja lisää oikeat callbackit, jahka tietorakenteet ovat saatu valmiiksi
muokkaaJasenCallback :: Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Input -> Ref Button -> IO ()
muokkaaJasenCallback n' h' k' pn' po' kp' tp' ap' lv' jm' mm' l' b' = do
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
  
  modifyIORef valittuKerho (lisaaJasen (Jasen   (Just nimi) 
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
                                                []))
  --l' <- getLabel b'
  --if (l' == "Hello World!")
  --  then setLabel b' "Goodbye World"
  --  else setLabel b' "Hello World"

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
    --ylin <- getTopline selain
    --select selain ylin True (lisaaJasenCallback jasenSelain)
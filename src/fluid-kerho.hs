{-# LANGUAGE OverloadedStrings #-}

module Main where
import Kayttoliittyma
import Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import qualified Data.Text as T
import Data.IORef
import Data.List
import Tietorakenteet
import System.IO
import Control.Monad
import Data.Function

-- Pääfunktio joka rupeaa pyörittämään fluid tiedostossa määriteltyä käyttöliittymää
-- TODO: poista ylimääräiset importit
main :: IO ()
main = do
  win <- makeWindow
  showWidget win
  _ <- FL.run
  return ()

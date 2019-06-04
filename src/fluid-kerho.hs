module Main where
import Kayttoliittyma
import Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations

main :: IO ()
main = do
  win <- makeWindow
  showWidget win
  _ <- FL.run
  return ()

module Main where
import Kayttoliittyma
import Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import qualified Data.Text as T
maxRows :: Int
maxRows = 30
maxCols :: Int
maxCols = 3
tableData :: [[Int]]
tableData =
    let rowIndices = [0 .. (maxRows - 1)]
        colIndices = [0 .. (maxCols - 1)]
    in
      map (\r -> map (\c -> 1000 + (r * 1000) + c) colIndices) rowIndices
drawHeader :: Ref Table -> T.Text -> Rectangle -> IO ()
drawHeader table s rectangle = do
    flcPushClip rectangle
    rhc <- getRowHeaderColor table
    flcDrawBox ThinUpBox rectangle rhc
    flcSetColor blackColor
    flcDrawInBox s rectangle alignCenter Nothing Nothing
    flcPopClip
  
drawData :: Ref Table -> T.Text -> Rectangle -> IO ()
drawData table s rectangle = do
    flcPushClip rectangle
    flcSetColor whiteColor >> flcRectf rectangle
    flcSetColor gray0Color >> flcDrawInBox s rectangle alignCenter Nothing Nothing
    color' <- getColor table
    flcSetColor color' >> flcRect rectangle
    flcPopClip
  
drawCell :: Ref Table -> TableContext -> TableCoordinate -> Rectangle -> IO ()
drawCell table context (TableCoordinate (Row row) (Column col)) rectangle = do
  case context of
   ContextStartPage -> flcSetFont helvetica (FontSize 16)
   ContextColHeader ->
       let a = fromEnum 'A'
           currentLetter :: Char
           currentLetter = (toEnum $ fromEnum a + col)
       in
       drawHeader table (T.pack [currentLetter]) rectangle
   ContextRowHeader -> drawHeader table (T.pack (show row)) rectangle
   ContextCell -> drawData table (T.pack (show $ tableData !! row !! col)) rectangle
   _ -> return ()
   
initializeTable :: Ref Table -> IO ()
initializeTable table = do
    begin table
    setRows table (Rows maxRows)
    setRowHeader table True
    setRowHeightAll table 20
    setRowResize table False
    setCols table (Columns maxCols)
    setColHeader table True
    setColWidthAll table 80
    setColResize table True
    end table
--TODO: Muuta taulukko oikean muotoiseksi ja lisää sort funktio. Taulukko on kopioitu fltkhs table-simple demosta
main :: IO ()
main = do
  win <- makeWindow
  begin win
  table <- tableCustom
             (Rectangle
               (Position (X 575) (Y 40))
               (Size (Width 260) (Height 415)))
             Nothing
             Nothing
             drawCell
             defaultCustomWidgetFuncs
             defaultCustomTableFuncs
  initializeTable table
  setResizable win (Just table)
  end win
  showWidget win
  _ <- FL.run
  return ()

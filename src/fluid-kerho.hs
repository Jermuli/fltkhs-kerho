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

data TableState = TableState {
  sortReverse :: IORef Bool,
  sortLastCol :: IORef Int,
  rowData :: IORef [[T.Text]]
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
                     rowData' <- readIORef (rowData tableState) >>= return . zip [(0 :: Int)..]
                     let sorted = sortBy (compare `on` (indexOr "" col'. snd)) rowData'
                     writeIORef
                       (rowData tableState)
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
    rowData' <- readIORef (rowData tableState)
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
              let currentRow = rowData' !! row'
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
--
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

--TODO: korvaa testi tekstitiedosto kunnollisella datankäsittelyllä ja tee taulukon soluista input fieldejä
main :: IO ()
main = do
  win <- makeWindow
  rows <- readFile "testi.txt" >>= return . map T.words . T.lines . T.pack
  rowData' <- newIORef rows
  sortReverse' <- newIORef False
  sortLastCol' <- newIORef (-1)
  let tableState = TableState sortReverse' sortLastCol' rowData'
  begin win
  table <- tableRowNew
             (Rectangle
               (Position (X 550) (Y 40))
               (Size (Width 275) (Height 415)))
             Nothing
             Nothing
             (drawCell tableState)
             defaultCustomWidgetFuncs
             defaultCustomTableFuncs
  setColHeader table True
  setColResize table True
  setSelectionColor table yellowColor
  setWhen table [WhenRelease]
  readIORef rowData' >>= setRows table . Rows . length
  readIORef rowData' >>= setCols table . Columns . maximum . map length
  setRowHeightAll table 18
  readIORef rowData' >>= autowidth table 20
  setColor table whiteColor
  setCallback table (eventCallback tableState)
  --setResizable win (Just table)
  end win
  showWidget win
  _ <- FL.run
  return ()

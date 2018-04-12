import GoldSeeker
import Data.List
import Data.IORef
import Data.STRef
import Graphics.UI.WX
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Char
import Data.Maybe


mainPanel f hartaRef textCtrlGridRef= do
  let prelucrareaErorilor event = do
       currentHarta <- readIORef hartaRef
       if isNothing currentHarta then
         alertPopup "Eroare!" "Încărcați harta din fișier cu \"File -> Încarcă din fișier\"."
       else
         event
  file <- menuPane [text := "&Menu"]
  readFileButton <- menuItem file [text := "&Incarcă din fișier", on command := openFileEvent f hartaRef textCtrlGridRef]
  readFileButton <- menuItem file [text := "&Salvează", on command :=prelucrareaErorilor $ saveFileEvent hartaRef textCtrlGridRef]
  exitButton <- menuQuit file [text := "Exit", on command := close f]
  menuLine file

  edit <- menuPane [text:= "&Edit"]
  addLineButton <- menuItem edit [text := "&Adaugă o linie", on command := prelucrareaErorilor $ addLineEvent f hartaRef textCtrlGridRef]
  addColumnButton <- menuItem edit [text := "&Șterge o linie", on command := prelucrareaErorilor $ removeEvent f hartaRef textCtrlGridRef True]
  addColumnButton <- menuItem edit [text := "&Adaugă o coloană", on command := prelucrareaErorilor $ addColumnEvent f hartaRef textCtrlGridRef]
  addColumnButton <- menuItem edit [text := "&Șterge o coloană", on command := prelucrareaErorilor $ removeEvent f hartaRef textCtrlGridRef False]
  menuLine edit

  evaluare <- menuPane [text := "&Exerciții"]
  patratMaximButton <- menuItem evaluare
    [
    text := "&Dreptunghi maxim"
    ,help := "Găsește dreptunghiul cu aria maximală, care are calitatea specificată pe pozițiile vârfurilor sale."
    ,on command :=prelucrareaErorilor $ patratMaximEvent f hartaRef textCtrlGridRef
    ]
  caleaDeAurButton <- menuItem evaluare
    [
    text := "&Calea de aur"
    ,help := "Calea, pe care robotul o parcurge, pentru a acumula cantitatea maximă de aur."
    ,on command := prelucrareaErorilor $ caleaDeAurEvent f hartaRef textCtrlGridRef
    ]
  primele2Calitati <- menuItem evaluare
    [
    text := "Linii cu primele 2 calități"
    ,help := "Afișează liniile, în care se află celule de primele 2 calități și înscrie în fișierul ConstCal.txt"
    ,on command := prelucrareaErorilor $ primele2CalitatiEvent hartaRef textCtrlGridRef
    ]
  menuLine evaluare

  set f [menuBar := [file, edit, evaluare]]
  return $ row 0 []

main = do
  start mainWindow
  -- testFunctionality

mainWindow :: IO()
mainWindow = do
  f <- frame [text := "Calea de aur"]
  -- (textBoxGrid, textWidgets) <- unzip . map unzip <$> makeTextBoxGrid 5 5 f
  textCtrlGridRef <- newIORef (([[]], [[]])::([[TextCtrl()]], [[TextCtrl()]]))
  hartaRef <- newIORef (Nothing::Maybe Harta)
  mainPanel f hartaRef textCtrlGridRef
  set f [size := Size 400 460]
  -- changeCellVal textWidgets $ replicate 5 [1..5]

changeCellVal :: (Show a) => [[TextCtrl()]] -> [[a]] -> IO()
changeCellVal x vals = do
  mapM_ (\(i,iVal)->
    mapM_ (\(j,val)-> do
      set j [text := show val]
      ) $ zip i iVal) $ zip x vals

readHartaFromFile :: IO Harta
readHartaFromFile = do
  res <- strToCalitate <$> readFile "calitate" <*> (strToHarta <$> readFile "input.i")
  return res

writeHartaToFile :: Harta -> IO()
writeHartaToFile harta = do
  writeFile "input.i" $ hartaToStr harta
  writeFile "calitate" $ calitateToStr harta

makeTextBoxGrid :: Int -> Int -> Frame()-> IO [[(Layout, TextCtrl())]]
makeTextBoxGrid y x f = do
 forM [0..y-1] (\ypos ->
  if x == 0 then return ([])
  else
    forM [0..x-1] (\xpos -> do
      pan <- panel f [ ]
      textBoxControl <- entry pan [clientSize := sz 20 10, size := Size 20 10]
      mapM_ (\char -> set textBoxControl [on (charKey char) := checkValidNumberEvent textBoxControl char]) (map chr [1..254])
      set textBoxControl [on command := set textBoxControl [bgcolor := blue]]
      set textBoxControl [on focus :=(\_-> checkValidNumberEvent textBoxControl '1')]
      let emptyTextBox = container pan $ shaped $ widget textBoxControl
      return (emptyTextBox, textBoxControl)
    )
  )


makeTextBoxGridLine :: Int -> Window f -> IO [(Layout, TextCtrl())]
makeTextBoxGridLine x f =
  if x == 0 then
    return []
  else do
    pan <- panel f [ ]
    textBoxControl <- entry pan [clientSize := sz 20 10, size := Size 20 10]
    mapM_ (\char -> set textBoxControl [on (charKey char) := checkValidNumberEvent textBoxControl char]) (map chr [1..254])
    set textBoxControl [on focus :=(\_-> checkValidNumberEvent textBoxControl '1')]
    let emptyTextBox = container pan $ shaped $ widget textBoxControl
    ((emptyTextBox, textBoxControl):) <$> makeTextBoxGridLine (x-1) f

checkValidNumberEvent :: TextCtrl() -> Char ->  IO()
checkValidNumberEvent control pressedChar = do
  txt <- (++ [pressedChar]) <$> get control text
  let isValid = all (`elem` ['0'..'9']) txt
  currentColor <- get control bgcolor
  if isValid then
    if currentColor == red then
      set control [bgcolor := white, color := black]
    else return ()
  else
    set control [bgcolor := red, color := black]

clearGrid :: ([[TextCtrl()]], [[TextCtrl()]]) -> IO()
clearGrid (grid1, grid2) = do
  let clr = mapM_ (mapM_ (\x -> set x [visible := False, enabled := False, size := sz 0 0]))
  clr grid1
  clr grid2

initiateGrid :: Frame() -> Harta -> IORef ([[TextCtrl()]], [[TextCtrl()]]) -> IO()
initiateGrid f harta widgetsRef = do
  let mapa = getMapa harta
  let calitate = getCalitate harta
  oldGrids <- readIORef widgetsRef
  clearGrid oldGrids
  -- pan1 <- panel f [clientSize := sz 500 300]
  -- pan2 <- panel f [clientSize := sz 500 300]
  (textBoxGrid, textWidgets) <- unzip . map unzip <$> makeTextBoxGrid (length mapa) (length $ head mapa) f
  (textBoxGrid2, textWidgets2) <- unzip . map unzip <$> makeTextBoxGrid (length calitate) (length $ head calitate) f
  changeCellVal textWidgets mapa
  changeCellVal textWidgets2 calitate
  modifyIORef' widgetsRef $ const (textWidgets, textWidgets2)
  let arrayToGrid = margin 5 . grid (-34) 3
  let gridsTogether =  marginTop $ margin 15 $ column 10 [
         boxed "Cantități" $ arrayToGrid textBoxGrid
        , boxed "Calități"  $ arrayToGrid textBoxGrid2
        ]
  set f [layout :=gridsTogether]

-- Events Start
openFileEvent :: Frame() -> IORef (Maybe Harta) -> IORef ([[TextCtrl()]], [[TextCtrl()]]) -> IO()
openFileEvent f hartaRef widgetsRef= do
  (grid1, grid2) <- readIORef widgetsRef
  -- setInvisible grid1
  -- setInvisible grid2
  newHarta <- readHartaFromFile
  harta <- readIORef hartaRef
  let updateGrid = do
        writeIORef hartaRef $ Just newHarta
        initiateGrid f newHarta widgetsRef
  if isJust harta then do
    let Just hartaEntity = harta
    unless (hartaEntity == newHarta) updateGrid
  else updateGrid


saveFileEvent :: IORef (Maybe Harta) -> IORef ([[TextCtrl()]], [[TextCtrl()]]) -> IO()
saveFileEvent hartaRef widgetsRef = do
  grids <- readIORef widgetsRef
  updateHarta hartaRef grids
  Just harta <- readIORef hartaRef
  writeHartaToFile harta

addLineEvent :: Frame() -> IORef(Maybe Harta) -> IORef ([[TextCtrl()]], [[TextCtrl()]]) -> IO()
addLineEvent f hartaRef widgetsRef = do
  Just harta <- readIORef hartaRef
  let event = saveChangesEvent hartaRef f widgetsRef saveChangesRow
  generatePopupFrame hartaRef (getM harta) widgetsRef event
  return ()

addColumnEvent :: Frame() -> IORef(Maybe Harta) -> IORef ([[TextCtrl()]], [[TextCtrl()]]) -> IO()
addColumnEvent f hartaRef widgetsRef = do
  Just harta <- readIORef hartaRef
  let event = saveChangesEvent hartaRef f widgetsRef saveChangesColumn
  generatePopupFrame hartaRef (getN harta) widgetsRef event
  return ()

removeEvent :: Frame() -> IORef(Maybe Harta) -> IORef ([[TextCtrl()]], [[TextCtrl()]]) ->
              Bool -> IO()
removeEvent f hartaRef widgetsRef isRemoveRow
  | isRemoveRow = do
     Just harta <- readIORef hartaRef
     popupForRemove f hartaRef widgetsRef (getN harta) removeMapaRow
  | otherwise = do
    Just harta <- readIORef hartaRef
    popupForRemove f hartaRef widgetsRef (getM harta) removeMapaColumn

patratMaximEvent :: Frame() -> IORef(Maybe Harta) -> IORef ([[TextCtrl()]], [[TextCtrl()]]) -> IO()
patratMaximEvent f hartaRef textCtrlGridRef= do
  readIORef textCtrlGridRef >>= updateHarta hartaRef
  -- Just harta <- readIORef hartaRef
  -- initiateGrid f harta textCtrlGridRef
  popupFrame  <- frame [text := "Dreptunghi maximal", clientSize := sz 400 400]
  let clearBackgroundsLocal = clearBackgrounds textCtrlGridRef
  calitateSelect <- comboBox popupFrame [items := map show [1..4], selection := 0]
  inapoiButton <- button popupFrame [text := "Înapoi",
    on command := do
      clearBackgroundsLocal
      close popupFrame
    ]
  text1 <- staticText popupFrame [text := ""]
  text2 <- staticText popupFrame [text := ""]
  let creazaMesaj mes1 mes2 = do
        set text1 [text := mes1]
        set text2 [text := mes2]
  afiseazaButton <- button popupFrame [text := "Afișează"
          ,on command := do
            clearBackgroundsLocal
            readIORef textCtrlGridRef >>= updateHarta hartaRef
            Just currentHarta <- readIORef hartaRef
            calitateSelectata <- (+1) <$> get calitateSelect selection
            let (aria, dreptunghiul) = patratMaxim calitateSelectata currentHarta
            if isNothing dreptunghiul then
                creazaMesaj "N-a fost găsit nici un dreptunghi" ""
            else do
                let Just dreptStr = show <$> dreptunghiul
                creazaMesaj ("Vârfuri: "++dreptStr) $ "Aria: " ++ show aria
                let Just dreptDeFacto = dreptunghiul
                readIORef textCtrlGridRef >>= colorCells dreptDeFacto yellow
        ]
  set popupFrame [layout :=margin 35 $ column 15 [
    alignCenter $ row 4 [marginTop $ margin 5 $ label "Calitate:", widget calitateSelect],
    widget text1,
    widget text2,
    row 15 [widget afiseazaButton, widget inapoiButton]
    ]]


caleaDeAurEvent :: Frame() -> IORef(Maybe Harta) -> IORef ([[TextCtrl()]], [[TextCtrl()]]) -> IO()
caleaDeAurEvent f hartaRef widgetsRef = do
  let clearBackgroundsLocal = clearBackgrounds widgetsRef
  readIORef widgetsRef >>= updateHarta hartaRef
  clearBackgroundsLocal
  popup <- frame [text := "Calea de aur"]
  okButton <- button popup [
    text := "Înapoi"
    ,on command :=  do
      clearBackgroundsLocal
      close popup
    ]
  Just harta <- readIORef hartaRef
  let (nr, pasi) = optimalPath harta
  let pasiStr = unlines $ map (\(y,x) -> show (x,y)) pasi
  text1 <- staticText popup [text := ("Aur: " ++ show nr ++ " u.c.")]
  text2 <- staticText popup [text := ("Pași: \n" ++ pasiStr)]
  set popup [layout := margin 10 $ column 10 [
    alignCenter $ widget text1
    ,alignCenter $ widget text2
    ,alignCenter $ widget okButton]]
  readIORef widgetsRef >>= colorCells pasi (colorRGB 255 192 0)

primele2CalitatiEvent :: IORef(Maybe Harta) -> IORef ([[TextCtrl()]], [[TextCtrl()]]) -> IO()
primele2CalitatiEvent hartaRef widgetsRef = do
  clearBackgrounds widgetsRef
  readIORef widgetsRef >>= updateHarta hartaRef
  popup <- frame [text := "Linii cu primele 2 calități"]
  Just harta <- readIORef hartaRef
  let resStr = liniiCuPrimele2Calitati harta
  mesaj <- staticText popup [text := unlines (tail $ lines resStr)]
  let rowsToDraw = map fst $ filter (\(_, x) ->any (`elem` [1,2]) x) $ zip [1..getN harta] $ getCalitate harta
  readIORef widgetsRef >>= colorCells (concatMap (flip zip [1..getM harta] . repeat) rowsToDraw) magenta
  writeButton <- button popup
    [
    text := "Scrie în fișier"
    ,on command :=
      if length resStr > 1 then do
        writeFile "ConstCal.txt" resStr
        alertPopup "Succes!" "Fișierul a fost scris cu succes!"
      else
        alertPopup "Fail" "Nu au fost găsite linii,\n care să conțină calitățile 1 și 2"
    ]
  inapoiButton <- button popup [
    text := "Înapoi"
    ,on command :=  do
      clearBackgrounds widgetsRef
      close popup
    ]
  set popup [layout := margin 25 $ column 15
    [
    alignCenter $ widget mesaj,
    alignCenter $ row 15 [widget writeButton, widget inapoiButton]
    ]]
-- Events End

--Popup windows
saveChangesEvent :: IORef (Maybe Harta) -> Frame() -> IORef ([[TextCtrl()]], [[TextCtrl()]]) ->
                    (IORef(Maybe Harta) -> Int -> String -> String -> IO()) ->
                    Frame() -> ComboBox() -> ([TextCtrl()], [TextCtrl()]) -> IO()
saveChangesEvent hartaRef mainFrame textCtrlRef
                 transformFunction
                 popupFrame positionControl (cantitateLine, calitateLine)   = do
                   pos <- get positionControl selection
                   cantitateStr <- textBoxArrayToString cantitateLine
                   calitateStr <- textBoxArrayToString calitateLine
                   (textBoxGrid, textBoxGrid2) <- readIORef textCtrlRef
                   updateHarta hartaRef (textBoxGrid, textBoxGrid2)
                   transformFunction hartaRef pos calitateStr cantitateStr
                   close popupFrame
                   Just harta <- readIORef hartaRef
                   initiateGrid mainFrame harta textCtrlRef

saveChangesRow :: IORef(Maybe Harta) -> Int -> String -> String -> IO()
saveChangesRow hartaRef pos calStr cantStr = do
  Just harta <- readIORef hartaRef
  let newHarta = addRowToHarta pos (addRowToCalitate pos harta calStr) cantStr
  modifyIORef' hartaRef $ const $ Just newHarta

saveChangesColumn :: IORef(Maybe Harta) -> Int -> String -> String -> IO()
saveChangesColumn hartaRef pos calStr cantStr = do
  Just harta <- readIORef hartaRef
  let newHarta = addColumnToHarta pos (addColumnToCalitate pos harta calStr) cantStr
  modifyIORef' hartaRef $ const $ Just newHarta

generatePopupFrame :: IORef(Maybe Harta) -> Int -> IORef ([[TextCtrl()]], [[TextCtrl()]]) -> (Frame() -> ComboBox() -> ([TextCtrl()], [TextCtrl()]) -> IO()) -> IO()
generatePopupFrame hartaRef boxCount widgetsRef event= do
  Just harta <- readIORef hartaRef
  let posCount
        | getN harta /= boxCount = getN harta
        | otherwise = getM harta
  popupFrame <- frame [text := "Introduceți conținutul"]
  (grid1, [textBoxesCantitate]) <- unzip . map unzip <$> makeTextBoxGrid 1 boxCount popupFrame
  (grid2, [textBoxesCalitate]) <- unzip . map unzip <$> makeTextBoxGrid 1 boxCount popupFrame
  closeButton <- button popupFrame [text := "Close", on command := close popupFrame]
  okButton <- button popupFrame [text := "Save"]
  positionCtrl <- comboBox popupFrame [items := map show [1..posCount], selection := 0]
  let mainGrid = margin 20 (grid 0 5 $ map (:[]) [
        label "Cantitate",
        grid (-34) 0 grid1,
        label "Calitate",
        grid (-34) 0 grid2,
        label "Poziția",
        widget positionCtrl,
        row 15 [widget closeButton,widget okButton]
        ])
  set popupFrame [layout := mainGrid]
  set okButton [on command := event popupFrame positionCtrl (textBoxesCantitate, textBoxesCalitate)]

popupForRemove :: Frame() -> IORef(Maybe Harta) -> IORef ([[TextCtrl()]], [[TextCtrl()]]) ->
                      Int -> (Int -> Harta -> Harta) -> IO()
popupForRemove f hartaRef widgetsRef numberOfElements transformator = do
  readIORef widgetsRef >>= updateHarta hartaRef
  popup <- frame [text := "Eliminarea liniei/coloanei"]
  selectNumberBox <- comboBox popup [items := map show [1..numberOfElements]]
  closeButton <- button popup [text := "Close", on command := close popup]
  removeButton <- button popup [
    text := "Șterge",
    on command := do
      nr <- (+1) <$> get selectNumberBox selection
      modifyIORef' hartaRef (\harta -> transformator nr <$> harta)
      Just harta <- readIORef hartaRef
      initiateGrid f harta widgetsRef
      close popup
    ]
  set popup [layout :=
    margin 20 $ column 15 [
      alignCenter $ row 10 [marginTop $ margin 5 $ label "Selectează", widget selectNumberBox],
      row 10 [widget removeButton, widget closeButton]]
    ]

alertPopup :: String -> String -> IO()
alertPopup title message = do
  popup <- frame [text := title]
  msgControl <- staticText popup [text := message]
  closeButton <- button popup [text := "OK", on command := close popup]
  set popup [layout := margin 20 $ column 15 [alignCenter $ widget msgControl, alignCenter $ widget closeButton]]
--Popup windwos end

--helper
textBoxArrayToString :: [TextCtrl()] -> IO String
textBoxArrayToString arr = intercalate " " <$> mapM (`get` text) arr

gridsToHarta :: ([[TextCtrl()]], [[TextCtrl()]]) -> IO(Maybe Harta)
gridsToHarta (grid1, grid2) = do
  let xy = map show [length grid1, length $ head grid1]
  let getValsFromControls = mapM (mapM (`get` text))
  calitate <- getValsFromControls grid2
  cantitate <- getValsFromControls grid1
  let isValid = not . any (any ( any (`notElem` ['0'..'9'])))
  if isValid calitate && isValid cantitate then
      let arrToStr = unlines . map unwords
          calitateStr = arrToStr calitate
          cantitateStr = arrToStr (xy:cantitate) in
      return $ Just $ strToCalitate calitateStr $ strToHarta cantitateStr
   else
      return Nothing

updateHarta :: IORef (Maybe Harta) ->  ([[TextCtrl()]], [[TextCtrl()]]) -> IO()
updateHarta hartaRef grids = do
  newHarta <- gridsToHarta grids
  unless (isNothing newHarta) $ writeIORef hartaRef newHarta

colorCells :: [(Int, Int)] -> Color -> ([[TextCtrl()]], [[TextCtrl()]]) -> IO()
colorCells positions chosenColor (grid1, grid2) =
  forM_ positions (\(y,x)-> do
    set (grid1 !! (y-1) !! (x-1)) [bgcolor := chosenColor]
    set (grid2 !! (y-1) !! (x-1)) [bgcolor := chosenColor]
    )

makeWhiteBackground = mapM_ (mapM_ (`set` [bgcolor := white]))

clearBackgrounds widgetsRef = do
  (grid1, grid2) <- readIORef widgetsRef
  makeWhiteBackground grid1
  makeWhiteBackground grid2
--helper

testFunctionality :: IO()
testFunctionality = do
  harta <- readHartaFromFile
  putStrLn $ hartaToStr $ harta
  putStrLn "RemoveMapaRow:"
  putStrLn $ hartaToStr $ removeMapaRow 1 harta
  putStrLn "addColumnToHarta:"
  putStrLn $ hartaToStr $ addColumnToHarta 1 harta "1 1 1 1"
  putStrLn "addRowToHarta:"
  putStrLn $ hartaToStr $ addRowToHarta 1 harta "1 1 1 1"
  putStrLn "removeMap:"
  putStrLn $ hartaToStr $ removeMapaColumn 1 harta
  putStrLn "maximLocal:"
  putStrLn $ show $ maximLocal harta
  putStrLn "getMediiDisponibileDupaCalitate:"
  putStrLn $ show $ getMediiDisponibileDupaCalitate harta
  putStrLn "getNumarZoneDupaCalitate:"
  putStrLn $ show $ getNumarZoneDupaCalitate harta
  putStrLn "liniiCuPrimele2Calitati:"
  putStrLn $ liniiCuPrimele2Calitati harta
  putStrLn $ show $ patratMaxim 2 harta
  putStrLn "optimalPath"
  putStrLn $ show $ optimalPath harta

module GoldSeeker(
  Harta,
  strToHarta,
  hartaToStr,
  addRowToHarta,
  addColumnToHarta,
  removeMapaRow,
  removeMapaColumn,
  maximLocal,
  strToCalitate,
  calitateToStr,
  getMediiDisponibileDupaCalitate,
  getNumarZoneDupaCalitate,
  liniiCuPrimele2Calitati,
  patratMaxim,
  optimalPath,
  getCalitate,
  getMapa,
  setCalitate,
  setMapa,
  getN,
  getM,
  addRowToCalitate,
  addColumnToCalitate
) where
import Data.List

type Point = (Int, Int)
type Patrat = [Point]

data Harta = Harta {
  mapa :: [[Int]],
  n :: Int,
  m :: Int,
  calitate :: [[Int]]
} deriving (Show, Eq)

getN :: Harta -> Int
getN Harta {n=n} = n

getM :: Harta -> Int
getM Harta {m=m} = m

getCalitate :: Harta -> [[Int]]
getCalitate Harta {calitate = calitate} = calitate

getMapa :: Harta -> [[Int]]
getMapa Harta {mapa = mapa} = mapa

setCalitate :: Harta -> [[Int]] -> Harta
setCalitate Harta {mapa = mapa, n = n, m=m} newCal=
  Harta { mapa = mapa, n = n, m = m, calitate = newCal}

setMapa :: Harta -> [[Int]] -> Harta
setMapa Harta {calitate = calitate, n = n, m=m} newMap=
  Harta { mapa = newMap, n = n, m = m, calitate = calitate}

readInt :: String -> Int
readInt = read

pairToArray :: ([a], [a]) -> [[a]]
pairToArray (a, b) = [a, b]

getRow :: Int -> [a] -> a
getRow nr = last . take nr

getElement :: Int -> Int -> [[Int]] -> Int
getElement x y arr = let
  searchedLine = getRow y arr
  element = last $ fst $ splitAt x searchedLine
  in element

aranjamente :: Int -> Int -> [[Int]]
aranjamente n m
  | m > n = error "m > n"
  | otherwise = aranjImpl n m m
      where
        aranjImpl :: Int -> Int -> Int -> [[Int]]
        aranjImpl n m count
          | m == 0 = [[1..n]]
          | count == 1 = [[x] | x<-[1..n]]
          | otherwise = [h:x | x <-(aranjImpl n m (count-1)) , h <- [1..n] ,  h `notElem` x  ]

strToHarta :: String -> Harta
strToHarta s = let
 lns = lines s
 firstLine = head lns
 stringRows = tail lns
 [rowsNr, colsNr] = map readInt $ words firstLine
 rows = foldr (\line acc-> map readInt (words line) : acc) [] stringRows
 in(
 if (rowsNr == length rows) && ((colsNr==0 && rowsNr==0) ||
    (rowsNr == length (filter (\x -> colsNr == length x) rows))) then
   Harta {
   mapa = rows,
   n = rowsNr,
   m = colsNr,
   calitate = []
   }
    else
      error "Numere n si m nu coincid cu numarul de coloane sau linii"
 )

hartaToStr :: Harta -> String
hartaToStr Harta { mapa = mapa, n = n, m = m} = let
  firstLine = show n ++ " " ++ show m
  rowLines = unlines $ foldr (\row acc -> unwords (map show row) : acc) [] mapa
  in firstLine ++ "\n" ++ rowLines

addRow :: Int -> [[Int]] -> String -> [[Int]]
addRow nr mapa newRow = let
  rowToInts = map readInt (words newRow)
  newMap = intercalate [rowToInts] $ pairToArray $ splitAt nr mapa
  in newMap

addRowToCalitate :: Int -> Harta -> String -> Harta
addRowToCalitate nr Harta { mapa = mapa, n = n, m = m, calitate = calitate} newRow =
   Harta { mapa = mapa, calitate = addRow nr calitate newRow, n=n, m=m}

addColumnToCalitate :: Int -> Harta -> String -> Harta
addColumnToCalitate nr Harta { mapa = mapa, n = n, m = m, calitate = calitate } newColumn = let
 newIntsColumn = map readInt (words newColumn)
 rowWithInsertedVal = zip newIntsColumn calitate
 newMap = foldr (\(val, row) acc ->intercalate [val] (pairToArray $ splitAt nr row) : acc) [] rowWithInsertedVal
 in Harta { mapa = mapa, n = n, m = m, calitate = newMap}

addRowToHarta :: Int -> Harta -> String -> Harta
addRowToHarta nr Harta { mapa = mapa, n = n, m = m, calitate = calitate} newRow =
   Harta { mapa = addRow nr mapa newRow, n = n+1, m = m, calitate = calitate}

addColumnToHarta :: Int -> Harta -> String -> Harta
addColumnToHarta nr Harta { mapa = mapa, n = n, m = m, calitate = calitate } newColumn = let
  newIntsColumn = map readInt (words newColumn)
  rowWithInsertedVal = zip newIntsColumn mapa
  newMap = foldr (\(val, row) acc ->intercalate [val] (pairToArray $ splitAt nr row) : acc) [] rowWithInsertedVal
  in Harta { mapa = newMap, n = n, m = m + 1, calitate = calitate}

removeRow ::(Eq a) => Int -> [[a]] -> [[a]]
removeRow nr mapa = let
  n = length mapa
  m = length $ head mapa
  newMap = filter (/=[]) $ zipWith (\rowNr row -> if nr /= rowNr then row else []) [1..] mapa
  in newMap

removeMapaRow :: Int -> Harta -> Harta
removeMapaRow nr Harta {mapa = mapa, n = n, m = m, calitate = calitate} =
   Harta { mapa = removeRow nr mapa, n = n - 1, m = m, calitate = removeRow nr calitate}

removeColumn :: Int -> [[a]] -> [[a]]
removeColumn nr  mapa = let
  n = length mapa
  m = length $ head mapa
  numbered = zip [1..]
  exclude = foldr (\(pos, el) acc -> if pos /= nr then el:acc else acc) [] . numbered
  newMap = foldr (\row acc -> exclude row : acc) [] mapa
  in newMap

removeMapaColumn :: Int -> Harta -> Harta
removeMapaColumn nr Harta { mapa = mapa, n = n, m = m, calitate = calitate} =
   Harta { mapa = removeColumn nr mapa, n = n, m = m-1, calitate = removeColumn nr calitate}

maximLocal :: Harta -> [(Int, Int)]
maximLocal Harta { mapa = mapa, n = n, m = m} = finding 2 2
  where
    finding x y
      | (x == m-1) && (y == n-1) = if compareToNeighbours then [(x,y)] else []
      | (x == m-1) = if compareToNeighbours then (x,y) : finding 2 (y+1) else finding 2 (y+1)
      | (x < m-1) = if compareToNeighbours then (x,y) : finding (x+1) y else finding (x+1) y
      | (y <= n-1) = if compareToNeighbours then (x,y) : finding x (y+1) else finding x (y+1)
        where
        getIfMaximum = if compareToNeighbours then Just (x, y) else Nothing
        compareToNeighbours = center > maximum [top, bottom, left, right, leftTop, rightTop, leftBottom, rightBottom]
        center = getElement x y mapa
        top = getElement x (y-1) mapa
        bottom = getElement x (y+1) mapa
        left = getElement (x-1) y mapa
        right = getElement (x+1) y mapa
        leftTop = getElement (x-1) (y-1) mapa
        rightTop = getElement (x+1) (y-1) mapa
        leftBottom = getElement (x-1) (y+1) mapa
        rightBottom = getElement (x+1) (y+1) mapa

strToCalitate :: String -> Harta -> Harta
strToCalitate str Harta { mapa = mapa, n = n, m = m, calitate = calitate} = let
  rows = map (map readInt . words) $ lines str
  in Harta { mapa = mapa, n = n, m = m, calitate = rows}

calitateToStr :: Harta -> String
calitateToStr Harta { calitate = calitate} = let
  strRow = intercalate " " . map show
  result = unlines $ map strRow calitate
  in result

getMediiDisponibileDupaCalitate :: Harta -> [(Int, Int)]
getMediiDisponibileDupaCalitate Harta { calitate = calitate, m = m, n = n, mapa = mapa } = let
  initialList = [(i, 0) | i <- [1..4]]
  zipTogether = concat $ zipWith (\rowQual rowQuant -> zip rowQual rowQuant) calitate mapa
  getQualityQuantity quality =sum $ foldr (\(qual, quant) acc-> quant : acc) [] $ filter (\(qual, _) -> qual == quality) zipTogether
  in sortBy (\(_, a) (_, b) -> if a<=b then GT else LT) $ zip [1..4] $ map getQualityQuantity [1..4]

getNumarZoneDupaCalitate :: Harta -> [(Int, Int)]
getNumarZoneDupaCalitate Harta {calitate = calitate, m = m, n = n, mapa = mapa } = let
  getQualityQuantity quality =length  $ filter (\qual -> qual == quality) $ concat calitate
  in sortBy (\(_, a) (_, b) -> if a<=b then LT else GT) $ zip [1..4] $ map getQualityQuantity [1..4]

liniiCuPrimele2Calitati :: Harta -> String
liniiCuPrimele2Calitati Harta { calitate = calitate, m = m, n = n, mapa = mapa } = let
  zippedCalitate = zip [1..] calitate
  containsCalitati = foldr (\x acc -> x `elem` [1, 2] || acc) False
  choosedRowsIndexes = foldr (\(index, x) acc-> if containsCalitati x then index : acc else acc) [] zippedCalitate
  choosedRows = foldr (\x acc -> getRow x mapa : acc) [] choosedRowsIndexes
  in hartaToStr $ Harta{
  mapa = choosedRows,
  calitate = calitate,
  n = length choosedRows,
  m = m
  }

calcArie :: [(Int, Int)] -> Int
calcArie [(ay, ax), (by, bx), (cy, cx), (dy, dx)]
 | ay == cy && ax == bx && cx == dx && dy == by = (dy - ay + 1) * (dx - ax + 1)
 | otherwise = 0
calcArie arr = error $ "Wrong number of vertexes: " ++ show (length arr) ++ " (should be 4)"

patratMaxim :: Int -> Harta -> (Int, Maybe Patrat)
patratMaxim searchedQuality Harta{calitate = calitate, n = n, m = m}
  | (n == 0 || m == 0) = (0, Nothing)
  | otherwise = (aria, biggestPatrat)
    where
      coords = concat [[(y, x) | x <- [1..m]] |  y <- [1..n]]
      zipWithCoords = zip coords $ concat calitate
      potentialVertexes = map fst $ filter (\((_, _), q) -> q == searchedQuality) zipWithCoords
      positionsToVertexes = foldr (\val acc -> getRow val potentialVertexes : acc) []
      vertexesCombinations = map positionsToVertexes $ aranjamente (length potentialVertexes) 4
      isBiggest el (currentBiggestAria, currentBiggestPatrat) =
         if (calcArie el) > currentBiggestAria then currentElement else  (currentBiggestAria, currentBiggestPatrat)
           where
              currentElement = (calcArie el,Just el)
      (aria, biggestPatrat) = foldr isBiggest (0, Nothing) vertexesCombinations

optimalPath :: Harta -> (Int, [Point])
optimalPath Harta { mapa = mapa, n = n, m = m} = let
  coords = concat [[(y, x) | x <- [1..m]] |  y <- [1..n]]
  zipWithCoords = zip coords $ concat mapa
  getVal pos = value
    where
      Just (_, value) = if calc == Nothing then error $ "Inexistent position: " ++ show pos else calc
      calc = find (\(yx, _) -> pos == yx ) zipWithCoords
  pathToGold x y
    |  y == n && x == m = [currentState]
    |  x == m = currentState: pathToGold x (y+1)
    |  y == n = currentState: pathToGold (x+1) y
    |  sumPath (pathToGold (x+1) y) > sumPath (pathToGold x (y+1)) = currentState: pathToGold (x+1) y
    |  otherwise = currentState : pathToGold x (y+1)
      where
        currentState = ((y, x), getVal (y,x))
  sumPath = foldl (\acc (_, x)-> acc+x) 0
  resultPath = pathToGold 1 1
  in (sumPath resultPath, map fst resultPath)

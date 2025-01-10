module MinesweeperLogic
    ( CellContent(..)
    , DisplayState(..)
    , Cell(..)
    , Board(..)
    , GameResult(..)
    , initBoard
    , revealCell
    , toggleFlag
    , checkWin
    , isMine
    , isRevealed
    , autoRevealSafe
    , revealAllMines
    ) where

----------------------------------------------------------------------------
-- MinesweeperLogic
--
--  if we find a cell that must be safe, we pick it. otherwise random guess
----------------------------------------------------------------------------

import System.Random
import Control.Monad (forM_)
import Data.Maybe (isJust, fromJust)
import Data.List  (nub)

----------------------------------------------------------------------------
-- data structures
----------------------------------------------------------------------------

data CellContent
    = Mine            -- cell that holds a mine
    | Safe Int        -- safe with adjacency count
    deriving (Eq, Show)

data DisplayState
    = Hidden
    | Revealed
    | Flagged
    deriving (Eq, Show)

data Cell = Cell
    { content       :: CellContent
    , displayStatus :: DisplayState
    } deriving (Eq, Show)

data Board = Board
    { width  :: Int
    , height :: Int
    , cells  :: [[Cell]]
    } deriving (Eq, Show)

data GameResult
    = InProgress
    | Lost
    | Won
    deriving (Eq, Show)

----------------------------------------------------------------------------
-- utilities
----------------------------------------------------------------------------

-- safeIndex2D checks if (r,c) is in range for the 2D list
safeIndex2D :: Int -> Int -> [[a]] -> Maybe a
safeIndex2D r c grid =
    if r >= 0 && r < length grid && c >= 0 && c < length (head grid)
       then Just (grid !! r !! c)
       else Nothing

-- replaceCell2D modifies one position (r,c) in the 2D list with newVal
replaceCell2D :: Int -> Int -> a -> [[a]] -> [[a]]
replaceCell2D r c newVal old2D =
    [ if rowIndex == r
         then replace1D c newVal oldRow
         else oldRow
    | (rowIndex, oldRow) <- zip [0..] old2D
    ]
  where
    replace1D idx val row =
      [ if colIndex == idx then val else elemVal
      | (colIndex, elemVal) <- zip [0..] row ]

----------------------------------------------------------------------------
-- Board creation
----------------------------------------------------------------------------

initBoard :: Int -> Int -> Int -> IO Board
initBoard w h mineCount = do
    let totalCells = w * h
    minePositions <- pickUniquePositions totalCells mineCount
    let initialCells =
          [ [ createCell r c minePositions w
            | c <- [0..(w-1)] ]
          | r <- [0..(h-1)] ]
    let newCells = fillAdjacents initialCells w h
    pure (Board w h newCells)

pickUniquePositions :: Int -> Int -> IO [Int]
pickUniquePositions total count = do
    genList <- shuffle [0..(total-1)]
    return $ take count genList

-- fisher-yates shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
    arr <- go (length xs - 1) (return xs)
    pure arr
  where
    go 0 lstIO = lstIO
    go i lstIO = do
      lst <- lstIO
      j   <- randomRIO (0, i)
      let swapped = swap i j lst
      go (i - 1) (return swapped)

    swap i j lst
      | i == j    = lst
      | otherwise =
          let vi = lst !! i
              vj = lst !! j
          in  replaceOne i vj (replaceOne j vi lst)

    replaceOne idx val old =
       [ if k == idx then val else e
       | (k,e) <- zip [0..] old ]

createCell :: Int -> Int -> [Int] -> Int -> Cell
createCell r c minePositions boardWidth =
    let index1D = r * boardWidth + c
    in if index1D `elem` minePositions
          then Cell Mine Hidden
          else Cell (Safe 0) Hidden

fillAdjacents :: [[Cell]] -> Int -> Int -> [[Cell]]
fillAdjacents grid w h =
    [ [ let oldCell = grid !! row !! col
        in case content oldCell of
             Mine      -> oldCell
             Safe _    ->
               let adj = countMines row col grid
               in oldCell { content = Safe adj }
      | col <- [0..(w-1)] ]
    | row <- [0..(h-1)] ]

-- count how many mines in the neighbours
countMines :: Int -> Int -> [[Cell]] -> Int
countMines r c grid =
    length
      [ ()
      | rr <- [r-1 .. r+1]
      , cc <- [c-1 .. c+1]
      , not (rr == r && cc == c)
      , isJust (safeIndex2D rr cc grid)
      , let Just neighbour = safeIndex2D rr cc grid
      , content neighbour == Mine
      ]

----------------------------------------------------------------------------
-- game operations
----------------------------------------------------------------------------

-- revealCell tries to reveal (r,c).
revealCell :: Board -> (Int, Int) -> (Board, GameResult)
revealCell board (r,c) =
  case safeIndex2D r c (cells board) of
    Nothing -> (board, InProgress)  -- out-of-bounds, do nothing
    Just cell ->
      case (content cell, displayStatus cell) of
        (Mine, Hidden)   -> (revealAllMines board, Lost)
        (Safe _, Hidden) -> revealAlgorithm board (r,c)
        _                -> (board, InProgress)

-- toggleFlag flips from hidden -> flagged or flagged -> hidden
toggleFlag :: Board -> (Int, Int) -> Board
toggleFlag brd (r,c) =
   let cgrid = cells brd
   in case safeIndex2D r c cgrid of
        Nothing -> brd
        Just oldCell ->
          case displayStatus oldCell of
            Hidden  -> brd { cells = replaceCell2D r c (oldCell{displayStatus=Flagged}) cgrid }
            Flagged -> brd { cells = replaceCell2D r c (oldCell{displayStatus=Hidden}) cgrid }
            _       -> brd

checkWin :: Board -> GameResult
checkWin brd =
  let cgrid = cells brd
      allCells = concat cgrid
      hiddenNonMines =
        [ x
        | x <- allCells
        , content x /= Mine
        , displayStatus x /= Revealed
        ]
  in if null hiddenNonMines
       then Won
       else InProgress

----------------------------------------------------------------------------
-- revealing logic
----------------------------------------------------------------------------

revealAlgorithm :: Board -> (Int, Int) -> (Board, GameResult)
revealAlgorithm brd (r,c) =
    let cgrid      = cells brd
        oldCell    = fromJust (safeIndex2D r c cgrid)
        updatedCell= oldCell { displayStatus = Revealed }
        cgrid2     = replaceCell2D r c updatedCell cgrid
        brd2       = brd { cells = cgrid2 }
        isZeroCell = case content updatedCell of
                       Safe n -> n == 0
                       _      -> False
    in if not isZeroCell
         then (brd2, InProgress)
         else floodNeighbors brd2 (r,c)

floodNeighbors :: Board -> (Int, Int) -> (Board, GameResult)
floodNeighbors brd (r,c) =
  let neighbours =
        [ (nr, nc)
        | nr <- [r-1..r+1]
        , nc <- [c-1..c+1]
        , not (nr == r && nc == c)
        ]
  in foldl f (brd, InProgress) neighbours
  where
    f (theBoard, _) (nr, nc) =
       let (nextBoard, _) = revealCell theBoard (nr, nc)
       in (nextBoard, InProgress)

-- revealAllMines is used when you lose
revealAllMines :: Board -> Board
revealAllMines brd =
   let cgrid = cells brd
       newGrid =
         [ [ if content cell == Mine
               then cell { displayStatus = Revealed }
               else cell
             | cell <- row
             ]
         | row <- cgrid
         ]
   in brd { cells = newGrid }

----------------------------------------------------------------------------
-- auto-player
----------------------------------------------------------------------------

-- here we do a little check for guaranteed safe squares:
--  if a revealed cell has adjacency number n, and it already has f flags around,
--  then if n == f, the rest of hidden squares around are safe
-- we collect all those safe squares. if none are found, random guess
autoRevealSafe :: Board -> IO (Board, GameResult)
autoRevealSafe brd =
  case findGuaranteedSafe brd of
    (s:_) -> do
       -- reveal the first guaranteed safe we found
       let (newB, res) = revealCell brd s
       return (newB, res)
    [] -> do
       -- no guaranteed safes => pick random hidden cell
       let hiddenList =
             [ (r,c)
             | r <- [0..(height brd - 1)]
             , c <- [0..(width brd - 1)]
             , let cl = fromJust (safeIndex2D r c (cells brd))
             , displayStatus cl == Hidden
             ]
       if null hiddenList
         then pure (brd, InProgress)
         else do
           i <- randomRIO (0, length hiddenList - 1)
           let choice       = hiddenList !! i
               (newB, res)  = revealCell brd choice
           pure (newB, res)

-- find guaranteed safe squares. if a revealed cell says it's n, and 
-- there's f flagged neighbours, and there are h hidden neighbours,
-- if n==f, then all hidden neighbours are safe.
findGuaranteedSafe :: Board -> [(Int,Int)]
findGuaranteedSafe brd =
  let w = width brd
      h = height brd
      cgrid = cells brd
  in concat
     [ hiddenNeighbours (r,c) cgrid
     | r <- [0..(h-1)], c <- [0..(w-1)]
     , let cellHere = cgrid !! r !! c
     , displayStatus cellHere == Revealed
     , case content cellHere of
         Safe adjCount ->
           let fs = countFlagsAround (r,c) brd
               hs = countHidden (r,c) brd
           in adjCount == fs && hs > 0  -- we found a square that indicates hidden are all safe
         _ -> False
     ]
  where
    -- gather all hidden neighbours
    hiddenNeighbours (r,c) allCells =
      [ (nr, nc)
      | nr <- [r-1 .. r+1]
      , nc <- [c-1 .. c+1]
      , not (nr == r && nc == c)
      , nr>=0, nr<height brd, nc>=0, nc<width brd
      , let cc = allCells !! nr !! nc
      , displayStatus cc == Hidden
      ]

-- count how many are flagged around
countFlagsAround :: (Int,Int) -> Board -> Int
countFlagsAround (r,c) (Board w h cgrid) =
  length
    [ ()
    | rr <- [r-1..r+1]
    , cc <- [c-1..c+1]
    , not (rr == r && cc == c)
    , rr>=0, rr<h, cc>=0, cc<w
    , let Cell _ d = cgrid !! rr !! cc
    , d == Flagged
    ]

-- count hidden around
countHidden :: (Int,Int) -> Board -> Int
countHidden (r,c) (Board w h cgrid) =
  length
    [ ()
    | rr <- [r-1..r+1]
    , cc <- [c-1..c+1]
    , not (rr == r && cc == c)
    , rr>=0, rr<h, cc>=0, cc<w
    , let Cell _ d = cgrid !! rr !! cc
    , d == Hidden
    ]

isMine :: Cell -> Bool
isMine c = content c == Mine

isRevealed :: Cell -> Bool
isRevealed c = displayStatus c == Revealed

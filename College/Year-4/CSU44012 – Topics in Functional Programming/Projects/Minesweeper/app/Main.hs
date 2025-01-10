{-# LANGUAGE OverloadedStrings #-}

module Main where

-------------------------------------------------------------------------------
-- Minesweeper-o-Matic
--
-- this file sets up the threepenny gui for minesweeper. 
-- we can start new games, switch between reveal and flag modes, 
-- and we also have a play moves button that does an automatic move 
-- (it tries a quick check for obviously safe squares, if none found then guesses).
-------------------------------------------------------------------------------

import Prelude
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import MinesweeperLogic

import Data.IORef
import Control.Monad (forM_, void)
import System.Random (randomRIO)
import Data.Maybe (fromJust)

--------------------------------------------------------------------------------
-- main sets up the server config and starts the gui
--------------------------------------------------------------------------------

main :: IO ()
main = do
    let config = defaultConfig
          { jsPort = Just 10000
          , jsAddr = Just "127.0.0.1"
          }
    startGUI config setup

--------------------------------------------------------------------------------
-- setup the gui
--------------------------------------------------------------------------------

setup :: Window -> UI ()
setup window = do
    void $ return window # set title "csu44012 minesweeper-o-matic"

    widthInput  <- UI.input # set UI.type_ "number" # set UI.value "10"
    heightInput <- UI.input # set UI.type_ "number" # set UI.value "10"
    minesInput  <- UI.input # set UI.type_ "number" # set UI.value "15"

    newGameBtn <- UI.button #+ [string "New Game"]
    sweepBtn   <- UI.button #+ [string "Reveal Mode"]
    flagBtn    <- UI.button #+ [string "Flag Mode"]
    autoBtn    <- UI.button #+ [string "Play Moves"]

    messageDiv <- UI.div
    boardDiv   <- UI.div #. "boardDiv"

    void $ getBody window #+ 
      [ row [string "Width: ", element widthInput]
      , row [string "Height:", element heightInput]
      , row [string "Mines: ", element minesInput]
      , row [element newGameBtn, element sweepBtn, element flagBtn, element autoBtn]
      , element messageDiv
      , element boardDiv
      ]

    -- store (Board, "reveal"/"flag", gameOverBool)
    gameRef <- liftIO $ newIORef (undefined :: Board, "reveal", False)

    let setMode newMode = do
          (bd, _oldMode, gOver) <- liftIO $ readIORef gameRef
          if gOver
            then void $ element messageDiv # set text "game is already over."
            else do
              liftIO $ writeIORef gameRef (bd, newMode, gOver)
              void $ element messageDiv # set text ("mode set to: " ++ newMode)

    on UI.click sweepBtn $ \_ -> setMode "reveal"
    on UI.click flagBtn  $ \_ -> setMode "flag"

    on UI.click newGameBtn $ \_ -> do
        mw <- getValue widthInput
        mh <- getValue heightInput
        mm <- getValue minesInput
        let wVal = read (maybe "10" id mw) :: Int
            hVal = read (maybe "10" id mh) :: Int
            mVal = read (maybe "15" id mm) :: Int

        board0 <- liftIO $ initBoard wVal hVal mVal
        liftIO $ writeIORef gameRef (board0, "reveal", False)
        void $ element messageDiv # set text "new game started."
        drawBoard boardDiv messageDiv gameRef

    -- hooking up the play moves (auto) button
    on UI.click autoBtn $ \_ -> do
        (board0, mode0, gOver) <- liftIO $ readIORef gameRef
        if gOver
          then void $ element messageDiv # set text "game is already over."
          else do
            -- do an auto move with improved logic
            (board1, res) <- liftIO $ autoRevealSafe board0
            let msg =
                  case res of
                    Lost -> "you stepped on a mine - game over!"
                    Won  -> "all non-mine cells are uncovered - you win!"
                    _    -> "auto-move done."
            let newOver = case res of
                             Lost -> True
                             Won  -> True
                             _    -> False
            liftIO $ writeIORef gameRef (board1, mode0, newOver)
            void $ element messageDiv # set text msg
            drawBoard boardDiv messageDiv gameRef

--------------------------------------------------------------------------------
-- drawBoard sets up the grid of buttons for the current board
--------------------------------------------------------------------------------

drawBoard :: Element -> Element -> IORef (Board, String, Bool) -> UI ()
drawBoard container msgElem gameRef = do
    (brd, _, _) <- liftIO $ readIORef gameRef
    void $ element container # set children []

    let w = width brd
        h = height brd

    grid <- UI.div #. "cells-wrapper"
       # set style [ ("display","grid")
                   , ("grid-template-columns", "repeat(" ++ show w ++ ", 32px)")
                   , ("grid-template-rows",    "repeat(" ++ show h ++ ", 32px)") ]

    forM_ [0..h-1] $ \r ->
      forM_ [0..w-1] $ \c -> do
         let cCell = cells brd !! r !! c
         cellButton <- makeCellUI r c cCell gameRef msgElem container
         void $ element grid #+ [element cellButton]

    void $ element container #+ [element grid]

--------------------------------------------------------------------------------
-- building each cell as a button
--------------------------------------------------------------------------------

makeCellUI
  :: Int -> Int -> Cell
  -> IORef (Board, String, Bool)
  -> Element
  -> Element
  -> UI Element
makeCellUI r c cell gameRef msgElem container = do
    btn <- UI.button #. "square" # set style
             [ ("width","30px")
             , ("height","30px") ]

    let st = displayStatus cell
    case st of
      Hidden  -> void $ element btn # set text ""
      Flagged -> void $ element btn # set text "F"
      Revealed -> case content cell of
          Mine   -> void $ element btn # set text "*"
          Safe 0 -> void $ element btn # set text ""
          Safe n -> void $ element btn # set text (show n)

    on UI.click btn $ \_ -> do
       (b, currentMode, gOver) <- liftIO $ readIORef gameRef
       if gOver
         then void $ element msgElem # set text "game is over, no more moves."
         else if currentMode == "flag"
           then do
             let newB = toggleFlag b (r,c)
             liftIO $ writeIORef gameRef (newB, currentMode, gOver)
             redraw newB
           else do
             let (newB, result) = revealCell b (r,c)
             case result of
               Lost -> do
                 let lostBoard = revealAllMines newB
                 liftIO $ writeIORef gameRef (lostBoard, currentMode, True)
                 void $ element msgElem # set text "game over! you revealed a mine!"
                 redraw lostBoard
               InProgress -> do
                 let gameRes = checkWin newB
                 case gameRes of
                   Won -> do
                     liftIO $ writeIORef gameRef (newB, currentMode, True)
                     void $ element msgElem # set text "you win! congrats!"
                     redraw newB
                   _ -> do
                     liftIO $ writeIORef gameRef (newB, currentMode, False)
                     redraw newB
               Won -> do
                 liftIO $ writeIORef gameRef (newB, currentMode, True)
                 void $ element msgElem # set text "you win!"
                 redraw newB

    pure btn
  where
    redraw b = do
      (oldB, oldMode, oldOver) <- liftIO $ readIORef gameRef
      liftIO $ writeIORef gameRef (b, oldMode, oldOver)
      drawBoard container msgElem gameRef

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

getValue :: Element -> UI (Maybe String)
getValue el = do
    v <- get UI.value el
    return $ if null v then Nothing else Just v

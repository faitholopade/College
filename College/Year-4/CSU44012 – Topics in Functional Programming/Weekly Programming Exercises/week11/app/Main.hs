{-# LANGUAGE OverloadedStrings #-}
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Data.IORef
import Control.Monad (void)
import Data.List (elemIndices)

main :: IO ()
main = startGUI defaultConfig { jsStatic = Just "." } setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Simple Calculator"

    -- Display
    display <- UI.input # set (attr "readonly") "true"
                        # set UI.style [("width", "208px"), ("height", "30px"), ("font-size", "16px")]

    -- Buttons
    let buttonLabels = ["7","8","9","/","4","5","6","*","1","2","3","-","0",".","=","+","Clear"]
    buttons <- mapM createButton buttonLabels

    -- Buttons in grid format
    let buttonElements = map snd buttons
    gridElement <- gridLayout (chunksOf 4 buttonElements)

    -- Accumulate the input and display it
    inputRef <- liftIO $ newIORef ""

    -- Event handling
    sequence_ [on UI.click b $ \_ -> handleButtonClick label inputRef display | (label, b) <- buttons]

    -- Add elements to the window
    void $ getBody window #+ [column [element display, element gridElement]]

createButton :: String -> UI (String, Element)
createButton label = do
    b <- UI.button #+ [string label]
                   # set UI.style [("width", "50px"), ("height", "50px"), ("font-size", "16px")]
    return (label, b)

-- Button clicks
handleButtonClick :: String -> IORef String -> Element -> UI ()
handleButtonClick label inputRef display = do
    case label of
        "Clear" -> do
            liftIO $ writeIORef inputRef ""
            void $ element display # set UI.value ""
        "=" -> do
            expr <- liftIO $ readIORef inputRef
            let result = calculate expr
            liftIO $ writeIORef inputRef result
            void $ element display # set UI.value result
        _ -> do
            liftIO $ modifyIORef inputRef (++ label)
            expr <- liftIO $ readIORef inputRef
            void $ element display # set UI.value expr

-- Expression evaluator
calculate :: String -> String
calculate expr = 
    case parseExpr expr of
        Just n  -> show n
        Nothing -> "Error"

-- Parse and evaluate the expression
parseExpr :: String -> Maybe Double
parseExpr expr = evalArithmetic expr

-- Arithmetic evaluator with correct operator precedence
evalArithmetic :: String -> Maybe Double
evalArithmetic expr = parseExprWithOps ['+', '-'] expr

parseExprWithOps :: [Char] -> String -> Maybe Double
parseExprWithOps ops expr =
    case splitOnOps expr ops of
        Just (lhs, op, rhs) -> do
            l <- parseExpr lhs
            r <- parseExpr rhs
            case op of
                '+' -> Just (l + r)
                '-' -> Just (l - r)
                '*' -> Just (l * r)
                '/' -> if r /= 0 then Just (l / r) else Nothing
                _   -> Nothing
        Nothing -> if ops == ['+', '-']
                   then parseExprWithOps ['*', '/'] expr
                   else parseNumber expr

splitOnOps :: String -> [Char] -> Maybe (String, Char, String)
splitOnOps expr ops =
    let indices = [i | (i, c) <- zip [0..] expr, c `elem` ops]
    in case indices of
        [] -> Nothing
        _  -> let idx = last indices
                  op = expr !! idx
                  (lhs, _:rhs) = splitAt idx expr
              in Just (lhs, op, rhs)

parseNumber :: String -> Maybe Double
parseNumber expr = case reads expr :: [(Double, String)] of
    [(n, "")] -> Just n
    _         -> Nothing

gridLayout :: [[Element]] -> UI Element
gridLayout rows = UI.grid (map (map return) rows) # set UI.style [("margin", "10px")]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (y, ys) = splitAt n xs in y : chunksOf n ys

> {-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
> module InterpreterBase where

> import Prelude hiding (lookup, print)
> import qualified Data.Map as Map
> import Data.Maybe
> import qualified System.IO as System

> import Control.Monad.Identity
> import Control.Monad.Except
> import Control.Monad.Reader
> import Control.Monad.State
> import Control.Monad.Writer
> import Control.Monad.Trans (lift)
> import Control.Monad.Except (runExceptT, throwError, liftEither)

{-------------------------------------------------------------------}
{- The pure expression language                                    -}
{-------------------------------------------------------------------}

> hoistEval :: Eval a -> Run a
> hoistEval action = do
>     env <- get
>     let result = runEval env action
>     case result of
>         Left err -> throwError err
>         Right val -> return val

> data Val = I Int | B Bool
>            deriving (Eq, Show)

> data Expr = Const Val
>      | Add Expr Expr | Sub Expr Expr  | Mul Expr Expr | Div Expr Expr
>      | And Expr Expr | Or Expr Expr | Not Expr 
>      | Eq Expr Expr | Gt Expr Expr | Lt Expr Expr
>      | Var String
>    deriving (Eq, Show)

> type Name = String 
> type Env = Map.Map Name Val

> lookup k t = case Map.lookup k t of
>                Just x -> return x
>                Nothing -> throwError ("Unknown variable " ++ k)

> type Eval a = ReaderT Env (ExceptT String Identity) a
> runEval env ex = runIdentity (runExceptT (runReaderT ex env))

> evali op e0 e1 = do e0' <- eval e0
>                     e1' <- eval e1
>                     case (e0', e1') of
>                          (I i0, I i1) -> return $ I (i0 `op` i1)
>                          _            -> throwError "type error in arithmetic expression"

> evalb op e0 e1 = do e0' <- eval e0
>                     e1' <- eval e1
>                     case (e0', e1') of
>                          (B i0, B i1) -> return $ B (i0 `op` i1)
>                          _            -> throwError "type error in boolean expression"

> evalib op e0 e1 = do e0' <- eval e0
>                      e1' <- eval e1
>                      case (e0', e1') of
>                           (I i0, I i1) -> return $ B (i0 `op` i1)
>                           _            -> throwError "type error in arithmetic expression"

> eval :: Expr -> Eval Val
> eval (Const v) = return v
> eval (Add e0 e1) = evali (+) e0 e1
> eval (Sub e0 e1) = evali (-) e0 e1
> eval (Mul e0 e1) = evali (*) e0 e1
> eval (Div e0 e1) = evali div e0 e1
> eval (And e0 e1) = evalb (&&) e0 e1
> eval (Or e0 e1) = evalb (||) e0 e1
> eval (Not e0) = evalb (\a _ -> not a) e0 (Const (B True))
> eval (Eq e0 e1) = evalib (==) e0 e1
> eval (Gt e0 e1) = evalib (>) e0 e1
> eval (Lt e0 e1) = evalib (<) e0 e1
> eval (Var s) = do env <- ask
>                   lookup s env

{-------------------------------------------------------------------}
{- The statement language with Writer Monad for Logging            -}
{-------------------------------------------------------------------}

> data Statement = Assign String Expr
>                | If Expr Statement Statement
>                | While Expr Statement
>                | Print Expr
>                | Seq Statement Statement
>                | Try Statement Statement
>                | Pass                    
>       deriving (Eq, Show)

> type Log = [String]
> type Run a = StateT Env (ExceptT String (WriterT Log Identity)) a


> exec :: Statement -> Run ()
> exec (Assign name expr) = do
>     val <- hoistEval (eval expr)
>     tell ["Assign " ++ name ++ " = " ++ show val]
>     modify (Map.insert name val)
 
> exec (If cond thenStmt elseStmt) = do
>     condVal <- hoistEval (eval cond)
>     case condVal of
>         B condVal' -> if condVal'
>                         then do
>                             tell ["If condition true"]
>                             exec thenStmt
>                         else do
>                             tell ["If condition false"]
>                             exec elseStmt
>         _ -> throwError "If condition must be a boolean"

> exec (While cond stmt) = do
>     condVal <- hoistEval (eval cond)
>     case condVal of
>         B condVal' -> if condVal'
>                         then do
>                             tell ["While loop iteration"]
>                             exec stmt
>                             exec (While cond stmt)
>                         else tell ["While loop exit"]
>         _ -> throwError "While condition must be a boolean"
 
> exec (Print expr) = do
>     val <- hoistEval (eval expr)
>     tell ["Print " ++ show val]
 
> exec (Seq stmt1 stmt2) = do
>     exec stmt1
>     exec stmt2
 
> exec (Try stmt1 stmt2) = do
>     result <- catchError (exec stmt1) (\_ -> exec stmt2)
>     tell ["Try block executed"]
>     return result
 
> exec Pass = tell ["Pass statement"]
 
> exec _ = throwError "Unhandled statement in exec"
 
> runProgram :: Env -> Statement -> (Either String (), Log)
> runProgram env stmt = runIdentity (runWriterT (runExceptT (evalStateT (exec stmt) env)))

{-------------------------------------------------------------------}
{- Helper Functions                                                -}
{-------------------------------------------------------------------}


> sampleEnv :: Env
> sampleEnv = Map.fromList [("x", I 10), ("y", I 20)]

> sampleProgram :: Statement
> sampleProgram = Seq (Assign "z" (Add (Var "x") (Var "y")))
>                      (Print (Var "z"))

> runSample :: IO ()
> runSample = do
>     let (result, log) = runProgram sampleEnv sampleProgram
>     case result of
>         Left err -> putStrLn $ "Error: " ++ err
>         Right _  -> putStrLn "Program executed successfully"
>     putStrLn "Log:"
>     mapM_ putStrLn log


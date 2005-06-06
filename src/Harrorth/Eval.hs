{-# OPTIONS_GHC -fglasgow-exts #-}

{- vim:set expandtab: -}
module Harrorth.Eval where

import Harrorth.AST
import Data.Map (Map, insert, (!))
import Control.Monad.RWS

type Stack = [Literal]
type Dict = Map Word Forth

-- type Eval a = ReaderT Interp IO a

data Interp = MkInterp
    { interpStack   :: Stack
    , interpDict    :: Dict
    } deriving (Show)

type Eval a = (MonadReader Interp m, MonadWriter Stack m) => m a

instance MonadWriter Stack IO where
    tell   = putStr . unlines . map (("==> " ++) . show)
    listen = fail "Cannot listen"
    pass   = fail "Cannot pass"
    
interpret :: Forth -> Eval Interp
interpret [] = ask
interpret (exp:exps) = do
    fun <- doExp exp
    local fun (interpret exps)

doExp :: Exp -> Eval (Interp -> Interp)
doExp (Push lit) = doStack (lit:)
doExp (Invoke ".") = do
    (x:xs) <- asks interpStack
    tell [x]
    return (\i -> i{ interpStack = xs })

doExp (Invoke ".S") = do
    stack <- asks interpStack
    tell stack
    return id

doExp (Invoke "0SP") = doStack $ const []
doExp (Invoke "DUP") = doStack $ \(x:xs) -> x:x:xs
doExp (Invoke "DROP") = doStack $ tail
doExp (Invoke "SWAP") = doStack $ \(a:b:xs) -> b:a:xs
doExp (Invoke "OVER") = doStack $ \stack -> (head stack):stack
doExp (NewWord word body) = return $ \i -> i{ interpDict = insert word body (interpDict i) }
doExp (Invoke userWord) = do
    dict    <- asks interpDict
    i'      <- interpret (dict ! userWord)
    return $ const i'

doStack :: (Stack -> Stack) -> Eval (Interp -> Interp)
doStack f = return $ \i -> i{ interpStack = f (interpStack i) }

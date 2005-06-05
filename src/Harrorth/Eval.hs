{- vim:set expandtab: -}
module Harrorth.Eval where

import Harrorth.AST
import Data.Map
import Control.Monad.Reader

type Stack = [Literal]
type Dict = Map Word Forth

type Eval a = ReaderT Interp IO a

data Interp = Interp { stack::Stack
                     , dict::Dict
                     }
                     deriving Show

interpret :: Forth -> Eval Interp
interpret [] = ask
interpret (exp:exps) = do
    fun <- doExp exp
    local fun (interpret exps)

doExp :: Exp -> Eval (Interp -> Interp)
doExp (Push lit) = doStack (lit:)
doExp (Invoke ".") = do
    (x:xs) <- asks stack
    liftIO $ print x
    return (\i -> i{ stack = xs })

doExp (Invoke ".S") = do
    liftIO . print =<< asks stack
    return id

doExp (Invoke "0SP") = doStack $ const []
doExp (Invoke "DUP") = doStack $ \(x:xs) -> x:x:xs
doExp (Invoke "DROP") = doStack $ tail
doExp (Invoke "SWAP") = doStack $ \(a:b:xs) -> b:a:xs
doExp (Invoke "OVER") = doStack $ \stack -> (head stack):stack
doExp (NewWord word body) = return $ \i -> i{ dict = insert word body (dict i) }
doExp (Invoke userWord) = do
    d   <- asks dict
    i'  <- interpret (d ! userWord)
    return $ const i'

doStack :: (Stack -> Stack) -> Eval (Interp -> Interp)
doStack f = return $ \i -> i{ stack = f (stack i) }

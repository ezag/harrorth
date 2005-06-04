{- vim:set expandtab: -}
module Harrorth.Eval where

import Harrorth.AST

data Interp = Interp { stack::[Literal] } deriving Show

interpret :: Interp -> Forth -> IO Interp
interpret i [] = return i
interpret i (exp:exps) = do
    i' <- doExp i exp
    interpret i' exps

doExp :: Interp -> Exp -> IO Interp
doExp i (Push lit) = return $ pushStack i lit
doExp i (Invoke ".") = do
    let (stackHead, i') = popStack i
    print stackHead
    return i'

popStack :: Interp -> (Literal, Interp)
popStack i@Interp{ stack = x:xs } = (x, i{ stack = xs })

pushStack :: Interp -> Literal -> Interp
pushStack i@Interp{ stack = stack } lit = i{ stack = (:) lit stack }


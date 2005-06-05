{- vim:set expandtab: -}
module Harrorth.Eval where

import Harrorth.AST
import Data.Map

type Stack = [Literal]
type Dict = Map Word Forth

data Interp = Interp { stack::Stack
                     , dict::Dict
                     }
                     deriving Show

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

doExp i (Invoke ".S") = do
    print $ stack i
    return i

doExp i (Invoke "0SP") = return i{ stack = [] }
doExp i@Interp{ stack = x:xs } (Invoke "DUP") = return i{ stack = x:x:xs }
doExp i@Interp{ stack = x:xs } (Invoke "DROP") = return i{ stack = xs }
doExp i@Interp{ stack = a:b:xs } (Invoke "SWAP") = return i{ stack = b:a:xs }
doExp i@Interp{ stack = stack } (Invoke "OVER") = return i{ stack = (head stack):stack }

doExp i@Interp{ dict = dict } (NewWord word body) = return i{ dict = insert word body dict }

doExp i@Interp{ dict = dict } (Invoke userWord) = interpret i $ dict ! userWord

popStack :: Interp -> (Literal, Interp)
popStack i@Interp{ stack = x:xs } = (x, i{ stack = xs })

pushStack :: Interp -> Literal -> Interp
pushStack i@Interp{ stack = stack } lit = i{ stack = (:) lit stack }


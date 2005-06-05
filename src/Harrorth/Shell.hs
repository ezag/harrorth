{- vim:set expandtab: -}
module Main where

import Text.ParserCombinators.Parsec
import Harrorth.Eval
import Harrorth.Parser
import Harrorth.AST
import Data.Map

main = do
    src <- getLine
    case (parse forthProgram "" src) of
        Left err -> do
            putStr "parse error at "
            print err
        Right x -> dumpInterp x

dumpInterp :: Forth -> IO ()
dumpInterp ast = do
    finished <- interpret Interp { stack = []
                                 , dict = empty
                                 } ast
    print finished


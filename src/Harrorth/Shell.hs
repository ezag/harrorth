{- vim:set expandtab: -}
module Main where

import Text.ParserCombinators.Parsec
import Harrorth.Eval
import Harrorth.Parser
import Harrorth.AST
import Data.Map (empty)
import Data.Char (toUpper)
import Control.Monad.Reader

main = do
    src <- fmap (map toUpper) getLine
    case (parse forthProgram "" src) of
        Left err -> do
            putStr "parse error at "
            print err
        Right x -> dumpInterp x

initInterp :: Interp
initInterp = MkInterp
    { interpStack = []
    , interpDict  = empty
    }

dumpInterp :: Forth -> IO ()
dumpInterp ast = do
    finished <- (`runReaderT` initInterp) $ interpret ast
    print finished


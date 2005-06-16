{-# OPTIONS_GHC -fglasgow-exts #-}
{- vim:set expandtab: -}

module Main where

import Control.Monad.Reader (runReaderT)
import Harrorth.Eval (nibbleBuffer, initInterp)

main = do
    putStrLn "Welcome to Harrorth!"
    runReaderT (nibbleBuffer) initInterp
    return ()


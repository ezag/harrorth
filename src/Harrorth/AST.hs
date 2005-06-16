{-# OPTIONS_GHC -fglasgow-exts #-}
{- vim:set expandtab: -}

module Harrorth.AST where

import Control.Monad.Reader

-- This file defines the structure of a forth system

-- Compiled representation of code

type Forth = [Exp]          -- a forth program is some expressions

data Exp                    -- an expression is
    = Push Literal          -- a literal being pushed onto the stack
    | ZeroBranch Forth      -- or a conditional branch to some code
    | Call WordDef          -- or a call to a word's Forth
    | Prim (Eval Interp)    -- or a primitive is a function in the Eval monad
    deriving Show

data WordDef = WordDef
    { immediate   :: Bool
    , compileOnly :: Bool
    , name        :: Word
    , body        :: Forth
    } deriving Show

-- these are the data types in forth
type Literal = Integer      -- a literal is an integer
type Word = String          -- a word name is a string



-- A Forth VM

data Interp = MkInterp
    { interpStack   :: Stack
    , interpDict    :: Dict
    , interpBuffer  :: String
    , interpState   :: InterpState
    } deriving Show

-- the data types that live inside an interpreter
type Stack = [Literal]
type Dict = [WordDef]

-- Order is important! fromEnum will correlate to STATE ON, STATE OFF
data InterpState = Interpretation | Compilation deriving Show


-- An Eval is a a monad that can be both a reader and a writer
type Eval a = ReaderT a IO a

instance Show (Eval Interp) where
    show = const "<prim>"


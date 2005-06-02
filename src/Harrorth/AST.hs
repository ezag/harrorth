module Harrorth.AST where

-- This file defines the structure of a forth program.

type Forth = [Exp]			-- a forth program is some expressions

data Exp					-- an expression is
	= Invoke Word			-- the invocation of a word
	| Push Literal			-- or a literal being pushed onto the stack
	| NewWord Word Forth    -- or the definition of a new word
	deriving Show			-- printable representation for free

type Word = String          -- a word is just a string, a name

type Literal = Integer      -- a literal is for now an integer


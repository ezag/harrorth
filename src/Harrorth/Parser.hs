module Harrorth.Parser where

import Text.ParserCombinators.Parsec
import Harrorth.AST

forthProgram :: Parser Forth
forthProgram = do
	ast <- forth
	eof
	return ast

forth :: Parser Forth
forth = sepEndBy forthExp sep

forthExp :: Parser Exp
forthExp = newWord
	   <|> literal
	   <|> word

newWord :: Parser Exp
newWord = do
	char ':'
	maybeSep
	name <- wordName
	sep
	body <- forth
	maybeSep
	char ';'
	return $ NewWord name body

literal :: Parser Exp
literal = do
	lit <- many1 digit
	return $ Push (read lit)

word :: Parser Exp
word = do
	name <- wordName
	return $ Invoke name

wordName :: Parser String
wordName = do
	name <- many1 wordChar
	return name

wordChar = do letter <|> char '.'
		
sep :: Parser ()
sep = skipMany1 space

maybeSep:: Parser ()
maybeSep = skipMany space


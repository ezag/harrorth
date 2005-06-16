{-# OPTIONS_GHC -fglasgow-exts #-}
{- vim:set expandtab: -}
module Harrorth.Eval where

import Harrorth.AST
import Control.Monad.Reader
import Data.Char (isSpace, toUpper)
import Data.List (find)
import System.Exit
import System.Console.Readline (readline)

-- an empty interpreter
initInterp = MkInterp
    { interpStack  = []
    , interpDict   = initialDict
    , interpBuffer = ""
    , interpState  = Interpretation
    }

-- the trace function ccan be used to emit debugging traces. It's unused right now
-- trace = liftIO . putStrLn

-- execute reduces lists of Exps, and creates derived versions of the Interp
-- as it goes along, passing modified versions down with the Reader monad
execute :: Forth -> Eval Interp

-- when there is nothhing to do (the AST is depleted) we read some more
execute [] = nibbleBuffer

-- Forth CPS ;-)
execute ((Call word):exps) = execute ((body word) ++ exps)

execute ((ZeroBranch false):true) = do
    (x:xs) <- asks interpStack    -- take TOS
    -- choose a branch and evaluate it with remainder of stack
    local (withStack xs) $ execute $ if (x == 0) then false else true

-- normal reduction of general expressions yields a modified interpreter
-- we use local to set a new version of this interpreter
execute (exp:exps) = do
    -- trace $ "reducing exp " ++ (show exp) ++ ", " ++ (show exps) ++ " remaining"
    i <- doExp exp
    local (const i) (execute exps)

-- doExp is the general single exp reducer
doExp :: Exp -> Eval Interp

-- applying a primitive is just applying the embedded function
doExp (Prim fun) = fun

-- doStack is the generic stack transformer.
-- Pushing a lit is done by applying a function that conses the lit with the stack
doExp (Push lit) = doStack (lit:)

-- applyStack creates functions which apply functions to the stack of an interpreter
applyStack :: (Stack -> Stack) -> (Interp -> Interp)
applyStack f = \i -> i{ interpStack = f (interpStack i) }

-- doStack is just applyStack in the Eval monad
doStack :: (Stack -> Stack) -> Eval Interp
doStack f = asks $ applyStack f

-- nibbleBuffer is called when there is nothing to do but read a word and execute it
-- it is the interactive loop, basically
nibbleBuffer :: Eval Interp
nibbleBuffer = do
    takeWord findWord
    where
        findWord :: Word -> Eval Interp
        findWord word = do
            dict <- asks interpDict
            case (find ((word == ) . name) dict) of
                Nothing -> do
                    case (reads word) of
                        [] -> do
                            liftIO $ putStrLn $ "Error: the word " ++ word ++ " is undefined"
                            nibbleNewBuffer
                        ((lit,_):_) -> doLit lit
                Just wordDef -> doWord wordDef

nibbleNewBuffer = local (withBuf "") nibbleBuffer

-- doLit will compile or execute a literal depending on state
doLit lit = do
    -- dict <- asks interpDict
    -- trace $ "compiling literal " ++ (show lit) ++ " into " ++ (name $ head dict)
    state <- asks interpState
    let instr = [ Push lit ]
    case state of
        Compilation -> appendToCurrentWord instr
        Interpretation -> execute instr

-- doWord will compile or execute a word depending on state
doWord wordDef = do
    state <- asks interpState
    case state of
        Compilation -> do
            if (immediate wordDef)
                then interpretWord wordDef
                else compileWord wordDef
        Interpretation -> do
            if (compileOnly wordDef)
                then do
                    liftIO $ putStrLn $ "Error: " ++ (name wordDef) ++ " is a compile-only word"
                    nibbleNewBuffer
                else interpretWord wordDef

-- compileWord takes the definition of a word as found in a dictionary
-- and compiles it into the last word inserted to the dictionary
compileWord wordDef = appendToCurrentWord [ Call wordDef ]

-- appendToCurrentWord adds a bit of Forth to the last entry in the dictionary
appendToCurrentWord exps = do
    i <- ask
    let dict        = interpDict i
        (cur:rest)  = dict
        cur'        = cur{ body = (body cur) ++ exps }
    local (\i -> i{ interpDict = cur':rest }) nibbleBuffer

-- interpretWord is just an invocation 
interpretWord wordDef = execute [Call wordDef]

-- takeWord takes a word out of the buffer, refilling it as necessary
takeWord fun = do
    buf <- asks interpBuffer
    let (word, remain) = breakWord buf
    case word of
        "" -> needMore
        _  -> local (withBuf remain) $ fun word
    where
        needMore = fillBuffer $ takeWord fun
        breakWord buf = break isSpace $ dropWhile isSpace buf

-- fillBuffer get's input from the user
-- when the user is finished (^D) it will return to main
fillBuffer cont = do
    mLine <- liftIO $ readline "> "
    case mLine of
        Nothing -> ask
        Just line -> local (withBuf (map toUpper line)) cont

-- withBuf replaces the buffer in an interpreter, typically passed to local
withBuf buf = \i -> i{ interpBuffer = buf }

-- withStack gives back a function that gets an Interp and replaces it's stack with the parameter it took
withStack = applyStack . const

-- withState changes the interpreter's state
withState state = \i -> i{ interpState = state }

-- the initial dictionary contains words based on the primitives
initialDict =
    (primToWord ";"){ immediate = True } : map primToWord  prims
    where
        primToWord word = WordDef -- primToWord wraps a prim's function in a word definition
            { name = word
            , body = [ Prim (prim word) ]
            , immediate = False
            , compileOnly = False
            }

-- a list of primitives
-- FIXME: this ought to be better taken care of
prims = words ". .S : 0SP DUP DROP SWAP BYE SEE + - * / ="

-- here is the table of primitives
prim :: Word -> Eval Interp

prim "BYE" = do -- exit the program
    -- i <- ask
    -- dump the interpreter - liftIO $ print i
    liftIO $ exitWith ExitSuccess

prim "." = do -- print tos
    x <- fmap head (asks interpStack)
    liftIO $ print x
    doStack tail

prim ".S" = do -- print stack
    stack <- asks interpStack
    liftIO $ print stack
    ask
    
prim ":" = do -- define word
    takeWord makeWord
    where
        makeWord word = do
            i <- ask
            return i -- return an interp with the modified dictionary, in the compile state
                { interpDict  = (newWord word):(interpDict i)
                , interpState = Compilation
                }
        newWord word = WordDef -- an empty word with a name
            { name        = word
            , immediate   = False
            , compileOnly = False
            , body        = []
            }

prim ";" = asks $ withState Interpretation

-- deparses a word's definition back into forth
prim "SEE" = do
    takeWord seeWord
    where
        seeWord word = do
            dict <- asks interpDict
            case (find ((word == ) . name) dict) of
                Nothing      -> do
                    liftIO $ putStrLn $ "Error: the word " ++ word ++ " does not exist"
                    nibbleNewBuffer
                Just wordDef -> do
                    liftIO $ putStrLn $ showWordDef wordDef
                    ask
        showWordDef wordDef = unwords
            [ ":"
            , name wordDef
            , showBody (body wordDef)
            , ";"
            , (if (immediate wordDef) then "IMMEDIATE" else "")
            , (if (compileOnly wordDef) then "COMPILE-ONLY" else "")
            ]
        showBody [] = ""
        showBody (exp:exps) = unwords [ (showExp exp), showBody exps ]
        showExp (Push lit) = show lit -- literals are just shhown
        showExp (Call wordDef) = name wordDef -- called words are named
        showExp (ZeroBranch _) = "0BRANCH" -- branches are stupidly portrayed
        showExp (Prim _) = "<prim>" -- primitives can't be shown

-- other prims are simple operations on the stack
prim x = doStack $ stackPrim x

-- these are all functions that create a list from a list
-- representing the stack operations
stackPrim :: Word -> (Stack -> Stack)

stackPrim "0SP" = const []
stackPrim "DUP" = \stack -> (head stack):stack
stackPrim "DROP" = tail
stackPrim "SWAP" = \(a:b:xs) -> b:a:xs

-- infixOp takes a binary function and applies it the two elements
-- at the top of the stack, replacing them with the result

stackPrim "+" = infixOp (+)
stackPrim "-" = infixOp (-)
stackPrim "*" = infixOp (*)
stackPrim "/" = infixOp div

-- binary logic is coerced into integers
-- FIXME: toInteger is not really Literal, but who cares
stackPrim "=" = infixOp $ ((toInteger . fromEnum) .) . (==)

infixOp f = \(a:b:xs) -> (f a b):xs


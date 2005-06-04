module ForthProggie where
import Harrorth.AST

proggie =
    [ Push 2
    , Push 4
    , Invoke "SWAP"
    , NewWord "FOO"
        [ Push 3
        , Invoke "SWAP"
        ]
    , Invoke "FOO"
    ]

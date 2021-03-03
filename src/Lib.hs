module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- The definition of Syntactic Category

-- Primitive categories
data PrimCate = S | Np

-- Slashes   /.   \.   /#   \#   /x   \x   /*   \*
data Slash = Fd | Bd | Fc | Bc | Fx | Bx | Fa | Ba





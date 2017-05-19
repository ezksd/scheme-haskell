module Main where

import           Interpreter
import           Parser


main :: IO ()
main = do
    env <- env0
    s <- getLine
    if null s
        then pure ()
        else case parse s of
            Nothing -> main
            Just expr -> do r <- eval env expr
                            case r of
                                Left err -> print err >> main
                                Right e  -> print e >> main


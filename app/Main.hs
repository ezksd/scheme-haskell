module Main where
import           Control.Monad.Trans.Except
import           Interpreter
import           Parser
import           Scheme
import           System.IO

main :: IO ()
main = getEnv >>= loop ""

putAndFlush :: String -> IO ()
putAndFlush s = putStr s >> hFlush stdout

loop :: String -> Env -> IO ()
loop s env = do s1 <- getLine
                if s1 == "exit"
                    then pure ()
                    else let s2 = s ++ s1
                         in if check s2
                             then evalAndPrint s2 env >> loop "" env
                             else loop s2 env

evalAndPrint :: String -> Env -> IO ()
evalAndPrint s env = runExceptT (ExceptT (pure (parse s)) >>= eval env) >>= either print print

count :: Int -> String -> Int
count c [] = c
count c (x:xs) = count r xs
    where r = case x of
                '(' -> c + 1
                ')' -> c - 1
                _   -> c
check :: String -> Bool
check s = count 0 s == 0



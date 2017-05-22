module Main where
import           Control.Monad
import           Control.Monad.Trans.Except
import           Interpreter
import           Scheme

-- main :: IO ()
-- main = getEnv >>= loop ""

main :: IO ()
main = getInterpreter >>= loop ""

loop :: String -> (String -> ExceptT ScmErr IO [Expr]) -> IO ()
loop s interp = do  s1 <- getLine
                    if s1 == "exit"
                    then pure ()
                    else let s2 = s ++ s1
                        in if check s2
                            then go s2 >> loop "" interp
                            else loop s2 interp
    where run = runExceptT . interp
          printAll = void . sequence . (print <$>)
          printResult = either print printAll
          go = (>>= printResult) . run

count :: Int -> String -> Int
count c [] = c
count c (x:xs) = count r xs
    where r = case x of
                '(' -> c + 1
                ')' -> c - 1
                _   -> c
check :: String -> Bool
check s = count 0 s == 0 && (not (null s))



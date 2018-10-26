{-# LANGUAGE LambdaCase #-}
module Interpreter (repl) where
import           System.IO
import           Data.IORef
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Ref
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import qualified Data.Map.Strict               as Map
import           Parser
import           Prelude                 hiding ( lookup,init )
import           Prims
import           Scheme

type Interpreter a = ReaderT Env (ExceptT ScmErr IO) a

repl :: IO ()
repl = init >>= loop' ""

init :: IO Env
init = do
    content <- readFile "primit.scm"
    ref     <- newIORef [Map.fromList primitives]
    eval content ref
    pure ref

loop :: Env -> IO ()
loop env = do
    putStr "> "
    hFlush stdout
    content <- getLine
    eval content env
    loop env

loop' :: String -> Env -> IO()
loop' s env = do
    putStr output
    hFlush stdout
    content <- getLine
    let s' = s ++ content 
        n = count s' 
        in case () of
            _ | n >  0 -> loop' s' env
              | n == 0 -> eval s' env >> loop' "" env
              | n <  0 -> putStrLn "illgal input" >> loop' "" env
    where 
        count = count' 0
        count' n [] = n
        count' n ('(':xs) = count' (n + 1) xs
        count' n (')':xs) = count' (n - 1) xs
        count' n (_:xs)   = count' n       xs
        num = count s
        output = case count s of
            n | n == 0 -> "> "
              | n > 0 -> space (n*2)
              | n < 0 -> "illegal input"
        space n = space' n []  
        space' 0 xs = xs
        space' n xs = space' (n - 1) ("  " ++ xs)

eval :: String -> Env -> IO ()
eval s env = case parseAll s of
    Nothing -> putStrLn "parse error"
    Just a  -> do
        result <- runExceptT (runReaderT (interpretAll a) env)
        case result of
            Left  e -> putStrLn e
            Right x -> putStr (x >>= m)
  where
    m Void = mempty
    m x    = show x ++ "\n"

define :: String -> Expr -> Interpreter Expr
define k v = ask >>= (`modifyRef` (\(x : xs) -> Map.insert k v x : xs)) >> pure Void

lookup :: String -> Interpreter Expr
lookup k = ask >>= readRef >>= maybe err pure . lookup'
  where
    err     = lift (throwE ("unbounded identifer :" ++ k))
    lookup' = foldr ((<|>) . Map.lookup k) empty

push :: [(String,Expr)] -> Interpreter ()
push xs = ask >>= (`modifyRef` (Map.fromList xs:))

pop :: Interpreter ()
pop = ask >>= (`modifyRef` tail)

interpret :: Expr -> Interpreter Expr
interpret (Symbol x) = lookup x
interpret (List   x) = case x of
    Symbol "define" : List (Symbol f : xs) : body -> ask >>= define f . Closure (unSymbols xs) body
    Symbol "lambda" : List xs : body -> Closure (unSymbols xs) body <$> ask
    Symbol "let" : List binding : body -> (extract binding >>= push) *> interpretBody body <* pop
    [Symbol "set!"  , Symbol k, v] -> interpret v >>= define k
    [Symbol "define", Symbol k, v] -> interpret v >>= define k
    [Symbol "quote", e]            -> pure e
    [Symbol "if", p, v1, v2]       -> do
        (Bool b) <- interpret p
        interpret (if b then v1 else v2)
    xs -> do
        (h : params) <- interpretAll xs
        case h of
            (Closure ps body env') -> push (zip ps params) *> interpretBody body <* pop
            (Func func) -> lift (func params)
            _           -> lift (throwE "not a function")
  where
    interpretBody = (last <$>) . interpretAll
    unSymbols = ((\case (Symbol input) -> input) <$>)
    extract []                        = pure []
    extract (List [Symbol s, v] : xs) = do
        e  <- interpret v
        es <- extract xs
        pure ((s, e) : es)
interpret x = pure x


interpretAll :: [Expr] -> Interpreter [Expr]
interpretAll = sequence . (interpret <$>)


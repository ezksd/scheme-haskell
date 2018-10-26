{-# LANGUAGE LambdaCase #-}
module Interpreter (repl) where
import           System.IO
import           Data.IORef
import           Data.List                      ( intercalate )
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Ref
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict               as Map
import           Parser
import           Prelude                 hiding ( lookup )
import           Prims
import           Scheme

type Interpreter a = ReaderT Env (ExceptT ScmErr IO) a

repl :: IO ()
repl = do
    content <- readFile "primit.scm"
    ref     <- newIORef [Map.fromList primitives]
    runReaderT (repl' content) ref
    runReaderT loop            ref
    where loop = lift (putStr "> ") >> lift getLine >>= repl' >> loop

repl' :: String -> ReaderT Env IO ()
repl' s = do
    env <- ask
    lift $ case parseAll s of
        Nothing -> putStrLn "parse error"
        Just a  -> do
            result <- runExceptT (runReaderT (interpretAll a) env)
            case result of
                Left  error -> putStrLn error
                Right x     -> putStrLn (x >>= m)
  where
    m Void = mempty
    m x    = show x ++ "\n"

define :: String -> Expr -> Interpreter Expr
define k v = ask >>= (`modifyRef` (\(x:xs) -> Map.insert k v x:xs)) >> pure Void

lookup :: String -> Interpreter Expr
lookup k = ask >>= readRef >>= maybe err pure . lookup'
  where
    err     = lift (throwE ("unbounded identifer :" ++ k))
    lookup' = foldr ((<|>) . Map.lookup k) empty

interpret :: Expr -> Interpreter Expr
interpret (Symbol x) = lookup x
interpret (List   x) = case x of
    Symbol "define" : List (Symbol f : xs) : body ->
        ask >>= define f . Closure (unSymbols xs) body
    Symbol "lambda" : List xs : body -> Closure (unSymbols xs) body <$> ask
    Symbol "let" : List binding : body -> extract binding >>= ((ask >>= readRef >>=) . (newRef .) . (:) . Map.fromList) >>= ((`local` interpretBody body) . const)
    [Symbol "set!", Symbol k, v]     -> interpret v >>= define k
    [Symbol "define", Symbol k, v]   -> interpret v >>= define k
    [Symbol "quote", e]              -> pure e
    [Symbol "if", p, v1, v2]         -> do
        expr <- interpret p
        interpret (if unBool expr then v1 else v2)
    xs -> do
        (h : params) <- interpretAll xs
        case h of
            (Closure ps body env') ->
                (ask >>= readRef >>= (newRef . (Map.fromList (zip ps params) :))) >>= ((`local` interpretBody body) . const)
            (Func func) -> lift (func params)
            _           -> lift (throwE "not a function")
  where
    interpretBody = (last <$>) . interpretAll
    unSymbols = ((\case (Symbol input) -> input) <$>)
    unBool (Bool b) = b
    unBool  _       = False
    extract []                        = pure []
    extract (List [Symbol s, v] : xs) = do
        e  <- interpret v
        es <- extract xs
        pure ((s, e) : es)
interpret x = pure x


interpretAll :: [Expr] -> Interpreter [Expr]
interpretAll = sequence . (interpret <$>)


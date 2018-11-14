{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Interpreter
    ( repl
    )
where
import           Data.IORef
import           Control.Monad.Ref
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Paths_scheme
import qualified Data.Map.Strict               as Map
import           Parser
import           Prelude                 hiding ( init
                                                , lookup
                                                )
import           Prims
import           Scheme
import           System.IO
import           System.IO.Error

type Interpreter a = StateT Env (ExceptT String IO) a

repl :: IO ()
repl = do
    putStrLn "press Ctrl+D to quit."
    ref <- newIORef (Map.fromList primitives)
    env <- getDataFileName "primit.scm" >>= readFile >>= (`repl'` [ref])
    catchIOError (loop "" env) (\e -> if isEOFError e then putStrLn "bye~" else ioError e)

repl' :: String -> Env -> IO Env
repl' s env = do
    x <- runExceptT (runStateT parseAndEval env)
    case x of
        Left  e      -> putStrLn e >> pure env
        Right (a, env') -> putStr (format a) >> pure env'
  where
    parseAndEval =
        maybe (lift (throwE "parse error")) pure (parseAll s) >>= interpretAll
    format = (>>= format')
    format' Void = ""
    format' x    = show x ++ "\n"

loop :: String -> Env -> IO ()
loop s env = do
    putStr indent >> hFlush stdout
    s' <- (s ++) <$> getLine
    case count s' of
        n' | n' > 0    -> loop s' env
           | n' == 0   -> repl' s' env >>= loop ""
           | otherwise -> putStrLn "illgal input" >> loop "" env
  where
    space 0 xs = xs
    space a xs = space (a - 1) ("    " ++ xs)
    n      = count s
    indent = if
        | n == zero    -> "> "
        | n > zero     -> space n []
        | otherwise -> "illegal input"
    count  = foldr
        (\a b -> case a of
            '(' -> b + 1
            ')' -> b - 1
            _   -> b
        )
        0
zero :: Integer
zero = 0

throw :: String -> Interpreter a
throw = lift . throwE

define :: String -> Expr -> Interpreter ()
define k v = head <$> get >>= (`modifyRef` Map.insert k v)

update :: String -> Expr -> Interpreter ()
update = ((get >>=) .) . update'
  where
    update' k _ []       = throw ("unbounded identifer :" ++ k)
    update' k v (x : xs) = do
        env <- readRef x
        if Map.member k env
            then modifyRef x (Map.insert k v)
            else update' k v xs

lookup :: String -> Interpreter Expr
lookup k = do
    env <- get
    lift $ maybeToExceptT ("unbounded identifer :" ++ k) (lookup' env)
  where
    lookup' []       = empty
    lookup' (x : xs) = do
        m <- lift (readIORef x)
        maybe empty pure (Map.lookup k m) <|> lookup' xs

push :: [(String, Expr)] -> Interpreter ()
push xs = newRef (Map.fromList xs) >>= (modify . (:))

pop :: Interpreter ()
pop = modify tail

interpret :: Expr -> Interpreter Expr
interpret (Symbol x) = lookup x
interpret (List   x) = case x of
    Symbol "define" : List (Symbol f : xs) : body ->
        get >>= define f . Closure (unSymbols xs) body >> pure Void
    Symbol "lambda" : List xs : body -> Closure (unSymbols xs) body <$> get
    Symbol "let" : List binding : body ->
        (extract binding >>= push) *> interpretBody body <* pop
    [Symbol "set!"  , Symbol k, v] -> interpret v >>= update k >> pure Void
    [Symbol "define", Symbol k, v] -> interpret v >>= define k >> pure Void
    [Symbol "quote", e]            -> pure e
    [Symbol "if", p, v1, v2]       -> do
        -- (Bool b) <- interpret p
        x' <- interpret p
        case x' of
            (Bool b) -> interpret (if b then v1 else v2)
            _ -> throw "not a bool"
    (Symbol "foldr" : xs)          -> do
        x' <- interpretAll xs
        case x' of
            [op, init, List xs'] -> foldM (\b a -> interpret (List [op, Lazy a, Lazy b])) init (reverse xs')
            _ -> throw "illegal parameter"
    xs                             -> do
        (h : params) <- interpretAll xs
        case h of
            (Closure ps body env') -> do
                env <- get
                put env'
                push (zip ps params)
                e <- interpretBody body
                put env
                pure e
            (Func func) -> lift (func params)
            _           -> throw "not a function"
  where
    interpretBody = (last <$>) . interpretAll
    unSymbols = ((\case (Symbol input) -> input) <$>)
    extract []                        = pure []
    extract (List [Symbol s, v] : xs) = do
        e  <- interpret v
        es <- extract xs
        pure ((s, e) : es)
    extract _ = throw "illegal parameters"
interpret (Lazy y) = pure y
interpret x        = pure x

interpretAll :: [Expr] -> Interpreter [Expr]
-- interpretAll = traverse interpret
interpretAll []       = pure []
interpretAll (x : xs) = do
    x'  <- interpret x
    xs' <- interpretAll xs
    pure (x' : xs')

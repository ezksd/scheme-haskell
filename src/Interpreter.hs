{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Interpreter
    ( repl
    )
where
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
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
import           System.IO.Unsafe

type Interpreter a = StateT Env (Either String) a

repl :: IO ()
repl = do
    putStrLn "press Ctrl+D to quit."
    env <-
        getDataFileName "primit.scm"
        >>= readFile
        >>= (`repl'` [Map.fromList primitives])
    catchIOError
        (loop "" env)
        (\e -> if
            | isEOFError e -> putStrLn "bye ~"
            | otherwise    -> ioError e
        )

repl' :: String -> Env -> IO Env
repl' s env = case runStateT parseAndEval env of
    Left  e      -> putStrLn e >> pure env
    Right (a, s) -> putStr (format a) >> pure s
  where
    parseAndEval =
        maybe (lift (Left "parse error")) pure (parseAll s) >>= interpretAll
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
    space n xs = space (n - 1) ("    " ++ xs)
    indent = if
        | n == 0    -> "> "
        | n > 0     -> space n []
        | otherwise -> "illegal input"
    count  = foldr
        (\x n -> case x of
            '(' -> n + 1
            ')' -> n - 1
            _   -> n
        )
        0
    n      = count s

throw :: String -> Interpreter a
throw = lift . Left

define :: String -> Expr -> Interpreter ()
define k v = modify (\(x : xs) -> Map.insert k v x : xs)

update :: String -> Expr -> Interpreter ()
update k v = get >>= update' []
  where
    update' _   []       = throw ("unbounded identifer :" ++ k)
    update' pre (x : xs) = if
        | Map.member k x -> put (reverse pre ++ (Map.insert k v x : xs))
        | otherwise      -> update' (x : pre) xs

lookup :: String -> Interpreter Expr
lookup k = do 
    env  <- get
    lift ( maybe (err env) pure  (lookup' env))
  where
    err  e   = Left ("unbounded identifer :" ++ k ++ show e)
    lookup' = foldr ((<|>) . Map.lookup k) empty

push :: [(String, Expr)] -> Interpreter ()
push = modify . (:) . Map.fromList

pop :: Interpreter ()
pop = modify tail

merge :: [a] -> [a] -> [a]
merge a b = merge' [] (reverse a) (reverse b)
  where
    merge' t []       _        = t
    merge' t xs       []       = reverse xs ++ t
    merge' t (_ : xs) (y : ys) = merge' (y : t) xs ys

interpret :: Expr -> Interpreter Expr
interpret (Symbol x) = lookup x
interpret (List   x) = case x of
    Symbol "define" : List (Symbol f : xs) : body ->
        get >>= define f . Closure (unSymbols xs) body >> pure Void
    Symbol "lambda" : List xs : body -> Closure (unSymbols xs) body <$> get
    -- Symbol "let" : List binding : body ->
    --     (extract binding >>= push) *> interpretBody body <* pop
    [Symbol "set!"  , Symbol k, v] -> interpret v >>= update k >> pure Void
    [Symbol "define", Symbol k, v] -> interpret v >>= define k >> pure Void
    [Symbol "quote", e]            -> pure e
    [Symbol "if", p, v1, v2]       -> do
        (Bool b) <- interpret p
        interpret (if b then v1 else v2)
    (Symbol "foldr" : xs)          -> do
        [op, init, List xs'] <- interpretAll xs
        foldM (\b a -> interpret (List [op, Lazy a, Lazy b])) init (reverse xs')
    xs                             -> do
        (h : params) <- interpretAll xs
        case h of
            (Closure ps body env') -> push (zip ps params) *> interpretBody body <* pop
            (Func func           ) -> lift (func params)
            _                      -> lift (Left "not a function")
  where
    interpretBody = (last <$>) . interpretAll
    unSymbols =
        ((\case
             (Symbol input) -> input
         ) <$>
        )
    extract []                        = pure []
    extract (List [Symbol s, v] : xs) = do
        e  <- interpret v
        es <- extract xs
        pure ((s, e) : es)
interpret (Lazy y) = pure y
interpret x        = pure x

interpretAll :: [Expr] -> Interpreter [Expr]
-- interpretAll = traverse interpret
interpretAll [] = pure []
interpretAll (x:xs) = do
    x' <- interpret x
    xs' <- interpretAll xs
    pure (x' : xs')

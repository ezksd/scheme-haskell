{-# LANGUAGE LambdaCase #-}
module Interpreter
    ( repl
    )
where
import           System.IO
import           Control.Applicative
import           Control.Monad
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict               as Map
import           Parser
import           Prelude                 hiding ( init
                                                , lookup
                                                )
import           Prims
import           Scheme

type Interpreter a = StateT Env (ExceptT ScmErr IO) a

repl :: IO ()
repl = init >>= void . execStateT loop
    where loop = lift getLine >>= repl' >> loop

repl' :: String -> StateT Env IO ()
repl' s = do
    env <- get
    case parseAll s of
        Nothing -> lift (hPutStrLn stderr "parse error")
        Just a  -> do
            result <- lift (runExceptT (runStateT (interpretAll a) env))
            case result of
                Left  error     -> lift (hPutStrLn stderr error)
                Right (a, env') -> lift (sequence (print <$> a)) >> put env'

init :: IO Env
init =
    readFile "primit.scm"
        >>= (flip execStateT [Map.fromList primitives'] . repl')

define :: String -> Expr -> Interpreter Expr
define k v = modify (\(x:xs) -> Map.insert k v x:xs) >> pure nil
-- define k v = do
--     (x : xs) <- get
--     put (Map.insert k v x : xs)
--     pure nil

lookup :: String -> Interpreter Expr
lookup k = get >>= maybe err pure . lookup'
  where
    err     = lift (throwE ("unbounded identifer :" ++ k))
    lookup' = foldr ((<|>) . Map.lookup k) empty

interpret :: Expr -> Interpreter Expr
interpret (Symbol x) = lookup x
interpret (List   x) = case x of
    Symbol "define" : List (Symbol f : xs) : body ->
        get >>= (define f . Closure (unSymbols xs) body)
    -- [Symbol "set!"  , Symbol k, v] -> interpret v >>= update k
    Symbol "lambda" : List xs : body -> Closure (unSymbols xs) body <$> get
    [Symbol "define", Symbol k, v]   -> interpret v >>= define k
    [Symbol "quote", e]              -> pure e
    [Symbol "if", p, v1, v2]         -> do
        expr <- interpret p
        case expr of
            Bool b -> interpret (if b then v1 else v2)
            _      -> lift (throwE "parameters not match")
    (f : xs) -> do
        h      <- interpret f
        params <- interpretAll xs
        case h of
            (Closure ps body env') -> do
                env <- get
                put (Map.fromList (zip ps params) : env')
                e <- interpretBody body
                put env
                pure e
            (Func func) -> lift (func params)
            _           -> lift (throwE "not a function")
    _ -> lift (throwE "illegal expression")
  where
    interpretBody = (last <$>) . interpretAll
    unSymbols xs =
        (\case
                (Symbol input) -> input
            )
            <$> xs
interpret x = pure x


interpretAll :: [Expr] -> Interpreter [Expr]
interpretAll = sequence . (interpret <$>)

-- interpretBody :: [Expr] -> Interpreter Expr
-- interpretBody []  = throw "empty function body"
-- interpretBody xs' = last <$> interpretAll xs'

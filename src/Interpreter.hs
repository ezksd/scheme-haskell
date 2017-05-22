{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Interpreter (eval,evalAll,getEnv) where
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.IORef
import qualified Data.Map.Strict            as Map
import           Parser
import           Prelude                    hiding (lookup)
import           Prims
import           Scheme


type Interpreter a = ReaderT Env (ExceptT ScmErr IO) a


eval :: Env -> String -> ExceptT ScmErr IO Expr
eval env s = runReaderT (parse' s >>= interpret) env >>= (lift . readIORef)
    where parse' = ReaderT . const . ExceptT . pure .parse

evalAll :: Env -> String -> ExceptT ScmErr IO [Expr]
evalAll env s = runReaderT (parseAll' s >>= intepretAll) env  >>= (lift . unRefs)
    where parseAll' = ReaderT . const . ExceptT . pure . parseAll
          intepretAll = sequence . (interpret <$>)

unRefs :: [IORef a] -> IO [a]
unRefs []     = pure []
unRefs (x:xs) = (:) <$> readIORef x <*> unRefs xs
-- parse' :: String -> Interpreter Expr
-- parse' s= ReaderT (const (ExceptT pure(parse s)))

-- eval :: Env -> Expr -> ExceptT ScmErr IO Expr
-- eval env e = runReaderT (interpret e) env >>= (lift . readIORef)

-- evalAll :: Env -> [Expr] -> ExceptT ScmErr IO [Expr]
-- evalAll env x  = sequence (eval env <$> x)

getEnv :: IO Env
getEnv = primitives >>= (newIORef . Map.fromList) >>= (pure . pure)

nil :: Expr
nil = List []

throw :: ScmErr -> Interpreter (IORef Expr)
throw = lift . throwE

define :: String -> (IORef Expr) -> Interpreter (IORef Expr)
define k v = asks head >>=  (liftIO . runMaybeT . def) >>= maybe (throw ("unboude identifer :" ++ k)) pure
    where def ref' = lift (readIORef ref') >>= (guard . not . Map.member k)
                   >> lift (modifyIORef' ref' (Map.insert k v ) >> (newIORef nil))

lookup :: String -> Interpreter (IORef Expr)
lookup k = ask >>= (liftIO . runMaybeT . lk) >>= maybe (throw ("unboude identifer :" ++ k)) pure
    where lk []     = MaybeT (pure Nothing)
          lk (x:xs) = (lift (readIORef x) >>= (MaybeT . pure . (Map.lookup k))) <|> lk xs

update :: String -> (IORef Expr) -> Interpreter (IORef Expr)
update k v = ask >>= (liftIO . runMaybeT . up) >>= maybe (throw ("unboude identifer :" ++ k)) pure
    where up [] = MaybeT (pure Nothing)
          up (x:xs) = (lift (readIORef x) >>= (guard . (Map.member k)) >>
            lift (modifyIORef' x (Map.adjust (const v) k) >>
            newIORef nil)) <|> up xs

newEnv :: [String] -> [IORef Expr] -> Env ->  Interpreter Env
newEnv keys vals env = (:env) <$> (ins keys vals Map.empty >>= (liftIO . newIORef))
    where   ins (k:ks) (v:vs) frame = ins ks vs (Map.insert k v frame)
            ins [] [] frame         = pure frame
            ins _ _ _               = lift (throwE "parameters not match")

interpret :: Expr -> Interpreter (IORef Expr)
interpret (Symbol x) = lookup x
interpret (List x) = case x of
    Symbol "define" : List (Symbol f : xs) : body -> makeClosuer body xs ask >>= define f
    Symbol "lambda" : List xs : body -> makeClosuer body xs ask
    [Symbol "define",Symbol k,v] -> interpret v >>= define k
    [Symbol "set!",Symbol k,v] -> interpret v >>= update k
    [Symbol "quote",e] -> liftIO (newIORef e)
    [Symbol "eq?",Symbol a,Symbol b] -> (==) <$> lookup a <*> lookup b >>= (liftIO . newIORef . Bool)
    [Symbol "if",p,v1,v2] -> do
        expr <- interpret p >>= (liftIO . readIORef)
        case expr of
            Bool b -> interpret (if b then v1 else v2)
            _      -> lift (throwE "parameters not match")
    (f:xs) -> do
        h <- interpret f >>= (liftIO . readIORef)
        params <- interpretAll xs
        case h of
            (Closure body ps env) -> newEnv ps params env >>= ((`local` interpretBody body) . const)
            (Func func) -> ReaderT (const (func params)) >>= (liftIO . newIORef)
            _ -> lift (throwE "not a function")
    _ -> lift (throwE "illegal expression")
    where interpretAll  = sequence . (interpret <$>)
          interpretBody []  = throw "empty function body"
          interpretBody xs' = last <$> interpretAll xs'
          unSymbols ((Symbol x'):xs) = (x':) <$> unSymbols xs
          unSymbols []               = pure []
          unSymbols _                = lift (throwE "illegal type")
          makeClosuer body xs = (>>= (liftIO . newIORef)) . ((Closure body) <$> unSymbols xs <*>)
interpret x = liftIO (newIORef x)

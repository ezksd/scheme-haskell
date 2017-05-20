{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Interpreter (eval,getEnv) where
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.IORef
import qualified Data.Map.Strict            as Map
import           Prelude                    hiding (lookup)
import           Scheme

type Interpreter a = ReaderT Env (ExceptT ScmErr IO) a

eval :: Env -> Expr -> ExceptT ScmErr IO Expr
eval env e = runReaderT (interpret e) env

getEnv :: IO Env
getEnv = newIORef Map.empty >>= (return . pure)

define :: String -> Expr -> Interpreter ()
define k v = do
    (x:_) <- ask
    m <- liftIO (readIORef x)
    if Map.member k m
        then lift (throwE DuplicateDefinition)
        else do
            ref <- liftIO (newIORef v)
            liftIO (modifyIORef' x (Map.insert k ref))


lookup :: String -> Interpreter Expr
lookup k = do
    (x:xs) <- ask
    m <- liftIO (readIORef x)
    case Map.lookup k m of
        Just e  -> liftIO (readIORef e)
        Nothing -> if null xs
            then lift (throwE UnboundIdentifer)
            else local tail (lookup k)

update :: String -> Expr -> Interpreter ()
update k v = do
    (x:xs) <- ask
    m <- liftIO (readIORef x)
    if Map.member k m
        then do
            ref <- liftIO (newIORef v)
            liftIO (modifyIORef x (Map.adjust (const ref) k))
        else if (null) xs
            then lift (throwE UnboundIdentifer)
            else local tail (update k v)

newFrame :: [String] -> [Expr] -> ExceptT ScmErr IO (IORef Frame)
newFrame keys vals = do frame <- ins keys vals Map.empty
                        lift (newIORef frame)
    where   ins (k:ks) (e:es) frame = do
                ref <- lift (newIORef e)
                ins ks es (Map.insert k ref frame)
            ins [] [] frame = pure frame
            ins _ _ _ = throwE IllegalType

unSymbols :: [Expr] -> Either ScmErr [String]
unSymbols ((Symbol x):xs) = (x:) <$> unSymbols xs
unSymbols []              = pure []
unSymbols _               = Left IllegalType

interpretAll :: [Expr] -> Interpreter [Expr]
interpretAll  = sequence . (interpret <$>)

--- Readert {runReart :: Env -> ExceptT error io a)}

interpret :: Expr -> Interpreter Expr
interpret (Symbol x) = lookup x
interpret (List x) = case x of
    [Symbol "define",Symbol k,v] -> do
        e <- interpret v
        define k e
        pure nil
    Symbol "define" : List xs : body -> do
        env <- ask
        syb <- ReaderT (const (ExceptT (pure (unSymbols xs))))
        case syb of
            (f:ps) -> define f (Closure env ps body) >> pure nil
            _      -> lift (throwE IllegalType)
    Symbol "lambda" : List xs : body -> do
        env <- ask
        ps  <- ReaderT (const (ExceptT (pure (unSymbols xs))))
        pure (Closure env ps body)
    [Symbol "if",p,v1,v2] -> do
        (Bool b) <- interpret p
        interpret (if b then v1 else v2)
    apply -> do
       xs <- interpretAll apply
       case xs of
           ((Closure env ps body):params) -> do
               frame <- ReaderT (const (newFrame ps params))
               rs<- local (const (frame:env)) (interpretAll body)
               case rs of
                   [] -> lift (throwE IllegalType)
                   zs -> pure (last zs)
           _ -> lift (throwE IllegalType)
interpret x = pure x

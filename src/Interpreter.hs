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
import           Prims
import           Scheme

type Interpreter a = ReaderT Env (ExceptT ScmErr IO) a

eval :: Env -> Expr -> ExceptT ScmErr IO Expr
eval env e = do ref <- runReaderT (interpret e) env
                expr <- lift (readIORef ref)
                pure expr

getEnv :: IO Env
getEnv = do prims <- primitives
            frame <- newIORef (Map.fromList prims)
            return [frame]
-- getEnv = newIORef (Map.fromList primitives) >>= (return . pure)

nil :: IO (IORef Expr)
nil = newIORef (List [])

define :: String -> (IORef Expr) -> Interpreter ()
define k v = do
    (x:_) <- ask
    m <- liftIO (readIORef x)
    if Map.member k m
        then lift (throwE DuplicateDefinition)
        else do
            liftIO (modifyIORef' x (Map.insert k v))


lookup :: String -> Interpreter (IORef Expr)
lookup k = do
    (x:xs) <- ask
    m <- liftIO (readIORef x)
    case Map.lookup k m of
        Just e  -> pure e
        Nothing -> if null xs
            then lift (throwE UnboundIdentifer)
            else local tail (lookup k)

update :: String -> Expr -> Interpreter ()
update k v = do
    (x:xs) <- ask
    m <- liftIO (readIORef x)
    case Map.lookup k m of
        Just ref  -> liftIO (writeIORef ref v)
        Nothing -> if null xs
            then lift (throwE UnboundIdentifer)
            else local tail (update k v)


newFrame :: [String] -> [IORef Expr] -> ExceptT ScmErr IO (IORef Frame)
newFrame keys vals = do frame <- ins keys vals Map.empty
                        lift (newIORef frame)
    where   ins (k:ks) (v:vs) frame = do
                ins ks vs (Map.insert k v frame)
            ins [] [] frame = pure frame
            ins _ _ _ = throwE IllegalType

unSymbols :: [Expr] -> Either ScmErr [String]
unSymbols ((Symbol x):xs) = (x:) <$> unSymbols xs
unSymbols []              = pure []
unSymbols _               = Left IllegalType

interpretAll :: [Expr] -> Interpreter [IORef Expr]
interpretAll  = sequence . (interpret <$>)


interpret :: Expr -> Interpreter (IORef Expr)
interpret (Symbol x) = lookup x
interpret (List x) = case x of
    [Symbol "define",Symbol k,v] -> do
        e <- interpret v
        define k e
        liftIO nil
    [Symbol "set!",Symbol k,v] -> do
        ref <- interpret v
        e   <- liftIO (readIORef ref)
        update k e
        liftIO nil
    Symbol "define" : List xs : body -> do
        env <- ask
        syb <- ReaderT (const (ExceptT (pure (unSymbols xs))))
        case syb of
            (f:ps) -> do
                ref <- liftIO (newIORef (Closure env ps body))
                define f ref
                liftIO nil
            _      -> lift (throwE IllegalType)
    Symbol "lambda" : List xs : body -> do
        env <- ask
        ps  <- ReaderT (const (ExceptT (pure (unSymbols xs))))
        liftIO (newIORef (Closure env ps body))
    [Symbol "quote",e] -> liftIO (newIORef e)
    [Symbol "if",p,v1,v2] -> do
        ref <- interpret p
        expr <- liftIO (readIORef ref)
        case expr of
            Bool b ->  interpret (if b then v1 else v2)
            _      -> lift (throwE IllegalType)
    Symbol "eq?":xs -> case xs of
        [Symbol a,Symbol b] -> do aref <- lookup a
                                  bref <- lookup b
                                  liftIO (newIORef (Bool (aref == bref)))
        _ -> liftIO (newIORef  (Bool False))
    apply -> do
       xs <- interpretAll apply
       case xs of
           (h:params) -> do
               v <- liftIO (readIORef h)
               case v of
                   (Closure env ps body) -> do
                        frame <- ReaderT (const (newFrame ps params))
                        rs <- local (const (frame:env)) (interpretAll body)
                        case rs of
                            [] -> lift (throwE IllegalType)
                            zs -> pure (last zs)
                   (Func f) -> do
                       r <- ReaderT (const (f params))
                       liftIO (newIORef r)
                   _ -> lift (throwE IllegalType)
           _ -> lift (throwE IllegalType)
interpret x = liftIO (newIORef x)

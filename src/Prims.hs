module Prims (primitives) where
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.IORef
import           Scheme
-- type IFunc = [IORef Expr] ->  ExceptT ScmErr IO Expr
-- type Frame = Map.Map String (IORef Expr)
-- type Env = [IORef Frame]
unRefs :: [IORef a] -> IO [a]
unRefs []     = pure []
unRefs (x:xs) = (:) <$> readIORef x <*> unRefs xs

wrap :: ([Expr] -> Either ScmErr Expr) -> IFunc
wrap f refs = do ps <- lift (unRefs refs)
                 ExceptT (pure (f ps))


numericOp :: (Int -> Int -> Expr) -> IFunc
numericOp op = wrap (\xs -> case xs of
    [Number a , Number b] -> pure (op a b)
    _                     -> Left ParametersNotMatch)

caculate :: (Int -> Int -> Int) -> IFunc
caculate op = numericOp (\a b -> Number (op a b))

comp :: (Int -> Int -> Bool) -> IFunc
comp op =  numericOp (\a b -> Bool (op a b))


unaryOp :: (Expr -> Either ScmErr Expr) -> IFunc
unaryOp op = wrap (\xs -> case xs of
    [x] ->  op x
    _   -> Left ParametersNotMatch)

binaryOp :: (Expr -> Expr -> Either ScmErr Expr) -> IFunc
binaryOp op = wrap (\xs -> case xs of
    [a,b] -> op a b
    _     -> Left ParametersNotMatch )

primitives :: IO [(String,IORef Expr)]
primitives = trans [("+", caculate (+)),
                    ("-", caculate (-)),
                    ("*", caculate (*)),
                    ("/", caculate (div)),
                    (">", comp (>)),
                    ("=", comp (==)),
                    ("<", comp (<)),
                    ("list?", unaryOp (\x -> case x of
                        List _ -> pure (Bool True)
                        _      -> pure (Bool False))),
                    ("pair?", unaryOp (\x -> case x of
                        Pair _ _ -> pure (Bool True))),
                    ("null?", unaryOp (\x -> case x of
                        List [] -> pure (Bool True)
                        _       -> pure (Bool False))),
                    ("car",unaryOp (\x -> case x of
                        List (a:_) -> pure a
                        Pair a _   -> pure a
                        _          -> Left IllegalType)),
                    ("cdr", unaryOp (\x -> case x of
                        List (_:b) -> pure (List b)
                        Pair _ b   -> pure b
                        _          -> Left IllegalType)),
                    ("cons",binaryOp(\a b -> case b of
                        List bs -> pure (List (a:bs))
                        _       -> pure (Pair a b))),
                    ("display",(\xs -> do
                        as <-  lift (unRefs xs)
                        case as of
                            [x] -> lift (putStrLn (show x)) >> pure (List [])
                            _   -> throwE ParametersNotMatch)),
                    ("newline",(\xs -> do
                        as <- lift (unRefs xs)
                        case as of
                            [] -> lift (putStr "\n") >> pure (List [])
                            _  -> throwE IllegalType))]

trans :: [(String,IFunc)] -> IO [(String,IORef Expr)]
trans [] = pure []
trans ((k,v):xs) = do ref <- newIORef (Func v)
                      t  <-  trans xs
                      pure ((k,ref):t)


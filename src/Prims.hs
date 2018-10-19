{-# LANGUAGE LambdaCase #-}
module Prims
    ( primitives
    , unRefs
    )
where
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.IORef
import           Scheme

unRefs :: [IORef a] -> IO [a]
unRefs = foldr (\ x -> (<*>) ((:) <$> readIORef x)) (pure [])

wrap :: ([Expr] -> Either ScmErr Expr) -> IFunc
wrap f refs = do
    ps <- lift (unRefs refs)
    ExceptT (pure (f ps))

numericOp :: (Int -> Int -> Expr) -> IFunc
numericOp op = wrap
    (\case
        [Number a, Number b] -> pure (op a b)
        _                    -> Left "paramters not match"
    )

unaryOp :: (Expr -> Either ScmErr Expr) -> IFunc
unaryOp op = wrap
    (\case
        [x] -> op x
        _   -> Left "paramters not match"
    )

binaryOp :: (Expr -> Expr -> Either ScmErr Expr) -> IFunc
binaryOp op = wrap
    (\case
        [a, b] -> op a b
        _      -> Left "paramters not match"
    )

and' :: [Expr] -> Either ScmErr Expr
and' []            = pure (Bool True)
and' (Bool b : xs) = if b then and' xs else pure (Bool False)
and' _             = Left "bot boolean type"

or' :: [Expr] -> Either ScmErr Expr
or' []            = pure (Bool True)
or' (Bool b : xs) = if b then pure (Bool True) else or' xs
or' _             = Left "bot boolean type"


primitives :: IO [(String, IORef Expr)]
primitives = trans
    [ ("+", caculate (+))
    , ("-", caculate (-))
    , ("*", caculate (*))
    , ("/", caculate div)
    , (">", comp (>))
    , ("=", comp (==))
    , ("<", comp (<))
    , ( "list?"
      , unaryOp
          (\case
              List _ -> pure (Bool True)
              _      -> pure (Bool False)
          )
      )
    , ( "pair?"
      , unaryOp
          (\case
              Pair _ _ -> pure (Bool True)
          )
      )
    , ( "null?"
      , unaryOp
          (\case
              List [] -> pure (Bool True)
              _       -> pure (Bool False)
          )
      )
    , ( "car"
      , unaryOp
          (\case
              List (a : _) -> pure a
              Pair a _     -> pure a
              _            -> Left "not a pair/list"
          )
      )
    , ( "cdr"
      , unaryOp
          (\case
              List (_ : b) -> pure (List b)
              Pair _ b     -> pure b
              _            -> Left "not a pair/list"
          )
      )
    , ( "cons"
      , binaryOp
          (\a b -> case b of
              List bs -> pure (List (a : bs))
              _       -> pure (Pair a b)
          )
      )
    , ("and", wrap and')
    , ("or" , wrap or')
    , ( "not"
      , unaryOp
          (\case
              Bool b -> pure (Bool (not b))
              _      -> Left "not boolean type"
          )
      )
    , ( "display"
      , \xs -> do
          as <- lift (unRefs xs)
          case as of
              [x] -> lift (print x) >> pure (List [])
              _   -> throwE "parameters not match"
      )
    , ( "newline"
      , \xs -> do
            as <- lift (unRefs xs)
            case as of
                [] -> lift (putStr "\n") >> pure (List [])
                _  -> throwE "parameters not match"
      )
    ]
  where
    caculate op = numericOp (\a b -> Number (op a b))
    comp op = numericOp (\a b -> Bool (op a b))
    trans []            = pure []
    trans ((k, v) : xs) = do
        ref <- newIORef (Func v)
        t   <- trans xs
        pure ((k, ref) : t)





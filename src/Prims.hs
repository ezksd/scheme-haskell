{-# LANGUAGE LambdaCase #-}
module Prims
    ( primitives
    , primitives'
    , unRefs
    )
where
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.IORef
import           Scheme
unRefs :: [IORef a] -> IO [a]
unRefs = foldr (\x -> (<*>) ((:) <$> readIORef x)) (pure [])

wrap :: ([Expr] -> Either ScmErr Expr) -> IFunc
wrap f = ExceptT . pure . f

numericOp :: (Int -> Int -> Expr) -> IFunc
numericOp op = \case
    [Number a, Number b] -> pure (op a b)
    _                    -> throwE "paramters not match"

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
and' _             = Left "not boolean type"

or' :: [Expr] -> Either ScmErr Expr
or' []            = pure (Bool True)
or' (Bool b : xs) = if b then pure (Bool True) else or' xs
or' _             = Left "bot boolean type"

-- trans :: [(a, IFunc)] -> IO [(a, Expr)]
-- trans xs = pure ((\(a, b) -> (a, Func b)) <$> xs)

primitives' :: [(String, Expr)]
primitives' = (\(a, b) -> (a, Func b)) <$> primitives

primitives :: [(String, IFunc)]
primitives =
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
              _ -> pure (Bool False)
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
      , \case
          [x] -> lift (print x) >> pure (List [])
          _   -> throwE "parameters not match"
      )
    , ( "newline"
      , \case
          [] -> lift (putStr "\n") >> pure (List [])
          _  -> throwE "parameters not match"
      )
    ]
  where
    caculate op = numericOp (\a b -> Number (op a b))
    comp op = numericOp (\a b -> Bool (op a b))






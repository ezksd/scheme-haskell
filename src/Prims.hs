{-# LANGUAGE LambdaCase #-}
module Prims
    ( primitives
    )
where
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Scheme
import           System.IO.Unsafe

numericOp :: (Int -> Int -> Expr) -> IFunc
numericOp op = \case
    [Number a, Number b] -> pure (op a b)
    _                    -> Left "paramters not match"

unaryOp :: (Expr -> Either String Expr) -> IFunc
unaryOp op = \case
    [x] -> op x
    _   -> Left "paramters not match"

binaryOp :: (Expr -> Expr -> Either String Expr) -> IFunc
binaryOp op = \case
    [a, b] -> op a b
    _      -> Left "paramters not match"

and' :: [Expr] -> Either String Expr
and' []            = pure (Bool True)
and' (Bool b : xs) = if b then and' xs else pure (Bool False)
and' _             = Left "not boolean type"

or' :: [Expr] -> Either String Expr
or' []            = pure (Bool True)
or' (Bool b : xs) = if b then pure (Bool True) else or' xs
or' _             = Left "bot boolean type"

primitives :: [(String, Expr)]
primitives =
    (\(a, b) -> (a, Func b))
        <$> [ ("+", caculate (+))
            , ("-", caculate (-))
            , ("*", caculate (*))
            , ("/", caculate div)
            , (">", comp (>))
            , ("=", comp (==))
            , ("<", comp (<))
            , ( "list?"
              , \case
                  [List _] -> pure (Bool True)
                  _        -> pure (Bool False)
              )
            , ( "pair?"
              , \case
                  [Pair _ _] -> pure (Bool True)
                  _          -> pure (Bool False)
              )
            , ( "null?"
              , \case
                  [List x] -> pure (Bool (null x))
                  _        -> Left "not a list"
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
              , \case
                  [x, List xs] -> pure (List (x : xs))
                  [a, b      ] -> pure (Pair a b)
                  _            -> Left "illegal parameter"
              )
            , ( "range"
              , \case
                  [Number a, Number b] | a < b ->
                      pure (List (Number <$> [a .. b]))
                  _ -> Left "wrone parameter1"
              )
            , ( "reverse"
              , \case
                  [List xs] -> pure (List (reverse xs))
              )
            , ("and", and')
            , ("or" , or')
            , ( "not"
              , unaryOp
                  (\case
                      Bool b -> pure (Bool (not b))
                      _      -> Left "not boolean type"
                  )
              )
            ]
  where
    caculate op = numericOp (\a b -> Number (op a b))
    comp op = numericOp (\a b -> Bool (op a b))





module Interpreter (eval) where
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict                  as Map
import           Prelude                          hiding (lookup)
import           Scheme

type Interpreter a = StateT Env Maybe a

eval :: Env -> Expr -> Maybe (Expr,Env)
eval env e = runStateT (interp e) env

lookup :: String -> Interpreter Expr
lookup k = do e <- get
              lift (look k e)
    where look _ []     = Nothing
          look s (x:xs) = Map.lookup s x <|> look s xs

define :: String -> Expr -> Interpreter Expr
define k v = do (e:es) <- get
                guard (not (Map.member k e))
                put (Map.insert k v e : es)
                pure nil
-- update target environment with current ant put it to state
updateEnv :: Env -> Interpreter ()
updateEnv env = do (_:es) <- get
                   put (up env es)
    where   up x'@(x:xs) y = if length x' == length y then y else x : up xs y

newEnv :: [String] -> [Expr] -> Interpreter ()
newEnv keys vals = do
    env <- get
    frame <- lift (h keys vals Map.empty)
    put (frame:env)
    where   h [] [] frame         = pure frame
            h (k:ks) (v:vs) frame = do guard $ not $ Map.member k frame
                                       h ks vs (Map.insert k v frame)
            h _ _ _               = Nothing

unSymbols :: [Expr] -> Interpreter [String]
unSymbols xs = lift (sequence (unSymbol <$> xs))
    where unSymbol (Symbol s) = Just s
          unSymbol _          = Nothing

{-# ANN interpParams "HLint: ignore" #-}
interpParams :: [Expr] -> StateT Env Maybe [Expr]
interpParams []     = pure []
interpParams (x:xs) = (:) <$> interp x <*> interpParams xs

interpBody :: [Expr] -> StateT Env Maybe Expr
interpBody []     = lift Nothing
interpBody [x]    = interp x
interpBody (x:xs) = interp x >> interpBody xs

interp :: Expr -> Interpreter Expr
interp (Symbol x ) = lookup x
interp (List x) = case x of
    [Symbol "define",Symbol k,v] -> interp v >>= define k
    Symbol "define" : List (Symbol k : params) : body -> do
        env <- get
        ps  <- unSymbols params
        v <- pure (Closure env ps body)
        define k v
    Symbol "lambda" : List params : body -> do
        env <- get
        ps  <- unSymbols params
        pure (Closure env ps body)
    [Symbol "if",p,v1,v2] -> do
        (Bool b) <- interp p
        interp (if b then v1 else v2)
    xs -> do
        (h:vals) <- interpParams xs
        case h of
            (Closure env1 ps body) -> do
                env <- get
                put env1
                newEnv ps vals
                r <- interpBody body
                put env
                pure r
            (Func f) -> lift (f vals)
            _ -> lift Nothing
interp x = pure x


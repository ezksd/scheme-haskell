module Lib where
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict                  as Map
import           Scheme

-- data Expr = Symbol String
--           | String String
--           | Number Int
--           | Bool Bool
--           | List [Expr]
--           | Closure Env Expr
--     deriving (Show)



eval :: Env -> Expr -> Maybe (Expr,Env)
eval env e = runStateT (interp e) env

evallist :: Env -> [Expr] -> Maybe (Expr,Env)
evallist env es = case es of
    []      -> Nothing
    [x]     -> eval env x
    (x:xs)  -> do (_,e) <- eval env x
                  evallist e xs

unSymbols :: [Expr] -> StateT Env Maybe [String]
unSymbols xs = lift (sequence (unSymbol <$> xs))
    where unSymbol (Symbol s) = Just s
          unSymbol _          = Nothing

define :: String -> Expr -> Env -> StateT Env Maybe Expr
define k v env = do env1 <- lift (insertVar k v env)
                    put env1
                    pure nil

newFrame :: [String] -> [Expr] -> Env -> Maybe Env
newFrame var val env = g var val (Map.empty:env)

g :: [String] -> [Expr] -> Env -> Maybe Env
g [] [] e = pure e
g (p:ps) (v:vs) (e:es) = do e1 <- pure (Map.insert p v e)
                            g ps vs (e1:es)
g _ _ _ = Nothing



interp :: Expr -> StateT Env Maybe Expr
interp e = do
    env <- get
    pure (print e)
    case e of
        (Symbol k) -> lift (lookupVar k env)
        (List l) -> case l of
            [Symbol "define",Symbol k,v] -> do
                r <- interp v
                define k r env
            Symbol "define" : List (Symbol k : params) : body -> do
                ps  <- unSymbols params
                v <- pure (Closure env ps body)
                define k v env
            Symbol "lambda" : List params : body -> do
                ps  <- unSymbols params
                pure (Closure env ps body)
            [Symbol "if",p,v1,v2] -> do
                (Bool b) <- interp p
                interp (if b then v1 else v2)
            xs -> do
                (f:vals) <- interps xs
                case f of
                    (Closure env1 ps body) -> do
                        env2 <- lift (newFrame ps vals env1)
                        put env2
                        r <- interp1 body
                        (_:e) <- get
                        put e
                        pure r
                    (Func f) -> lift (f vals)
                    _ -> lift Nothing
        _ -> pure e

{-# ANN interps "HLint: ignore" #-}
interps :: [Expr] -> StateT Env Maybe [Expr]
interps []     = pure []
interps (x:xs) = (:) <$> interp x <*> interps xs

interp1 :: [Expr] -> StateT Env Maybe Expr
interp1 []     = lift Nothing
interp1 [x]    = interp x
interp1 (x:xs) = interp x >> interp1 xs


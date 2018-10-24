module Scheme where
import           Control.Monad.Trans.Except
import qualified Data.Map.Strict               as Map
import           Prelude                 hiding ( init
                                                , lookup
                                                )

data Expr = Symbol String
          | String String
          | Number Int
          | Bool Bool
          | List [Expr]
          | Pair Expr Expr
          | Closure [String] [Expr] Env
          | Func IFunc
type ScmErr = String

instance Show Expr where
    show (Symbol s) = "variable:" ++ s
    show (String s) = "\"" ++ s ++ "\""
    show (Number i) = show i
    show (Bool b)   = show b
    show (List l) = "(" ++ f l
        where f []     = ")"
              f [x]    = show x ++ ")"
              f (x:xs) = show x ++ "," ++ f xs
    show (Pair a b) = "(" ++ show a ++ " . " ++ show b ++")"
    show Closure{} = "function"
    show (Func _) = "primitive"

nil :: Expr
nil  =  List []

type IFunc = [Expr] ->  ExceptT ScmErr IO Expr
type Frame = Map.Map String  Expr
type Env = [Frame]




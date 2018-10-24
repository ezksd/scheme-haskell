module Scheme where
import           Data.IORef
import qualified Data.Map.Strict               as Map
import           Control.Monad.Trans.Except
data Expr = Symbol String
          | String String
          | Number Int
          | Bool Bool
          | List [Expr]
          | Pair Expr Expr
          | Closure [String] [Expr] Env
          | Func IFunc
          | Void

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
    show Void = mempty

nil :: Expr
nil = List []

type IFunc = [Expr] ->  ExceptT ScmErr IO Expr
type Env = IORef [Map.Map String Expr]


module Scheme where
import           Control.Applicative
import qualified Data.Map.Strict     as Map
import           Prelude             hiding (init, lookup)

data Expr = Symbol String
          | String String
          | Number Int
          | Bool Bool
          | List [Expr]
          | Pair Expr Expr
          | Closure Env [String] [Expr]
          | Func IFunc

instance Show Expr where
    show (Symbol s) = "variable:" ++ s
    show (String s) = "\"" ++ s ++ "\""
    show (Number i) = show i
    show (Bool b)   = show b
    show (List l) = "(" ++ f l
        where f []     = ")"
              f [x]    = show x ++ ")"
              f (x:xs) = show x ++ "," ++ f xs
    show (Closure{}) = "function"
    show (Func _) = "primitive"

type IFunc = [Expr] -> Maybe Expr

type Frame = Map.Map String Expr
type Env = [Frame]


nil :: Expr
nil = List []

env0 :: Env
env0 = [init Map.empty]

unaryOp :: (Expr -> Maybe Expr) -> IFunc
unaryOp f [x] = f x
unaryOp _ _   = Nothing

binaryNumOp ::(Int -> Int -> Int) -> IFunc
binaryNumOp f [Number a,Number b] = pure (Number (f a b ))
binaryNumOp _ _                   = Nothing

def :: String -> IFunc -> Frame -> Frame
def name f = Map.insert name (Func f)

init :: Frame -> Frame
init = def "id" (unaryOp (pure . id))
     . def "+" (binaryNumOp (+))
     . def "-" (binaryNumOp (-))
     . def "*" (binaryNumOp (*))
     . def "/" (binaryNumOp div)
     . def "car" (unaryOp (\e -> case e of
         List (x:_) -> pure x
         List _     -> Nothing))
     . def "cdr" (unaryOp (\e -> case e of
         List (_:xs) -> pure (List xs)
         List _      -> Nothing))




{-# LANGUAGE FlexibleInstances #-}
module Scheme where
import           Control.Applicative
import           Control.Monad
import qualified Data.Map.Strict     as Map
import           Parser


type Env = [Map.Map String Expr]
type IFun = [Expr] -> Maybe Expr

lookup :: IO ()
lookup = putStrLn ""
lookupVar :: String -> Env -> Maybe Expr
lookupVar k (e:es) = Map.lookup k e <|> lookupVar k es
lookupVar k []     = Nothing
-- lookupVar k = foldr ((<|>) . Map.lookup k) Nothing

insertVar :: String -> Expr -> Env -> Maybe Env
insertVar k v (e:es) = guard (not (Map.member k e)) >> pure (Map.insert k v e : es)

contains :: String -> Env -> Bool
contains k = foldr ((&&) . Map.member k) False

instance Show IFun where
    show = const "fun"

data Expr = Symbol String
          | String String
          | Number Int
          | Bool Bool
          | List [Expr]
          | Closure Env [String] [Expr]
          | Func IFun
    deriving  (Show)
nil :: Expr
nil = List []

isClosure :: Expr -> Bool
isClosure Closure{} = True
isClosure _         = False


env0 :: Env
env0 = [init1 Map.empty]

unaryOp :: (Expr -> Expr) -> IFun
unaryOp f [x] = pure ( f x)
unaryOp _ _   = Nothing

binaryNumOp ::(Int -> Int -> Int) -> IFun
binaryNumOp f [Number a,Number b] = pure (Number (f a b ))
binaryNumOp _ _                   = Nothing



ins :: String -> IFun -> Map.Map String Expr -> Map.Map String Expr
ins name f = Map.insert name (Func f)


init1 :: Map.Map String Expr -> Map.Map String Expr
init1 = ins "id" (unaryOp id)
     . ins "+" (binaryNumOp (+))
     . ins "-" (binaryNumOp (-))
     . ins "*" (binaryNumOp (*))
    --  . ins "t" List [Symbol "lambda",List [Symbol "i"],Symbol "i"]
    --  List [Symbol "lambda",List [Symbol "i"],Symbol "i"]


symbol :: Parser Expr
symbol = Symbol <$> s
    where s = some $ notIn " \r\n\"\'#()"

string :: Parser Expr
string = String <$> (char '"' *> letter <* char '"')

number :: Parser Expr
number = Number <$> int

bool :: Parser Expr
bool = Bool <$> (char '#' *> (t <|> f))
    where t = char 't' >> pure True
          f = char 'f' >> pure False

list :: Parser Expr
list = List <$> (char '(' *> x <* char ')')
    where x = many $ token expr

expr :: Parser Expr
expr = string <|> bool <|> number <|> symbol <|> list

parseExpr :: String -> Maybe Expr
parseExpr = runParse expr


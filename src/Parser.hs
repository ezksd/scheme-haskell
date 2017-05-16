module Parser where
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State.Strict
import           Data.Char
import           Scheme

type Parser a = StateT String Maybe a

runParser :: Parser a -> String -> Maybe a
runParser = runStateT

runParse :: Parser a -> String -> Maybe a
runParse p s =  do (a,b) <- runStateT p s
                   guard (null b)
                   return a

item :: Parser Char
item = do s <- get
          guard (not (null s))
          put (tail s)
          return (head s)

test :: Parser a -> (a -> Bool) -> Parser a
test m p = do a <- m
              guard (p a)
              return a

sat :: (Char -> Bool) -> Parser Char
sat = test item

char :: Char -> Parser Char
char = sat . (==)

letter :: Parser String
letter = some (sat isAlpha)

int :: Parser Int
int = do sign <- (char '-' >> pure negate) <|> pure id
         num <- read <$> some (sat isDigit)
         return (sign num)

oneOf :: String -> Parser Char
oneOf = sat . flip elem

notIn :: String -> Parser Char
notIn = sat . flip notElem

token :: Parser a -> Parser a
token = (>>) spaces
    where spaces = many $ sat isSpace


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



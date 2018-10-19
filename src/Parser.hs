module Parser
    ( parse
    , parseAll
    )
where
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class

import           Data.Char
import           Scheme
-- type Parser a = StateT String Maybe a
type Parser a = StateT String (ExceptT String IO) a

consumed :: Parser a -> Parser a
-- expr' = lift (catchE (evalStateT (expr <* (get >>= guard . null))) (const (throwE "parse error...")))
consumed p = do
    e <- p
    s <- get
    case s of
        "" -> pure e
        _  -> lift (throwE "parse error")

-- parse :: String -> Either ScmErr Expr
-- parse = maybe (Left "parse error...") pure . evalStateT expr'
parse :: String -> ExceptT String IO Expr
parse = evalStateT (consumed expr)

-- parseAll :: String -> Either ScmErr [Expr]
-- parseAll = maybe (Left "parse error...") pure . evalStateT (some (token expr'))
parseAll :: String -> ExceptT String IO [Expr]
parseAll = evalStateT (consumed (many (token expr)))

item :: Parser Char
item = do
    (x : xs) <- get
    put xs
    return x

test' :: (a -> Bool) -> Parser a -> Parser a
test' = mfilter

sat :: (Char -> Bool) -> Parser Char
sat = flip test' item

char :: Char -> Parser Char
char = sat . (==)

letter :: Parser String
letter = many (sat isAlpha)

int :: Parser Int
int = negate <$> (char '-' >> nat) <|> nat
    where nat = read <$> some (sat isDigit)

notIn :: String -> Parser Char
notIn = sat . flip notElem

token :: Parser a -> Parser a
token = (>>) spaces where spaces = many $ sat isSpace

symbol :: Parser Expr
symbol = Symbol <$> s where s = some $ notIn " \r\n\"\'#()."

string :: Parser Expr
string = String <$> (char '"' *> letter <* char '"')

number :: Parser Expr
number = Number <$> int

bool :: Parser Expr
bool = Bool <$> (char '#' *> (t <|> f))
  where
    t = char 't' >> pure True
    f = char 'f' >> pure False

list :: Parser Expr
list = List <$> (char '(' *> x <* token (char ')'))
    where x = many $ token expr

pair :: Parser Expr
pair = Pair <$> left <*> right
  where
    left  = char '(' *> token expr <* token (char '.')
    right = token expr <* token (char ')')

quote :: Parser Expr
quote = (\e -> List [Symbol "quote", e]) <$> (char '\'' *> expr)

expr :: Parser Expr
expr = string <|> bool <|> number <|> symbol <|> list <|> pair <|> quote


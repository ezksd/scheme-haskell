module Parser
    ( parse
    , parseAll
    )
where
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State.Strict
import           Data.Char
import           Scheme
-- type Parser a = StateT String Maybe a
type Parser a = StateT String Maybe a

consumed :: Parser a -> Parser a
consumed p = p <* (get >>= (guard . null))

parse :: String -> Maybe Expr
parse = evalStateT (consumed (expr <* spaces))
-- parse = evalStateT expr
parseAll :: String -> Maybe [Expr]
parseAll = evalStateT (consumed (many (spaces *> expr) <* spaces))
-- parseAll = evalStateT (some (token expr))

item :: Parser Char
item = do
    (x : xs) <- get
    put xs
    return x

sat :: (Char -> Bool) -> Parser Char
sat = flip mfilter item

char :: Char -> Parser Char
char = sat . (==)

letter :: Parser String
letter = many (sat (\x -> isAlpha x || isDigit x))

int :: Parser Int
int = negate <$> (char '-' *> nat) <|> nat
    where nat = read <$> some (sat isDigit)

notIn :: String -> Parser Char
notIn = sat . flip notElem

spaces :: Parser ()
spaces = many (sat isSpace) >> pure ()

token :: Parser a -> Parser a
token = (*>) spaces

symbol :: Parser Expr
symbol = Symbol <$> s where s = some $ notIn " \r\n\"\',#()."

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

listLiteral :: Parser Expr
listLiteral = List <$> ((:) <$> (char '(' *> expr) <*> (rest <* char ')'))
    where rest = many (char ',' *> expr)

pair :: Parser Expr
pair = Pair <$> left <*> right
  where
    left  = char '(' *> token expr <* token (char '.')
    right = token expr <* token (char ')')

quote :: Parser Expr
quote = (\e -> List [Symbol "quote", e]) <$> (char '\'' *> expr)

expr :: Parser Expr
expr = string <|> bool <|> number <|> symbol <|> listLiteral <|> list <|> pair <|> quote


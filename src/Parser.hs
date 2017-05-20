module Parser (parse,parseAll) where
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State.Strict
import           Data.Char
import           Scheme
type Parser a = StateT String Maybe a

runParser :: Parser Expr -> String -> Maybe (Expr, String)
runParser = runStateT

parse :: String -> Either ScmErr Expr
parse s = convert $ do (a,rest) <- runParser expr s
                       guard $ null rest
                       pure a
    where convert m = case m of
                        Just x  -> pure x
                        Nothing -> Left ParseError


parseAll :: String -> Maybe [Expr]
parseAll s = do (a,s1) <- runParser expr s
                as <- parseAll s1
                pure (a :as)

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

notIn :: String -> Parser Char
notIn = sat . flip notElem

token :: Parser a -> Parser a
token = (>>) spaces
    where spaces = many $ sat isSpace


symbol :: Parser Expr
symbol = Symbol <$> s
    where s = some $ notIn " \r\n\"\'#()."

string :: Parser Expr
string = String <$> (char '"' *> letter <* char '"')

number :: Parser Expr
number = Number <$> int

bool :: Parser Expr
bool = Bool <$> (char '#' *> (t <|> f))
    where t = char 't' >> pure True
          f = char 'f' >> pure False

list :: Parser Expr
list = List <$> (char '(' *> x <* (token $ char ')'))
    where x = many $ token expr

pair :: Parser Expr
pair = Pair <$> left <*> right
    where left = char '(' *> token expr <* token (char '.')
          right = token expr <* token (char ')')

expr :: Parser Expr
expr = string <|> bool <|> number <|> symbol <|> list <|> pair


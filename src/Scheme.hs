module Scheme where
import           Control.Monad.Trans.Except
import           Data.IORef
import qualified Data.Map.Strict            as Map
import           Prelude                    hiding (init, lookup)

data Expr = Symbol String
          | String String
          | Number Int
          | Bool Bool
          | List [Expr]
          | Pair Expr Expr
          | Closure Env [String] [Expr]
          | Func IFunc

data ScmErr = ParseError
            | IllegalType
            | UnboundIdentifer
            | DuplicateDefinition
            deriving (Show)

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

type IFunc = [Expr] ->  ExceptT ScmErr IO Expr
type Frame = Map.Map String (IORef Expr)
type Env = [IORef Frame]

nil :: Expr
nil = List []




-- nil :: Expr
-- nil = List []

-- env0 :: IO Env
-- env0 =  do a <- newIORef Map.empty
--            pure [a]

-- define :: String -> Expr -> Env -> ExceptT ScmErr IO ()
-- define k v (e:_) = do ref <- liftIO newIORef v
--                       modifyIORef e (Map.insert k ref)

-- update :: String -> Expr -> Env -> IO ()


-- update :: String -> Expr -> E

-- unaryOp :: (Expr -> Maybe Expr) -> IFunc
-- unaryOp f [x] = f x
-- unaryOp _ _   = Nothing

-- binaryNumOp ::(Int -> Int -> Int) -> IFunc
-- binaryNumOp f [Number a,Number b] = pure (Number (f a b ))
-- binaryNumOp _ _                   = Nothing

-- def :: String -> IFunc -> Frame -> Frame
-- def name f = Map.insert name (Func f)

-- init :: Frame -> Frame
-- init = def "id" (unaryOp (pure . id))
--      . def "+" (binaryNumOp (+))
--      . def "-" (binaryNumOp (-))
--      . def "*" (binaryNumOp (*))
--      . def "/" (binaryNumOp div)
--      . def "car" (unaryOp (\e -> case e of
--          List (x:_) -> pure x
--          List _     -> Nothing))
--      . def "cdr" (unaryOp (\e -> case e of
--          List (_:xs) -> pure (List xs)
--          List _      -> Nothing))




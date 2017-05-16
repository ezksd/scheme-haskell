module Main where
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Lib
import           Scheme
main :: IO ()
main =  void $ runMaybeT(loop env0)

loop :: Env -> MaybeT IO ()
loop env = do s <- lift getLine
              guard (not (null s))
              f s
    where f s = (do e <-MaybeT(pure (parseExpr s))
                    (r,env1)  <- MaybeT(pure (eval env e))
                    lift (print r)
                    loop env1)
                <|> loop env



-- f :: String -> MaybeT IO ()
-- f s = (do e <-MaybeT(pure (parseExpr s))
--           (r,env1)  <- MaybeT(pure (eval env0 e))
--           lift (print r)
--           loop env1)
--        <|> loop env0




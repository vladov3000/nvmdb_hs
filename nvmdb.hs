import Data.Char           (isSpace)
import Data.Maybe          (listToMaybe)
import Data.Set            (Set)
import Data.Functor        ((<&>), ($>))
import Control.Applicative (Alternative, empty, (<|>), many, some)
import Control.Monad       (when)

import qualified Data.Set as Set

newtype Parser i a = Parser { runParser :: [i] -> Maybe (a, [i]) }

instance Functor (Parser i) where
  fmap f p = Parser $ \input -> do
    (value, rest) <- runParser p input
    pure (f value, rest)
    
instance Applicative (Parser i) where
  pure x  = Parser $ \input -> pure (x, input)
  f <*> p = Parser $ \input -> do
    (f', rest)     <- runParser f input
    (value, rest') <- runParser p rest
    pure (f' value, rest')
    
instance Monad (Parser i) where
  p >>= f = Parser $ \input -> do
    (value, rest) <- runParser p input
    runParser (f value) rest

instance Alternative (Parser i) where
  empty    = Parser $ const empty
  p <|> p' = Parser $ \input -> (runParser p input) <|> (runParser p' input)

instance Semigroup a => Semigroup (Parser i a) where
  p <> p' = Parser $ \input -> do
    (value,  rest)  <- runParser p  input
    (value', rest') <- runParser p' rest
    pure (value <> value', rest')

instance Monoid a => Monoid (Parser i a) where
  mempty = pure mempty

satisfy :: (i -> Bool) -> Parser i i
satisfy f = Parser $ \input -> do
  first <- listToMaybe input
  if f first then Just (first, tail input) else Nothing

char :: Eq i => i -> Parser i i
char i = satisfy (== i)

string :: Eq i => [i] -> Parser i [i]
string = traverse char

eof :: Parser i ()
eof = Parser $ \input -> if null input then Just ((), input) else Nothing

whitespace :: Parser Char ()
whitespace = many (satisfy isSpace) $> ()

literal :: String -> Parser Char ()
literal s = whitespace *> string s <* whitespace $> ()

token :: Parser Char String
token = whitespace *> (some $ satisfy $ not . isSpace) <* whitespace

data Command = Insert String | Find String deriving (Show, Eq)

commandParser :: Parser Char Command
commandParser = (insert <|> find) <* eof
  where insert     = literal "insert" *>
                     common <*
                     field "customer" <*
                     field "count" <&>
                     tail <&>
                     Insert
        find       = literal "find" *> common <&> tail <&> Find
        common     = field "hotelId" <> field "inDate" <> field "outDate"
        field name = literal name *> pure " " <> token
        
repl :: Set String -> IO (Set String)
repl set = do
  line <- getLine
  let command = fst <$> runParser commandParser line
  case command of
    Just (Insert s) -> pure (Set.insert s set) <* putStrLn "insert OK"
    Just (Find   s) -> pure set <* when (Set.member s set) 
                       (putStrLn "find FOUND")
    Nothing         -> pure set <* putStrLn "unknown input"

main :: IO ()
main = go Set.empty
  where go state = repl state >>= go


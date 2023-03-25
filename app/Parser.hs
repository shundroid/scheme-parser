module Parser where
import Control.Monad.State
import Data.List
import Data.Char
import qualified Data.Map.Strict as Map
import Control.Monad.ST (ST)
import Debug.Trace (trace)

data Node = Number Int | Boolean Bool | String String | Label String | Parens [Node] | Quote Node deriving (Show)

-- (<|>) :: StateT s (Either Memos) a -> StateT s (Either Memos) a -> StateT s (Either Memos) a
-- (StateT a) <|> (StateT b) = StateT $ \s ->
--   a s <|> b s where
--     L <|> x = x
--     y <|> _ = y

type Memos = Map.Map (Int, String) (Maybe (Int, Node))
findMemo :: Memos -> Int -> String -> Maybe (Maybe (Int, Node))
findMemo memos i label = Map.lookup (i, label) memos
addMemo :: Memos -> Int -> String -> Maybe (Int, Node) -> Memos
addMemo memos i label result = Map.insert (i, label) result memos

initMemos :: Memos
initMemos = Map.empty

type ParseState = (Int, Memos, String)
useMemo :: String
  -> StateT ParseState (Either Memos) Node
  -> StateT ParseState (Either Memos) Node
useMemo label p = StateT $ \(i, memos, xs) -> case findMemo memos i label of
  Just (Just (i', node)) -> Right (node, (i', memos, drop (i' - i) xs))
  Just Nothing -> Left memos
  Nothing -> case runStateT p (i, memos, xs) of
    Left memos -> Left (addMemo memos i label Nothing)
    Right (a, (i', memos, xs)) -> Right (a, (i', addMemo memos i label (Just (i', a)), xs))

(<|>) :: StateT ParseState (Either Memos) t -> StateT ParseState (Either Memos) t -> StateT ParseState (Either Memos) t
(StateT a) <|> (StateT b) = StateT $ \(i, memos, xs) -> case a (i, memos, xs) of
    Left memos -> b (i, memos, xs)
    y -> y

string :: String -> StateT ParseState (Either Memos) String
string s = do
  StateT $ \(i, memos, xs) -> if s `isPrefixOf` xs then Right ((), (i, memos, xs)) else Left memos
  modify $ \(i, memos, xs) -> (i + length s, memos, drop (length s) xs)
  return s

digit :: StateT ParseState (Either Memos) String
digit = StateT digit where
  digit (i, memos, x:xs) = if isDigit x then Right ([x], (i + 1, memos, xs)) else Left memos
  digit (_, memos, _) = Left memos

many p = StateT inner where
  inner s = case runStateT p s of
    Right (x', s') -> case runStateT (many p) s' of
      Right (x'', s'') -> Right (x' ++ x'', s'')
      Left memos -> let (i', _, xs') = s' in Right (x', (i', memos, xs'))
    Left memos -> let (i, _, xs) = s in Right ([], (i, memos, xs))

oneOrMore p = do
  x <- p
  xs <- many p
  return $ x ++ xs

anyChar = StateT inner where
  inner (i, memos, x:xs) = Right ([x], (i + 1, memos, xs))
  inner (_, memos, _) = Left memos

char c = StateT inner where
  inner (i, memos, x:xs) = if x == c then Right ([x], (i + 1, memos, xs)) else Left memos
  inner (_, memos, _) = Left memos

charNot p = StateT inner where
  inner (i, memos, x:xs) = case runStateT p (i, memos, x:xs) of
    Left memos -> Right ([x], (i + 1, memos, xs))
    Right (_, (_, memos, _)) -> Left memos
  inner (_, memos, _) = Left memos

optional p = StateT $ \s -> case runStateT p s of
  Left memos -> let (i, _, xs) = s in Right ([], (i, memos, xs))
  Right x -> Right x

sBoolean = useMemo "Boolean" (do
    string "#t"
    return $ Boolean True
  <|> do
    string "#f"
    return $ Boolean False)

sNumber = useMemo "Number" $ do
  x <- oneOrMore digit
  return $ Number (read x :: Int)

sLabel = useMemo "Label" $ do
  label <- oneOrMore $ charNot $ char ' ' <|> char '\t' <|> char '\n' <|> char '(' <|> char ')'
  return $ Label label

sString = useMemo "String" $ do
  char '\"'
  x <- many $ (do
    char '\\'
    anyChar) <|> charNot (char '\\' <|> char '"' <|> char '\n')
  char '\"'
  return $ String x

sQuote = useMemo "Quote" $ do
  char '\''
  optional sBlank
  Quote <$> sExp

sValue :: StateT ParseState (Either Memos) Node
sValue = useMemo "Value" $ sNumber <|> sBoolean <|> sString <|> sQuote

sBlank = oneOrMore $ char ' ' <|> char '\t' <|> char '\n'

sParens = useMemo "Parens" $ (do
  char '('
  optional sBlank
  exps <- sExps
  optional sBlank
  char ')'
  return $ Parens exps)
  <|> do
    string "()"
    return $ Parens []

sExp = useMemo "Exp" $ (sValue <|> sLabel) <|> sParens
sExps :: StateT ParseState (Either Memos) [Node]
sExps = (do
  exp <- sExp
  sBlank
  exps <- sExps
  return (exp:exps)) <|> do
    exp <- sExp
    return [exp]

sBreakLine = do
  char '\n'
  return []

sComment = do
  char ';'
  many $ charNot $ char '\n'
  return []

sLine :: StateT ParseState (Either Memos) [Node]
sLine = sComment <|> (do
  exp <- sExp
  many sBlank
  sComment
  return [exp]) <|> (return <$> sExp)

sFile :: StateT ParseState (Either Memos) [Node]
sFile = do
  many sBreakLine
  exps1 <- sLine
  (do
    oneOrMore sBreakLine
    exps2 <- sFile
    return $ exps1 ++ exps2) <|> (do
      many sBreakLine
      -- char '$'
      return exps1)

parse file = evalStateT sFile $ (0, initMemos, file) -- ++ "$"

test = do
  print $ runStateT sFile $ (0, initMemos, "(1 2 3)") -- (let-eval env exp)\n" ++
    -- "(if (correct-syntax? 'let exp)\n" ++ 
    -- "(base-eval env (let->app exp))\n" ++ 
    -- "(eval-error env 'syntax-error exp)))"


module Parser where
import Control.Monad.State
import Data.List
import Data.Char
import qualified Data.Map.Strict as Map
import Control.Monad.ST (ST)
import Runner (Node (..))
import qualified Data.Text as Text

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

type ParseState = (Int, Memos, Text.Text)
useMemo :: String
  -> StateT ParseState (Either Memos) Node
  -> StateT ParseState (Either Memos) Node
useMemo label p = StateT $ \(i, memos, xs) -> case findMemo memos i label of
  Just (Just (i', node)) -> Right (node, (i', memos, Text.drop (i' - i) xs))
  Just Nothing -> Left memos
  Nothing -> case runStateT p (i, memos, xs) of
    Left memos -> Left (addMemo memos i label Nothing)
    Right (a, (i', memos, xs)) -> Right (a, (i', addMemo memos i label (Just (i', a)), xs))

(<|>) :: StateT ParseState (Either Memos) t -> StateT ParseState (Either Memos) t -> StateT ParseState (Either Memos) t
(StateT a) <|> (StateT b) = StateT $ \(i, memos, xs) -> case a (i, memos, xs) of
    Left memos -> b (i, memos, xs)
    y -> y

forward :: Int -> StateT ParseState (Either Memos) ()
forward i' = do
  (i, memos, text) <- get
  put (i + i', memos, Text.drop i' text)

string :: String -> StateT ParseState (Either Memos) String
string s = do
  (_, memos, xs) <- get
  unless (Text.pack s `Text.isPrefixOf` xs) $ lift $ Left memos
  forward $ length s
  return s

guardText :: StateT ParseState (Either Memos) (Char, Text.Text)
guardText = do
  (i, memos, text) <- get
  when (Text.length text == 0) $ lift $ Left memos
  return (Text.head text, Text.tail text)

returnFail :: StateT ParseState (Either Memos) t
returnFail = do
  (_, memos, _) <- get
  lift $ Left memos

digit :: StateT ParseState (Either Memos) String
digit = do
  (x, _) <- guardText
  unless (isDigit x) returnFail
  forward 1
  return [x]

many :: StateT ParseState (Either Memos) [t] -> StateT ParseState (Either Memos) [t]
many p = StateT inner where
  inner s = case runStateT p s of
    Right (x', s') -> case runStateT (many p) s' of
      Right (x'', s'') -> Right (x' ++ x'', s'')
      Left memos -> let (i', _, xs') = s' in Right (x', (i', memos, xs'))
    Left memos -> let (i, _, xs) = s in Right ([], (i, memos, xs))

oneOrMore :: StateT ParseState (Either Memos) [t] -> StateT ParseState (Either Memos) [t]
oneOrMore p = do
  x <- p
  xs <- many p
  return $ x ++ xs

anyChar :: StateT ParseState (Either Memos) String
anyChar = do
  (x, _) <- guardText
  forward 1
  return [x]

char :: Char -> StateT ParseState (Either Memos) String
char c = do
  (x, _) <- guardText
  unless (x == c) returnFail
  forward 1
  return [x]

charNot :: StateT ParseState (Either Memos) String -> StateT ParseState (Either Memos) String
charNot p = do
  (x, _) <- guardText
  parseState <- get
  case runStateT p parseState of
    Left memos -> do
      forward 1
      return [x]
    Right (_, (_, memos, _)) -> returnFail

optional p = StateT $ \s -> case runStateT p s of
  Left memos -> let (i, _, text) = s in Right ([], (i, memos, text))
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

parse :: Text.Text -> Either Memos [Node]
parse file = evalStateT sFile (0, initMemos, file) -- ++ "$"

test = do
  print $ runStateT sFile (0, initMemos, Text.pack "(1 2 3)") -- (let-eval env exp)\n" ++
    -- "(if (correct-syntax? 'let exp)\n" ++ 
    -- "(base-eval env (let->app exp))\n" ++ 
    -- "(eval-error env 'syntax-error exp)))"


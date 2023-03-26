{-# LANGUAGE LambdaCase #-}

module Runner where

import Control.Monad
import Control.Monad.State.Strict

-- Environments
type Frame = [(String, Node)]
type Env = [Frame]

emptyFrame :: Frame
emptyFrame = []
update var val frame = (var, val):frame
lookupFrame var ((var', val'):frame') = if var == var' then Just val' else lookupFrame var frame'
lookupFrame var _ = Nothing

makeEnv :: Env
makeEnv = [emptyFrame]
extendEnv env = emptyFrame:env
defineVar :: String -> Node -> Env -> Env
defineVar var val (frame:rest) = update var val frame : rest
defineVarM var val = StateT $ \env -> Right ((), defineVar var val env)
bulkDefineVars (var:vars) (val:vals) env = bulkDefineVars vars vals $ defineVar var val env
bulkDefineVars _ _ env = env
lookupEnv var (frame:rest) = case lookupFrame var frame of
  Just val' -> Just val'
  Nothing -> lookupEnv var rest
lookupEnv var _ = Nothing

data Node =
  Number Int |
  Boolean Bool |
  String String |
  Label String |
  Parens [Node] |
  Quote Node |
  Closure Env [String] [Node] |
  Primitive Int PrFn |
  Undefined |
  PairLast Node |
  Command Node deriving (Show)

newtype PrFn = PrFn ([Node] -> Either String Node)
instance Show PrFn where
  show _ = "PrFn"

baseEval :: Node -> StateT Env (Either String) Node
baseEval exp
 | isConstant exp = StateT $ \env -> Right (exp, env)
 | isLabel exp = varEval exp
 | isQuote exp = let Quote node = exp in return node
 | isParens exp = case exp of
    Parens (Label label:rest) -> (case label of
      "exit" -> return $ Command $ Label "exit"
      "define" -> defEval exp
      "let" -> letEval exp
      "letrec" -> letrecEval exp
      "let*" -> letrecEval exp
      "lambda" -> lambdaEval exp
      "if" -> ifEval exp
      "cond" -> condEval exp
      "and" -> andEval exp
      "or" -> orEval exp
      "begin" -> beginEval exp
      "quote" -> quoteEval exp
      _ -> appEval exp)
    _ -> appEval exp
 | otherwise = returnError "unknown data" exp
eval exp = runStateT (baseEval exp)

isConstant :: Node -> Bool
isConstant exp = case exp of
  Boolean _ -> True
  Number _ -> True
  String _ -> True
  _ -> False

isQuote (Quote _) = True
isQuote _ = False

isLabel (Label _) = True
isLabel _ = False

isParens (Parens _) = True
isParens _ = False

returnError name exp = StateT $ \s -> Left $ "SyntaxError: Invalid " ++ name ++ ": " ++ show exp

letEval :: Node -> StateT Env (Either String) Node
letEval exp = case exp of
  Parens (_:Parens defs:body) -> do
    defNames <- forM defs $ \case
      (Parens [name, value]) -> lift $ Right name
      _ -> lift $ Left $ "Invalid let.defs: " ++ show defs 
    defValues <- forM defs $ \case
      (Parens [name, value]) -> lift $ Right value
      _ -> lift $ Left $ "Invalid let.defs: " ++ show defs
    appEval $ Parens (Parens ([Label "lambda", Parens defNames] ++ body):defValues)
  _ -> returnError "let" exp

defEval :: Node -> StateT Env (Either String) Node
defEval exp = case exp of
  Parens [_, var, body] -> case var of
    Label label -> do
      val <- baseEval body
      modify (defineVar label val)
      return val
    Parens ((Label fnName):args) -> do
      defEval $ Parens [Label "define", Label fnName, Parens [Label "lambda", Parens args, body]]
    _ -> returnError "eval" exp
  _ -> returnError "eval" exp

varEval :: Node -> StateT Env (Either String) Node
varEval exp = case exp of
  Label label -> StateT $ \env -> case lookupEnv label env of
    Just val -> Right (val, env)
    Nothing -> Left $ "ReferenceError: " ++ label ++ " is undefined."
  _ -> returnError "var" exp

lambdaEval :: Node -> StateT Env (Either String) Node
lambdaEval exp = case exp of
  Parens (_:Parens args:body) -> do
    names <- forM args $ \case
      Label name -> lift $ Right name
      _ -> lift $ Left $ "SyntaxError: invalid lambda" ++ show exp
    env <- get
    return $ Closure env names body
  _ -> returnError "lambda" exp

beginEval :: Node -> StateT Env (Either String) Node
beginEval exp = case exp of
  Parens (_:body) -> do
    evaled <- forM body baseEval
    return $ last evaled
  _ -> returnError "begin" exp

appEval :: Node -> StateT Env (Either String) Node
appEval exp = case exp of
  Parens (fn:args) -> do -- args may be []
    evaledFn <- baseEval fn
    evaledArgs <- forM args baseEval
    baseApply evaledFn evaledArgs

baseApply fun argVals = case fun of
  Closure env' argVars nodes -> if length argVals /= length argVars then
    returnError "baseApply" (fun, argVals) else
    StateT $ \env -> let
      exp = Parens (Label "begin":nodes)
      runEnv = bulkDefineVars argVars argVals $ extendEnv env'
      result = runStateT (beginEval exp) runEnv in
        case result of
          Right (node, _) -> Right (node, env)
          Left error -> Left error
  Primitive arity (PrFn fn) -> if arity /= -1 && length argVals /= arity then returnError "baseApply" argVals
    else StateT $ \env -> case fn argVals of
      Right node -> Right (node, env)
      Left error -> Left error

isTrueNode node = case node of
  Boolean value -> value
  _ -> True

ifEval exp = case exp of
  Parens [_, cond, p1, p2] -> do
    evaled <- baseEval cond
    if isTrueNode evaled then baseEval p1 else baseEval p2
  Parens [_, cond, p] -> do
    evaled <- baseEval cond
    if isTrueNode evaled then baseEval p else return Undefined
  _ -> returnError "if" exp

orEval exp = case exp of
  Parens (_:cond:rest) -> do
    evaled <- baseEval cond
    if isTrueNode evaled
      then return $ Boolean True
      else orEval $ Parens (Label "or":rest)
  Parens _ -> return $ Boolean False
  _ -> returnError "or" exp

andEval exp = case exp of
  Parens (_:cond:rest) -> do
    evaled <- baseEval cond
    if isTrueNode evaled
      then andEval $ Parens (Label "and":rest)
      else return $ Boolean False
  Parens _ -> return $ Boolean True
  _ -> returnError "and" exp

condEval exp = case exp of
  Parens (_:condCase:rest) -> case condCase of
    Parens [Label "else", p] -> baseEval p
    Parens [cond, p] -> do
      evaled <- baseEval cond
      if isTrueNode evaled
        then baseEval p
        else condEval $ Parens (Label "cond":rest)
    _ -> returnError "cond" exp
  Parens _ -> return Undefined
  _ -> returnError "cond" exp

quoteEval exp = case exp of
  Parens [_, quote] -> return quote
  _ -> returnError "quote" exp

letrecEval exp = case exp of
  Parens (_:Parens defs:body) -> do
    newDefs <- forM defs $ \case
      Parens [Label varName, val] -> return $ Parens [Label "define", Label varName, val]
      _ -> lift $ Left $ "Invalid letrec.defs: " ++ show defs
    baseEval $ Parens $ [Label "let", Parens []] ++ newDefs ++ body

{-# LANGUAGE LambdaCase #-}

module Runner where

import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Graph as Graph

-- Environments
type Frame = (Int, [(String, Node)])
type Env = [Frame]

emptyFrame :: Int -> Frame
emptyFrame next = (next, [])
update var val (index, frameInner) = (index, (var, val):frameInner)
-- update var val (index, frameInner) = (index, updateClosure (var, val):map updateClosure frameInner) where
--   updateClosure (var', val') = case val' of
--     Closure env args nodes -> (var', Closure (updateEnv env) args nodes)
--     other -> (var', other)
--   updateEnv (RealEnv frames) = RealEnv (updateFrames frames)
--   updateEnv env = env
--   updateFrames :: [Frame] -> [Frame]
--   updateFrames ((j, frameInner'):rest)
--     | j > index = (j, frameInner'):updateFrames rest
--     | j == index = (j, (var, val):map updateClosure frameInner'):rest
--     | otherwise = rest
--   updateFrames [] = []
lookupFrame var (_, frameInner) = inner var frameInner where
  inner var ((var', val'):frame') = if var == var' then Just val' else inner var frame'
  inner var _ = Nothing
getFrameIndex (index, _) = index
getEnvCurrent (topFrame:_) = getFrameIndex topFrame
getEnvNext env = 1 + getEnvCurrent env
getFrameTo index (head:tail)
  | getFrameIndex head > index = getFrameTo index tail
  | getFrameIndex head == index = head:tail
  | otherwise = []
getFrameTo _ _ = []

type ID = Env
modifyEnv :: (Env -> Env) -> StateT ID (Either String) ()
modifyEnv = modify

makeID :: ID
makeID = [emptyFrame 0]
extendEnv :: ID -> ID
extendEnv env = emptyFrame (getEnvNext env):env
defineVar :: String -> Node -> Env -> Env
defineVar var val (frame:rest) = update var val frame : rest
defineVarM var val = StateT $ \env -> Right ((), defineVar var val env)
bulkDefineVars (var:vars) (val:vals) env = bulkDefineVars vars vals $ defineVar var val env
bulkDefineVars _ _ env = env
-- defineVarM :: String -> Node -> StateT ID (Either String) ()
-- defineVarM var val = StateT $ \(RealEnv (head:tail)) -> Right ((), RealEnv (update var val head:tail))

lookupEnv :: String -> Env -> Maybe Node
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
  Closure Int [String] [Node] |
  Primitive Int PrFn |
  Undefined |
  PairLast Node |
  Command Node deriving (Show)

newtype PrFn = PrFn ([Node] -> Either String Node)
instance Show PrFn where
  show _ = "PrFn"

baseEval :: Node -> StateT ID (Either String) Node
baseEval exp
 | isConstant exp = StateT $ \id -> Right (exp, id)
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

returnError name exp = StateT $ \_ -> Left $ "SyntaxError: Invalid " ++ name ++ ": " ++ show exp

letEval :: Node -> StateT ID (Either String) Node
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

defEval :: Node -> StateT ID (Either String) Node
defEval exp = case exp of
  Parens (_:var:body) -> case var of
    Label label -> do
      val <- baseEval $ Parens (Label "begin":body)
      defineVarM label val
      return val
    Parens ((Label fnName):args) -> do
      defEval $ Parens [Label "define", Label fnName, Parens (Label "lambda":Parens args:body)]
    _ -> returnError "define" exp
  _ -> returnError "define" exp

varEval :: Node -> StateT ID (Either String) Node
varEval exp = case exp of
  Label label -> StateT $ \env -> case lookupEnv label env of
    Just val -> Right (val, env)
    Nothing -> Left $ "ReferenceError: " ++ label ++ " is undefined."
  _ -> returnError "var" exp

lambdaEval :: Node -> StateT ID (Either String) Node
lambdaEval exp = case exp of
  Parens (_:Parens args:body) -> do
    names <- forM args $ \case
      Label name -> lift $ Right name
      _ -> lift $ Left $ "SyntaxError: invalid lambda" ++ show exp
    env <- get
    return $ Closure (getEnvCurrent env) names body
  _ -> returnError "lambda" exp

beginEval :: Node -> StateT ID (Either String) Node
beginEval exp = case exp of
  Parens (_:body) -> do
    evaled <- forM body baseEval
    return $ last evaled
  _ -> returnError "begin" exp

appEval :: Node -> StateT ID (Either String) Node
appEval exp = case exp of
  Parens (fn:args) -> do -- args may be []
    evaledFn <- baseEval fn
    evaledArgs <- forM args baseEval
    baseApply evaledFn evaledArgs

baseApply :: Node -> [Node] -> StateT ID (Either String) Node
baseApply fun argVals = case fun of
  Closure envIndex argVars nodes -> if length argVals /= length argVars then
    returnError "baseApply" (fun, argVals) else
    StateT $ \env -> let
      exp = Parens (Label "begin":nodes)
      preRunEnv = extendEnv $ getFrameTo envIndex env
      runEnv = bulkDefineVars argVars argVals preRunEnv
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

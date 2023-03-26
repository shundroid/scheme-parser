{-# LANGUAGE LambdaCase #-}

module Env where
import Runner
import Control.Monad.State.Strict

checkNumber2Fn nodes = case nodes of
  [Number i, Number j] -> Right (i, j)
  [_, Number _] -> Left "wrong type argument: position 1"
  [Number _, _] -> Left "wrong type argument: position 2"
  _ -> Left "wrong type argument: position 1 and 2"
defineNumber2Fn operator fn =
  defineVarM operator $ Primitive 2 $ PrFn $ \nodes -> case checkNumber2Fn nodes of
    Right (i, j) -> Right $ fn i j
    Left error -> Left error

makeTopEnv :: Env
makeTopEnv = result where
  Right result = evalStateT state makeEnv
  state :: StateT Env (Either String) Env
  state = do
    defineVarM "=" $ Primitive 2 $ PrFn $ \case
      [Number i, Number j] -> Right $ Boolean (i == j)
      [String i, String j] -> Right $ Boolean (i == j)
      [Boolean i, Boolean j] -> Right $ Boolean (i == j)
      _ -> Left "uncomparable two"
    defineNumber2Fn "+" $ \a b -> Number $ a + b
    defineNumber2Fn "*" $ \a b -> Number $ a * b
    defineNumber2Fn "-" $ \a b -> Number $ a - b
    defineNumber2Fn "/" $ \a b -> Number $ a `div` b
    defineNumber2Fn "<" $ \a b -> Boolean $ a < b
    defineNumber2Fn ">" $ \a b -> Boolean $ a > b
    defineNumber2Fn "<=" $ \a b -> Boolean $ a <= b
    defineNumber2Fn ">=" $ \a b -> Boolean $ a >= b
    defineVarM "list" $ Primitive (-1) $ PrFn $ \nodes ->
      Right $ Parens nodes
    defineVarM "cons" $ Primitive 2 $ PrFn $ \[a, b] -> case b of
      Parens nodes -> Right $ Parens (a:nodes)
      other -> Right $ Parens (a:[PairLast other])
    defineVarM "car" $ Primitive 1 $ PrFn $ \[pair] -> case pair of
      Parens (x:xs) -> Right x
      _ -> Left $ "wrong type argument in position 1 (expecting pair): " ++ show pair
    defineVarM "cdr" $ Primitive 1 $ PrFn $ \[pair] -> case pair of
      Parens [x, y] -> case y of
        PairLast node -> Right node
        _ -> Right $ Parens [y]
      Parens (x:xs) -> Right $ Parens xs
      _ -> Left $ "wrong type argument in position 1 (expecting pair): " ++ show pair
    defineVarM "load" $ Primitive 1 $ PrFn $ \[file] -> case file of
      String fileName -> Right $ Command $ Parens [Label "load", String fileName]
      _ -> Left $ "wrong type argument in position 1 (expecting string): " ++ show file
    get

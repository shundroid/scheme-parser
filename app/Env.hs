{-# LANGUAGE LambdaCase #-}

module Env where
import Runner
import Control.Monad.State.Strict

checkNumber1Fn nodes = case nodes of
  [Number i] -> Right i
  _ -> Left "wrong type argument: position 1"
defineNumber1Fn operator fn =
  defineVarM operator $ Primitive 1 $ PrFn $ \nodes -> case checkNumber1Fn nodes of
    Right i -> Right $ fn i
    Left error -> Left error
checkNumber2Fn nodes = case nodes of
  [Number i, Number j] -> Right (i, j)
  [_, Number _] -> Left "wrong type argument: position 1"
  [Number _, _] -> Left "wrong type argument: position 2"
  _ -> Left "wrong type argument: position 1 and 2"
defineNumber2Fn operator fn =
  defineVarM operator $ Primitive 2 $ PrFn $ \nodes -> case checkNumber2Fn nodes of
    Right (i, j) -> Right $ fn i j
    Left error -> Left error
defineNumberInfFn operator min fn =
  defineVarM operator $ Primitive (-1) $ PrFn $ \nodes -> do
    unless (length nodes >= min) $ Left "wrong number of arguments"
    nums <- forM nodes $ \case
      Number i -> return i
      _ -> Left "wrong type argument"
    return $ fn nums

makeTopEnv :: Env
makeTopEnv = result where
  Right result = evalStateT state makeID
  state :: StateT Env (Either String) Env
  state = do
    defineVarM "=" $ Primitive 2 $ PrFn $ \case
      [Number i, Number j] -> Right $ Boolean (i == j)
      [String i, String j] -> Right $ Boolean (i == j)
      [Boolean i, Boolean j] -> Right $ Boolean (i == j)
      _ -> Left "uncomparable two"
    defineNumberInfFn "+" 0 $ \nums -> Number $ sum nums
    defineNumberInfFn "*" 0 $ \nums -> Number $ product nums
    defineNumberInfFn "-" 1 $ \(head:tail) -> Number $ foldl (-) head tail
    defineNumberInfFn "/" 1 $ \(head:tail) -> Number $ foldl div head tail
    defineNumber2Fn "<" $ \a b -> Boolean $ a < b
    defineNumber2Fn ">" $ \a b -> Boolean $ a > b
    defineNumber2Fn "<=" $ \a b -> Boolean $ a <= b
    defineNumber2Fn ">=" $ \a b -> Boolean $ a >= b
    defineNumber2Fn "modulo" $ \a b -> Number $ a `mod` b
    defineNumber1Fn "even?" $ \a -> Boolean $ even a
    defineNumber1Fn "odd?" $ \a -> Boolean $ odd a
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

module Language.Interpreter (executeScript) where

import Language.Types
import Language.Runtime (ElementaState, setDimension, setMetric, definePoint)

executeScript :: Script -> ElementaState -> IO ElementaState
executeScript [] state = return state
executeScript (cmd:rest) state = case cmd of
  SetSpaceDim n -> do
    putStrLn $ "🌌 Setting space dimension: " ++ show n
    executeScript rest (setDimension n state)

  SetConstantMetric m -> do
    putStrLn "📐 Setting constant metric:"
    mapM_ print m
    executeScript rest (setMetric m state)

  DefinePoint name coords -> do
    putStrLn $ "📍 Defining point " ++ name ++ " at " ++ show coords
    executeScript rest (definePoint name coords state)

  Comment _ -> executeScript rest state
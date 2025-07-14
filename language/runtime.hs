module Language.Runtime
  ( ElementaState
  , emptyState
  , setDimension
  , setMetric
  , definePoint
  ) where

import qualified Data.Map as Map

data ElementaState = ElementaState
  { spaceDim  :: Int
  , metricMat :: [[Double]]
  , points    :: Map.Map String [Double]
  }

emptyState :: ElementaState
emptyState = ElementaState 3 [[1,0,0],[0,1,0],[0,0,1]] Map.empty

setDimension :: Int -> ElementaState -> ElementaState
setDimension n st = st { spaceDim = n }

setMetric :: [[Double]] -> ElementaState -> ElementaState
setMetric m st = st { metricMat = m }

definePoint :: String -> [Double] -> ElementaState -> ElementaState
definePoint name coords st = st { points = Map.insert name coords (points st) }
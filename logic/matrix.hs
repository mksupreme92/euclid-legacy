module Logic.Matrix (Matrix, matrixFromList, identityMatrix) where

type Matrix a = [[a]]

matrixFromList :: [[a]] -> Matrix a
matrixFromList = id

identityMatrix :: Num a => Int -> Matrix a
identityMatrix n =
  [ [ fromIntegral (fromEnum (i == j)) | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]
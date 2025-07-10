module Logic.Matrix (Matrix, matrixFromList, identityMatrix, matrixVectorProduct) where

type Matrix a = [[a]]

matrixFromList :: [[a]] -> Matrix a
matrixFromList = id

identityMatrix :: Num a => Int -> Matrix a
identityMatrix n =
  [ [ fromIntegral (fromEnum (i == j)) | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]

-- | Safe matrix-vector product: m · v
-- Returns Nothing if dimensions mismatch
matrixVectorProduct :: Num a => Matrix a -> [a] -> Maybe [a]
matrixVectorProduct m v =
  if all (\row -> length row == length v) m
     then Just $ map (sum . zipWith (*) v) m
     else Nothing
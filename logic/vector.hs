module Logic.Vector
  ( Vector
  , vectorFromList
  , vectorAdd
  , vectorSub
  , dotProduct
  , scalarMul
  , norm
  ) where

type Vector a = [a]

-- Semantic wrapper
vectorFromList :: [a] -> Vector a
vectorFromList = id

-- Safe elementwise sum of 1 or more vectors
vectorAdd :: Num a => [Vector a] -> Maybe (Vector a)
vectorAdd [] = Nothing
vectorAdd (v:vs)
  | all (\u -> length u == length v) vs = Just (foldl (zipWith (+)) v vs)
  | otherwise = Nothing


-- Element-wise subtraction
vectorSub :: Num a => Vector a -> Vector a -> Vector a
vectorSub = zipWith (-)

-- Safe dot product with length check
dotProduct :: Num a => Vector a -> Vector a -> Maybe a
dotProduct v1 v2 =
  if length v1 == length v2
    then Just (sum (zipWith (*) v1 v2))
    else Nothing

-- Scalar multiplication
scalarMul :: Num a => a -> Vector a -> Vector a
scalarMul k v = map (k *) v

-- Euclidean norm (magnitude)
norm v = fmap sqrt (dotProduct v v)
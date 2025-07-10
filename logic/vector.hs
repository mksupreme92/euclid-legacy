module Logic.Vector
  ( Vector
  , vectorFromList
  , vectorAdd
  , vectorSub
  , dotProduct
  , scalarMul
  , norm
  , normalize
  ) where

type Vector a = [a]

vectorFromList :: [a] -> Vector a
vectorFromList = id

vectorAdd :: Num a => [Vector a] -> Maybe (Vector a)
vectorAdd [] = Nothing
vectorAdd (v:vs)
  | all (\u -> length u == length v) vs = Just (foldl (zipWith (+)) v vs)
  | otherwise = Nothing

vectorSub :: Num a => Vector a -> Vector a -> Maybe (Vector a)
vectorSub v1 v2 =
  if length v1 == length v2
    then Just (zipWith (-) v1 v2)
    else Nothing

dotProduct :: Num a => Vector a -> Vector a -> Maybe a
dotProduct v1 v2 =
  if length v1 == length v2
    then Just (sum (zipWith (*) v1 v2))
    else Nothing

-- Safe 3D cross product
crossProduct :: Num a => Vector a -> Vector a -> Maybe (Vector a)
crossProduct [a1,a2,a3] [b1,b2,b3] =
  Just [ a2*b3 - a3*b2
       , a3*b1 - a1*b3
       , a1*b2 - a2*b1 ]
crossProduct _ _ = Nothing

scalarMul :: Num a => a -> Vector a -> Vector a
scalarMul k v = map (k *) v

norm :: Floating a => Vector a -> Maybe a
norm v = fmap sqrt (dotProduct v v)

normalize :: (Floating a, Eq a) => Vector a -> Maybe (Vector a)
normalize v = case norm v of
  Just 0 -> Nothing
  Just n -> Just (scalarMul (1 / n) v)
  Nothing -> Nothing
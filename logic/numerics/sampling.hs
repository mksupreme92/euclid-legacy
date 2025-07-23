module Logic.Numerics.Sampling
  ( sampleRectilinearGrid
  ) where

-- Generate list of index tuples for N dimensions
multiIndices :: [Int] -> [[Int]]
multiIndices [] = [[]]
multiIndices (n:ns) = [ i : rest | i <- [0 .. n-1], rest <- multiIndices ns ]

-- Generate an ND grid of points
sampleRectilinearGrid :: (Fractional a, Enum a) => [a] -> [a] -> [Int] -> [[a]]
sampleRectilinearGrid mins maxs steps
  | length mins /= length maxs || length mins /= length steps = error "Dimension mismatch in sampleRectilinearGrid"
  | otherwise =
      let stepSizes = zipWith3 (\a b n -> if n > 1 then (b - a) / fromIntegral (n - 1) else 0) mins maxs steps
          indices = multiIndices steps
      in map (\idxs -> zipWith3 (\a s i -> a + s * fromIntegral i) mins stepSizes idxs) indices

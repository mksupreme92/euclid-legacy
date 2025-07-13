module Initialize (loadInitialSpace) where

import System.Directory (listDirectory)
import System.FilePath (takeExtension)
import Data.List (isPrefixOf)
import Text.Read (readMaybe)
import Logic.Space
import Logic.Metric
import Numeric (showFFloat)  -- Add at the top of initialize.hs

-- Local pretty printer
prettyPrintMetric :: (RealFloat a, Show a) => MetricType a -> String
prettyPrintMetric (ConstantMetric mat) =
  unlines $ map formatRow mat
  where
    formatRow row = "  [" ++ unwords (map formatNum row) ++ "]"
    formatNum x
      | x == fromInteger (round x) = show (round x)
      | otherwise = showFFloat (Just 4) x ""
prettyPrintMetric (FieldMetric _) = "<field metric (function)>"

-- Public API
loadInitialSpace :: IO (Space Double)
loadInitialSpace = do
  files <- listDirectory "."
  let euclidFiles = filter (\f -> takeExtension f == ".euclid") files
  case euclidFiles of
    (f:_) -> do
      putStrLn $ "📄 Initializing from file: " ++ f
      contents <- readFile f
      case parseSpaceHeader contents of
        Just s  -> do
          putStrLn $ ("✅ Initialized space with dimension " ++ show (dim s) ++ " from file: " ++ f)
          putStrLn "📐 Metric:"
          putStrLn (prettyPrintMetric (metric s))
          return s
        Nothing -> do
          let fallback = defaultSpace
          putStrLn ("⚠️ Failed to parse header of file: " ++ f)
          putStrLn "🔧 Initializing with default settings:"
          putStrLn $ "✅ Dimension: " ++ show (dim fallback)
          putStrLn "📐 Metric:"
          putStrLn (prettyPrintMetric (metric fallback))
          return fallback

    [] -> do  -- ✅ aligned correctly now
      let fallback = defaultSpace
      putStrLn "⚠️ No .euclid file found."
      putStrLn "🔧 Initializing with default settings:"
      putStrLn $ "✅ Dimension: " ++ show (dim fallback)
      putStrLn "📐 Metric:"
      putStrLn (prettyPrintMetric (metric fallback))
      return fallback

-- Default fallback: 3D Euclidean
defaultSpace :: Floating a => Space a
defaultSpace = Space 3 (ConstantMetric [[1,0,0],[0,1,0],[0,0,1]])

-- Parse the file header for space + metric
parseSpaceHeader :: String -> Maybe (Space Double)
parseSpaceHeader input = do
  let ls = lines input
      dimLine    = findLinePrefix "space " ls
      metricLine = findLinePrefix "metric constant " ls

  dimStr <- stripPrefix "space " =<< dimLine
  dim <- readMaybe dimStr
  matStr <- stripPrefix "metric constant " =<< metricLine
  matrix <- readMaybe matStr :: Maybe [[Double]]
  return (Space dim (ConstantMetric matrix))

-- Helpers
findLinePrefix :: String -> [String] -> Maybe String
findLinePrefix prefix = safeHead . filter (prefix `isPrefixOf`)

stripPrefix :: String -> String -> Maybe String
stripPrefix pre str
  | pre `isPrefixOf` str = Just (drop (length pre) str)
  | otherwise            = Nothing

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
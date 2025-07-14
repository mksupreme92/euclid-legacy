module Language.Parser (parseElementa) where

import Language.Types
import Data.List (isPrefixOf)
import Text.Read (readMaybe)

-- | Parses a full script file into a list of commands
parseElementa :: String -> Either String Script
parseElementa input = fmap reverse $ foldl parseLine (Right []) (lines input)

parseLine :: Either String Script -> String -> Either String Script
parseLine acc line
  | "--" `isPrefixOf` line = fmap (Comment (drop 2 line) :) acc
  | "space " `isPrefixOf` line = case readMaybe (drop 6 line) of
      Just n  -> fmap (SetSpaceDim n :) acc
      Nothing -> Left $ "Invalid space declaration: " ++ line
  | "metric constant " `isPrefixOf` line =
      case readMaybe (drop 16 line) of
        Just mat -> fmap (SetConstantMetric mat :) acc
        Nothing  -> Left $ "Invalid metric matrix: " ++ line
  | "point " `isPrefixOf` line =
      let rest = drop 6 line
          (name, vecStr) = span (/= ' ') rest
      in case readMaybe (drop 1 vecStr) of
           Just coords -> fmap (DefinePoint name coords :) acc
           Nothing     -> Left $ "Invalid point coordinates: " ++ line
  | null (words line) = acc
  | otherwise = Left $ "Unknown command: " ++ line
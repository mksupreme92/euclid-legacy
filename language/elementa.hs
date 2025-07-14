module Language.Elementa (runElementaScript) where

import Language.Parser (parseElementa)
import Language.Interpreter (executeScript)
import Language.Runtime (emptyState)
import System.IO (readFile)

-- | Entry point: runs an .elementa script file
runElementaScript :: FilePath -> IO ()
runElementaScript path = do
  contents <- readFile path
  case parseElementa contents of
    Left err -> putStrLn $ "❌ Parse error: " ++ err
    Right script -> do
      putStrLn $ "🚀 Running script: " ++ path
      finalState <- executeScript script emptyState
      putStrLn "✅ Done."
      return ()
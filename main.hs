module Main (main, initSpace) where

import Logic.Space
import Initialize (loadInitialSpace)

main :: IO ()
main = do
  putStrLn "Euclid Geometry Kernel"
  putStrLn "📏🧮📐"
  putStrLn "Initializing Space"
  space <- initSpace
  putStrLn "✅ Initialization complete."

initSpace :: IO (Space Double)
initSpace = loadInitialSpace
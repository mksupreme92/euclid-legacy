module Language.Types where

-- | Simple placeholder AST node type
data Command
  = SetSpaceDim Int
  | SetConstantMetric [[Double]]
  | DefinePoint String [Double]
  | Comment String
  deriving (Show, Eq)

type Script = [Command]
module NaturalScheme.Tree where

import Data.Text (Text)
import qualified Data.Text as T

data Tree = BoundedAssumption Text Text
          | Assumption Text
          | Axiom Text Text
          | Deduction Text [Tree] Text
          | BindingDeduction Text Text [Tree] Text
          deriving(Eq, Show)

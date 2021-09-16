{-# LANGUAGE OverloadedStrings #-}
module Latex (
        treeToLatex
) where

import qualified Data.Text.Lazy.Builder as B

import NaturalScheme.Tree

withinBraces :: B.Builder -> B.Builder
withinBraces b = B.singleton '{' <> b <> B.singleton '}'

-- Convert a tree to a latex expression using the prftree package
treeToLatex :: Tree -> B.Builder
treeToLatex (BoundedAssumption binder f) = mconcat
    [ B.fromText "\\prfboundedassumption"
    , withinBraces (B.fromText f)
    ]

treeToLatex (Assumption f) = mconcat
    [ B.fromText "\\prfassumption"
    , withinBraces (B.fromText f)
    ]

treeToLatex (Axiom name f) = mconcat
    [ B.fromText "\\prfbyaxiom"
    , withinBraces (B.fromText name)
    , withinBraces (B.fromText f)
    ]

treeToLatex (Deduction ruleName hps conc) =
    let renderedHps  = map (withinBraces . treeToLatex) hps
        renderedConc = withinBraces (B.fromText conc)
    in mconcat $ [ B.fromText "\\prftree[r]"
                 , withinBraces (B.fromText ruleName)
                 ]
                 ++ renderedHps
                 ++ [ renderedConc ]

-- TODO: we can also keep track of the binder adding numbers
treeToLatex (BindingDeduction ruleName binder hps conc) =
    treeToLatex (Deduction ruleName hps conc)


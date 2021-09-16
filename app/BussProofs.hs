{-# LANGUAGE OverloadedStrings #-}
module BussProofs (
        treeToLatex
) where

import qualified Data.Text.Lazy.Builder as B

import NaturalScheme.Tree

within :: Char -> Char -> B.Builder -> B.Builder
within open close b = B.singleton open <> b <> B.singleton close

withinBraces :: B.Builder -> B.Builder
withinBraces b = within '{' '}' b

withinSquares :: B.Builder -> B.Builder
withinSquares = within '[' ']'

mathMode :: B.Builder -> B.Builder
mathMode = within '$' '$'

-- Convert a tree to a latex expression using the prftree package
treeToLatex :: Tree -> B.Builder
treeToLatex (BoundedAssumption binder f) = mconcat
    [ B.fromText "\\AxiomC"
    , withinBraces (mathMode (withinSquares (B.fromText f)))
    ]

treeToLatex (Assumption f) = mconcat
    [ B.fromText "\\AxiomC"
    , withinBraces (mathMode (B.fromText f))
    ]

treeToLatex (Axiom name f) = mconcat
    [ B.fromText "\\AxiomC{}\n"
    , B.fromText "\\RightLabel"
    , withinBraces (B.fromText name)
    , B.singleton '\n'
    , B.fromText "\\UnaryInfC"
    , withinBraces (mathMode (B.fromText f))
    ]

-- Deduction rules must have at most 5 hypotheses in bussproofs, sadly (or maybe not)
treeToLatex (Deduction ruleName hps conc) =
        let hpsLen = length hps
            renderedHps  = map (withinBraces . treeToLatex) hps
            renderedConc = withinBraces (B.fromText conc)
        in if hpsLen == 0
           then treeToLatex (Axiom ruleName conc)
           else mconcat $
                 renderedHps
                 ++ [ B.fromText "\\RightLabel"
                    , withinBraces (B.fromText ruleName)
                    , B.singleton '\n'
                    , inferOperName hpsLen
                    , withinBraces (mathMode (B.fromText conc))
                    ]

    where inferOperName 1 = "\\UnaryInfC"
          inferOperName 2 = "\\BinaryInfC"
          inferOperName 3 = "\\TrinaryInfC"
          inferOperName 4 = "\\QuaternaryInfC"
          inferOperName 5 = "\\QuinaryInfC"
          inferOperName _ = error "Unsupported number of hypotheses"

-- TODO: we can also keep track of the binder adding numbers
treeToLatex (BindingDeduction ruleName binder hps conc) =
    treeToLatex (Deduction ruleName hps conc)



module Main where

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as B

import NaturalScheme.Parser
import NaturalScheme.Postprocess
import NaturalScheme.Tree

import BussProofs

-- Read input, parse and validate it, finally convert it to a latex proof tree
main :: IO ()
main = getArgs >>= \args -> do
    treeDescr <- T.getContents
    case parseInput "<input>" treeDescr of
        Left err   -> putStrLn err
        Right tree ->
            case scopeCheck tree of
                Just n -> putStrLn $ "Unknown binder: " ++ show n
                Nothing -> do
                    let latex = B.toLazyText (treeToLatex tree)
                    TL.putStrLn latex
                    return ()

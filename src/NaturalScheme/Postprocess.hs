module NaturalScheme.Postprocess (
    scopeCheck
) where

import           NaturalScheme.Tree
import           Data.Text (Text)
import qualified Data.Set as S

-- Perform scope checking of bounded assumptions
scopeCheck' :: S.Set Text -> Tree -> Either Text ()
scopeCheck' inScope (BoundedAssumption binder _)
    | binder `S.member` inScope = Right ()
    | otherwise                 = Left binder

scopeCheck' _ Assumption{}  = Right ()
scopeCheck' _ Axiom{} = Right ()
scopeCheck' inScope (Deduction _ hps _) = mapM_ (scopeCheck' inScope) hps

scopeCheck' inScope (BindingDeduction _ binder hps _) = do
    let newScope = S.insert binder inScope
    mapM_ (scopeCheck' newScope) hps

-- Perform scope checking, if there is any out of scope binder,
-- returns Just name, otherwise Nothing.
scopeCheck :: Tree -> Maybe Text
scopeCheck tree =
    case scopeCheck' S.empty tree of
        Left err -> Just err
        Right _  -> Nothing

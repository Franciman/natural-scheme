{-# LANGUAGE OverloadedStrings #-}
module NaturalScheme.Parser (
    parseInput
) where
    
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Void
import           Data.Text (Text)
import qualified Data.Text as T

import NaturalScheme.Tree

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space
    space1
    (L.skipLineComment ";")
    -- We don't have block comments, just like comments
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

formula :: Parser Text
formula = lexeme (T.pack <$> (char '\"' *> manyTill printChar (char '\"')))

identifier :: Parser Text
identifier = lexeme (T.pack <$> some (alphaNumChar <|> char '-'))

parenExpr :: Parser Tree
parenExpr = do
    i <- identifier
    case i of
        "rule" -> ruleExpr
        "rule-bind" -> ruleBind
        "axiom" -> axiom
        binder -> BoundedAssumption binder <$> formula

axiom :: Parser Tree
axiom = Axiom <$> formula <*> formula

expr :: Parser Tree
expr = choice
    [ parens parenExpr
    , Assumption <$> formula
    ]


-- Helper to Make rule
makeRule :: Text -> [Tree] -> Parser Tree
makeRule name ts = do
    case last ts of
        Assumption conclusion ->
            let hypotheses = init ts
            in return $ Deduction name hypotheses conclusion
        _ -> fail "Expected conclusion as last argument"

-- Helper to Make Binding rule
makeBindingRule :: Text -> Text -> [Tree] -> Parser Tree
makeBindingRule name binder ts =
    case last ts of
        Assumption conclusion ->
            let hypotheses = init ts
            in return $ BindingDeduction name binder hypotheses conclusion
        _ -> fail "Expected conclusion as last argument"

ruleExpr :: Parser Tree
ruleExpr = do
    ruleName <- formula
    trees <- some expr
    makeRule ruleName trees

ruleBind :: Parser Tree
ruleBind = do
    ruleName <- formula
    binder <- parens identifier
    trees <- some expr
    makeBindingRule ruleName binder trees

parseInput :: String -> Text -> Either String Tree
parseInput inputName input = case runParser (spaceConsumer *> expr) inputName input of
    Left err -> Left $ errorBundlePretty err
    Right res -> Right res


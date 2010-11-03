module SloLog.Parser where

import Text.Parsec
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec.Language as Language
import Text.Parsec.Expr

import SloLog.Database

lexer = Token.makeTokenParser Language.haskellDef

identifier = Token.identifier lexer
parens = Token.parens lexer
brackets = Token.brackets lexer
reservedOp = Token.reservedOp lexer

variable = fmap Variable (char '?' >> identifier) <?> "Variable"
atom = fmap Atom identifier <?> "Atom"
compound = fmap Compound (parens $ many1 structure) <?> "Compound" 
structure = variable <|> atom <|> compound <?> "Structure"

operatorTable = [
    [Prefix (reservedOp "!" >> return Negation)],
    [Infix (reservedOp "&" >> return (\x y -> Conjunct [x, y])) AssocLeft],
    [Infix (reservedOp "|" >> return (\x y -> Disjunct [x, y])) AssocLeft]
    ]

simplequery = brackets query <|> fmap Query structure <?> "Simple query"
query = buildExpressionParser operatorTable simplequery <?> "Query"

clause = do
    body <- structure
    conclusion <- option Pass (reservedOp "<-" >> query)
    return (body, conclusion)
        <?> "Clause"

statement = 
    fmap Left (reservedOp "?" >> query) <|>
    fmap Right clause
        <?> "Statement"



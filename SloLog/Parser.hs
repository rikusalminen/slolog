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

variable = char '?' >> identifier >>= (return . Variable)
atom = identifier >>= (return . Atom)
compound = parens (many1 structure) >>= (return . Compound)
structure = variable <|> atom <|> compound

operator_table = [
    [Prefix (reservedOp "!" >> return Negation)],
    [Infix (reservedOp "&" >> return (\x y -> Conjunct [x, y])) AssocLeft],
    [Infix (reservedOp "|" >> return (\x y -> Disjunct [x, y])) AssocLeft]
    ]

simplequery = brackets query <|> (structure >>= (return . Query))
query = buildExpressionParser operator_table simplequery

clause = do
    body <- structure
    conclusion <- option Pass (reservedOp "<-" >> query)
    return (body, conclusion)

statement = 
    (reservedOp "?" >> query >>= (return . Left)) <|>
    (clause >>= (return . Right))



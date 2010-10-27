module SloLog.Database (
    Identifier,
    Structure (..),
    rewrite,
    Substitution,
    instantiate,
    Query (..),
    rewriteQ,
    instantiateQ,
    Clause,
    rewriteClause,
    instantiateClause,
    Database
    ) where

import qualified Data.Map as Map

type Identifier = String

data Structure = Atom String | Variable Identifier | Compound [Structure]
	deriving (Show)

rewrite _ x@(Atom _) = x
rewrite prefix (Variable ident) = Variable (prefix ++ ident)
rewrite prefix (Compound children) = Compound $ map (rewrite prefix) children
    
type Substitution = Map.Map Identifier Structure

instantiate sub x@(Atom _) = x
instantiate sub (Variable ident) = 
    case Map.lookup ident sub of
        Just value ->
            instantiate sub value
        Nothing ->
            error ("Cannot instantiate variable " ++ ident)
instantiate sub (Compound children) = Compound $ map (instantiate sub) children


data Query =
    Query Structure
    | Disjunct [Query]
    | Conjunct [Query]
    | Negation Query
    | Pass
        deriving Show

rewriteQ prefix (Query structure) = Query $ rewrite prefix structure
rewriteQ prefix (Disjunct children) = Disjunct $ map (rewriteQ prefix) children
rewriteQ prefix (Conjunct children) = Conjunct $ map (rewriteQ prefix) children
rewriteQ prefix (Negation child) = Negation $ rewriteQ prefix child
rewriteQ prefix Pass = Pass

instantiateQ _ Pass = Pass
instantiateQ sub (Query query) = Query $ instantiate sub query
instantiateQ sub (Disjunct children) = Disjunct $ map (instantiateQ sub) children
instantiateQ sub (Conjunct children) = Conjunct $ map (instantiateQ sub) children
instantiateQ sub (Negation child) = Negation $ instantiateQ sub child

type Clause = (Structure, Query)
type Database = [Clause]

rewriteClause prefix (body, conclusion) = 
    (rewrite prefix body, rewriteQ prefix conclusion)

instantiateClause sub (body, conclusion) = 
    (instantiate sub body, instantiateQ sub conclusion)


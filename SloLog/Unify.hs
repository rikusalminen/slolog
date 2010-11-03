module SloLog.Unify (
    unify
    ) where

import qualified Data.Map as Map

import SloLog.Database

occursCheck :: Identifier -> Structure -> Bool
occursCheck _ (Atom _) = False
occursCheck ident (Variable identr) = ident == identr
occursCheck ident (Compound children) = any (occursCheck ident) children     

extend :: Substitution -> Identifier -> Structure -> Maybe Substitution
extend sub ident value =
    case Map.lookup ident sub of
        Just struct ->
            unify sub struct value
        Nothing ->
            extendVar value
            where
            extendVar (Variable identr) =
                case Map.lookup identr sub of
                    Just struct ->
                        unify sub (Variable ident) struct
                    Nothing ->
                        if ident == identr then Just sub else Just extendedSub
            extendVar struct =
                if occursCheck ident struct then Nothing else Just extendedSub
            extendedSub =       
                Map.insert ident value sub

unify :: Substitution -> Structure -> Structure -> Maybe Substitution
unify sub (Atom x) (Atom y)
    | x == y = Just sub
    | otherwise = Nothing
unify sub (Variable ident) right = extend sub ident right
unify sub left (Variable ident) = extend sub ident left
unify sub (Compound []) (Compound []) = Just sub
unify sub (Compound (x:xs)) (Compound (y:ys)) =
    case unify sub x y of
        Just sub' ->
            unify sub' (Compound xs) (Compound ys)
        Nothing -> Nothing
unify _ _ _ = Nothing





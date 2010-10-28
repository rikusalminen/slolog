module SloLog.Pretty (
    pprint
    ) where

import SloLog.Database

import Text.PrettyPrint

pretty :: Structure -> Doc
pretty (Atom ident) = text ident
pretty (Variable ident) = text ("?" ++ ident)
pretty (Compound children) = parens (hsep $ map pretty children)

prettyQ :: Query -> Doc
prettyQ (Negation child) = text "!" <+> prettyQ child
prettyQ (Conjunct children) = hsep $ punctuate (text " &") (map (brackets . prettyQ) children) 
prettyQ (Disjunct children) = hsep $ punctuate (text " |") (map (brackets . prettyQ) children) 
prettyQ (Query struct) = pretty struct
prettyQ Pass = text "pass"

pprint = render . prettyQ

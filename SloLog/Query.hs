module SloLog.Query (
    qeval
    ) where

import Data.STRef
import Control.Monad
import Control.Monad.ST
import qualified Data.Maybe as Maybe

import SloLog.Database
import SloLog.Unify

qeval :: Database -> Query -> [Substitution] -> [Substitution]
qeval db query frames =
    runST $ do 
    counter <- newSTRef 0
    qeval' counter db query frames

qeval' _ _ Pass frames = return frames
qeval' counter db (Query struct) frames =
    fmap concat . mapM applied $ frames 
    where
    applied frame = fmap concat . mapM (apply counter db frame struct) $ db 
qeval' counter db (Disjunct disjuncts) frames = 
    fmap concat . mapM (\child -> qeval' counter db child frames) $ disjuncts
qeval' counter db (Conjunct conjuncts) frames =
    foldM (flip $ qeval' counter db) frames conjuncts  
qeval' counter db (Negation child) frames = do
    frames' <- mapM (\frame -> qeval' counter db child [frame]) frames
    return [frame | (frame, []) <- zip frames frames']
    
       
apply counter db frame struct clause = do
    num <- readSTRef counter
    modifySTRef counter (+1)
    apply' counter db frame struct (rewriteClause (show num ++ "#") clause)

apply' counter db frame struct (conclusion, body) = 
    case unify frame struct conclusion of
        Just frame' ->
            qeval' counter db body [frame']
        Nothing ->
            return []



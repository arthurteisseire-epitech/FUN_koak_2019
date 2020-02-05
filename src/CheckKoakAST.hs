module CheckKoakAST where

import           Data.Maybe
import           Debug.Trace
import           KoakAST

checkKoakAST :: KStmt -> Either String KStmt
checkKoakAST stmt -- TODO : check same identifier between funcCall and prototype
    | isValid = Right stmt
    | otherwise = Left "Identifier error"
  where
    isValid = checkIdentifierInFuncCall stmt

checkIdentifierInFuncCall :: KStmt -> Bool
checkIdentifierInFuncCall stmt =
    any (checkFuncCallMatchPrototype ((head . findFuncCallsFromStmt) stmt)) (findPrototypes stmt)

checkFuncCallMatchPrototype :: KPostfix -> KPrototype -> Bool
checkFuncCallMatchPrototype (KFuncCall (KIdentifier funcCallId) _) (KPrototype protoId _) = funcCallId == protoId
checkFuncCallMatchPrototype _ _ = False


findFuncCallsFromStmt :: KStmt -> [KPostfix]
findFuncCallsFromStmt (KStmt defs) = concatMap findFuncCalls (mapMaybe getExpressions defs)

getExpressions :: KDefs -> Maybe KExpressions
getExpressions (KExpressions exprs) = return exprs
getExpressions _ = Nothing

findFuncCalls :: KExpressions -> [KPostfix]
findFuncCalls (KListExpr exprs) = mapMaybe extractFuncCall exprs

extractFuncCall :: KExpression -> Maybe KPostfix
extractFuncCall (KExpression (KPostfix funcCall) []) = getFuncCall funcCall
extractFuncCall _                                    = Nothing

getFuncCall :: KPostfix -> Maybe KPostfix
getFuncCall (KFuncCall identifier args) = return $ KFuncCall identifier args
getFuncCall _                           = Nothing


findPrototypes :: KStmt -> [KPrototype]
findPrototypes (KStmt defs) = mapMaybe extractPrototype defs

extractPrototype :: KDefs -> Maybe KPrototype
extractPrototype (KDefs (KPrototype identifier args) _) = return $ KPrototype identifier args
extractPrototype _ = Nothing

module CheckKoakAST where

import           Data.Maybe
import           KoakAST

checkKoakAST :: KStmt -> Either String KStmt
checkKoakAST stmt -- TODO : check same identifier between funcCall and prototype
    | isValid = Right stmt
    | otherwise = Left "Identifier error"
  where
    isValid = checkFuncCallsMatchPrototypes stmt

checkFuncCallsMatchPrototypes :: KStmt -> Bool
checkFuncCallsMatchPrototypes stmt = all checkFuncCallMatchPrototypes (findFuncCalls stmt)
  where
    checkFuncCallMatchPrototypes funcCall = any (checkFuncCallMatchPrototype funcCall) (findPrototypes stmt)
    checkFuncCallMatchPrototype (KFuncCall (KIdentifier funcCallId) _) (KPrototype protoId _) = funcCallId == protoId
    checkFuncCallMatchPrototype _ _ = False

findFuncCalls :: KStmt -> [KPostfix]
findFuncCalls (KStmt defs) = concatMap findFuncCallsFromExpressions (mapMaybe getExpressions defs)
  where
    findFuncCallsFromExpressions (KListExpr exprs) = mapMaybe extractFuncCall exprs
    getExpressions (KExpressions exprs) = return exprs
    getExpressions _                    = Nothing
    extractFuncCall (KExpression (KPostfix (KFuncCall identifier args)) []) = return $ KFuncCall identifier args
    extractFuncCall _ = Nothing

findPrototypes :: KStmt -> [KPrototype]
findPrototypes (KStmt defs) = mapMaybe extractPrototype defs
  where
    extractPrototype (KDefs (KPrototype identifier args) _) = return $ KPrototype identifier args
    extractPrototype _ = Nothing

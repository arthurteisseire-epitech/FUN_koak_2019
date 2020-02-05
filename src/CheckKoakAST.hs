module CheckKoakAST where

import           Data.Maybe (fromJust, isJust)
import           KoakAST

checkKoakAST :: KStmt -> Either String KStmt
checkKoakAST stmt -- TODO : check same identifier between funcCall and prototype
    | isJust checkedStmt = Right stmt
    | otherwise = Left "Identifier error"
  where
    checkedStmt = checkIdentifierInFuncCall stmt

checkIdentifierInFuncCall :: KStmt -> Maybe KStmt
checkIdentifierInFuncCall stmt = do
    (KFuncCall (KIdentifier funcCallId) _) <- findFuncCall stmt
    (KPrototype prototypeId _) <- findPrototype stmt
    if funcCallId == prototypeId
        then return stmt
        else Nothing

findFuncCall :: KStmt -> Maybe KPostfix
findFuncCall (KStmt [KExpressions (KListExpr [KExpression (KPostfix (KFuncCall (KIdentifier identifier) (KCallExpr args))) []])]) =
    Just $ KFuncCall (KIdentifier identifier) (KCallExpr args)
findFuncCall _ = Nothing

findPrototype :: KStmt -> Maybe KPrototype
findPrototype (KStmt [KDefs (KPrototype identifier args) (KListExpr [])]) = Just $ KPrototype identifier args
findPrototype _ = Nothing

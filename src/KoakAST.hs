module KoakAST where

data KExpressions
    = KFor
          { kForCounter    :: KIdentifier
          , kForStartValue :: KExpression
          , kForMax        :: KExpression
          , kForIn         :: KExpressions
          }
    | KIf -- Mahé kif ce type
          { kIfCond :: KExpression
          , kIfThen :: KExpressions
          }
    | KIfElse
          { kIfCond :: KExpression
          , kIfThen :: KExpressions
          , kIfElse :: KExpressions
          }
    | KWhile -- Trop mignon lui
          { kWhileStopCond :: KExpression
          , kWhileDo       :: KExpressions
          }
    | KListExpr [KExpression]
    deriving (Show, Eq)

data KExpression = KExpression KUnary [KBinOpSuffix] deriving (Show, Eq)

data KBinOpSuffix = KBinOpUnary KBinOp KUnary | KBinOpExpr KBinOp KExpression deriving (Show, Eq)

data KUnary
    = KUnary KUnOp KUnary
    | KPostfix KPostfix
    deriving (Show, Eq)

data KPostfix
    = KPrimary KPrimary
    | KFuncCall KPrimary KCallExpr
    deriving (Show, Eq)

newtype KCallExpr = KArgs [KExpression] deriving (Show, Eq)

data KPrimary
    = KIdentifier KIdentifier
    | KLiteral KLiteral
    | KExpressions KExpressions
    deriving (Show, Eq)

data KLiteral
    = KDouble Double
    | KInt Int
    deriving (Show, Eq, Ord)

type KReturnType = String
type KIdentifier = String
type KBinOp = String
type KUnOp = String


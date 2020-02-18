module KoakAST where

newtype KStmt =
    KStmt [KDefs]
    deriving (Show, Eq)

data KDefs
    = KDefs KPrototype KExpressions
    | KExpressions KExpressions
    deriving (Show, Eq)

data KPrototype =
    KPrototype KIdentifier KPrototypeArgs
    deriving (Show, Eq) -- TODO : comprendre la grammaire pour compléter ce type (cf: sujet)

data KPrototypeArgs =
    KPrototypeArgs [KPrototypeArg] KType
    deriving (Show, Eq)

data KPrototypeArg =
    KPrototypeArg KIdentifier KType
    deriving (Show, Eq)

data KExpressions
    = KFor
          { kForCounter   :: KExpression
          , kForCondition :: KExpression
          , kForIncrement :: KExpression
          , kForIn        :: KExpressions
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

data KExpression =
    KExpression KUnary [(KBinOp, KUnary)]
    deriving (Show, Eq)

data KUnary
    = KUnOpUnary KUnOp KUnary
    | KPostfix KPostfix
    deriving (Show, Eq)

data KPostfix
    = KPrimary KPrimary
    | KFuncCall KPrimary KCallExpr
    deriving (Show, Eq)

newtype KCallExpr =
    KCallExpr [KExpression]
    deriving (Show, Eq)

data KPrimary
    = KIdentifier KIdentifier
    | KLiteral KLiteral
    | KPrimaryExpressions KExpressions
    deriving (Show, Eq)

data KLiteral
    = KDecimalConst Int
    | KDoubleConst Double
    deriving (Show, Eq, Ord)

data KType
    = KIntType
    | KDoubleType
    | KVoidType
    deriving (Show, Eq)

data KBinOp
    = KBinOpPlus
    | KBinOpLess
    | KBinOpMul
    | KBinOpDiv
    | KBinOpInf
    | KBinOpAssign
    deriving (Show, Eq)

data KUnOp
    = KUnOpNot
    | KUnOpLess
    deriving (Show, Eq)

type KIdentifier = String

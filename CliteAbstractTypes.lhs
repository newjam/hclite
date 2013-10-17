
> module CliteAbstractTypes where

> data Variable = Variable String
>   deriving (Show, Eq, Ord)

 data Value a = Value Type a

> data Value = IntValue {intVal::Int}
>            | BoolValue {boolVal::Bool}
>            | CharValue {charVal::Char}
>            | FloatValue {floatVal::Float}
>   deriving (Show, Eq)

> data ArithmeticOp = Gop GAriOp | Sop Type GAriOp
>   deriving (Show, Eq)

> data GAriOp = Add | Sub | Mul | Div
>   deriving (Show, Eq)

The book doesn't mention anything about type specific relational operators, so I will leave them overloaded. Maybe I will implement 'Ord Value'

> data RelationalOp = Gte | Lte | Gt | Lt | Eq | Neq
>   deriving (Show, Eq)
> data BooleanOp = And | Or
>   deriving (Show, Eq)

The book has us mixing up our type specific operators and our overloaded operators, which is something I dont like one bit. I suppose it makes more sense than creating a whole new "type specific abstract syntax tree" data type.

> data UnaryOp = Not | Neg | 
>   ToFloat | ToInt | ToChar |
>   C2i | I2f | -- type specific implicit cats
>   F2i | I2c | -- type spcific explicit cats
>   FNeg | INeg -- type specific
>   deriving (Show, Eq)

> data BinaryOp = AriBin ArithmeticOp
>               | RelBin RelationalOp
>               | BooBin BooleanOp
>   deriving (Show, Eq)    

> data Expr = Unary UnaryOp Expr
>           | Binary BinaryOp Expr Expr
>           | ValExpr Value
>           | VarExpr Variable
>  deriving (Show, Eq)

> data Type = IntT | BoolT | FloatT | CharT
>   deriving (Eq, Show)

> type Statements = [Statement]
> data Statement = Assignment Variable Expr
>                | Skip
>                | Block Statements
>                | Conditional Expr Statement Statement
>                | Loop Expr Statement
>   deriving (Eq, Show)

> type Declarations = [Declaration]

> data Declaration = VarDec Type Variable
>   deriving (Show, Eq)

> data Program = Program Declarations Statements
>   deriving (Show, Eq)

==== Abstract Syntax Tree Display ====

> class VisualizeTree a where
>   toStr :: a -> [String]

> pts :: VisualizeTree a => a -> [String]
> pts = map ("     " ++) . toStr

> instance VisualizeTree Expr where
>  toStr (Unary op e) = [show op] ++ pts e
>  toStr (Binary op e1 e2) = [show op] ++ 
>    pts e1 ++
>    pts e2 
>  toStr (ValExpr v) = [show v]
>  toStr (VarExpr v) = [show v]

> instance VisualizeTree Statement where
>   toStr (Assignment t s) = ["AssignTo " ++ show t] ++
>     pts s
>   toStr Skip = ["Skip"]
>   toStr (Block ss) = ["{"] ++ concatMap toStr ss ++ ["}"]
>   toStr (Loop e s) = ["While"] ++ 
>     pts e ++ ["Do"] ++ pts s
>   toStr (Conditional e s1 s2) = ["If"] ++
>     pts e ++ ["Then"] ++ pts s1 ++ ["Else"] ++ pts s2

> instance VisualizeTree Program where
>   toStr (Program ds ss) = concatMap toStr ds ++ concatMap toStr ss

> instance VisualizeTree Declaration where
>   toStr (VarDec t var) = [show t ++ show var]


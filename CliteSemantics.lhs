
TODO!

> module CliteSemantics where 

> import Control.Monad
> import Control.Monad.State
> import CliteAbstractTypes
> import qualified Map
> import Control.Applicative
> import Data.Char

 tick :: State Int Int
 tick = do 
   n <- get
   put (n+1)
   return n

> type StateMap = Map.Map Variable Value

> testSM = Map.mkMap [(Variable "x", IntValue 2)]
> testTM = Map.mkMap [(Variable "x", IntT)]

> evalE :: Expr -> State StateMap Value
> evalE (VarExpr var) = do
>     m <- get
>     case Map.get var m of
>       Nothing -> error $ "variable not in state map: " ++ show var ++ "\n" ++ show m
>       Just x -> return x

> evalE (ValExpr x) = return x

> evalE (Binary op e1 e2) = do
>   v1 <- evalE e1
>   v2 <- evalE e2
>   return $ case op of 
>     AriBin (Sop FloatT x) -> FloatValue $
>       (floatOp x) (floatVal v1) (floatVal v2)
>     AriBin (Sop IntT x) -> IntValue $
>       (intOp x) (intVal v1) (intVal v2)
>     RelBin x -> BoolValue $ 
>       (relOp x) v1 v2
>     BooBin x -> BoolValue $
>       (boolOp x) (boolVal v1) (boolVal v2) 

> evalE (Unary op e) = do
>   v <- evalE e
>   return $ case op of
>     Not -> BoolValue $ not (boolVal v)
>     C2i -> IntValue $ ord (charVal v)
>     I2f -> FloatValue $ fromIntegral (intVal v)
>     F2i -> IntValue $ truncate (floatVal v)
>     I2c -> CharValue $ chr (intVal v)
>     FNeg -> FloatValue $ (- (floatVal v))
>     INeg -> IntValue $ (- (intVal v))

> floatOp Add = (+)
> floatOp Sub = (-)
> floatOp Mul = (*)
> floatOp Div = (/)
> intOp Add = (+)
> intOp Sub = (-)
> intOp Mul = (*)
> intOp Div = div
> relOp op = case op of
>    Gte -> (>=)
>    Lte -> (<=)
>    Gt  -> (>)
>    Lt  -> (<)
>    Eq  -> (==)
>    Neq -> (/=)
> instance Ord Value where
>   compare (IntValue x) (IntValue y) = compare x y 
>   compare (FloatValue x) (FloatValue y) = compare x y
>   compare (CharValue x) (CharValue y) = compare x y
>   compare (BoolValue x) (BoolValue y) = compare x y
> boolOp And = (&&)
> boolOp Or  = (||)

> evalS :: Statement -> State StateMap ()
> evalS (Assignment t s) = do
>   v <- evalE s
>   m <- get
>   put (Map.set t v m)

> evalS Skip = return ()

> evalS (Conditional e s1 s2) = do
>   p <- evalE e
>   evalS $ if (boolVal p) then s1 else s2

> evalS (Loop e s) = do
>   p <- evalE e
>   sequence_ $ if (boolVal p) 
>     then [evalS s, evalS (Loop e s)]
>     else [return ()]

> evalS (Block ss) = sequence_ (map evalS ss)

 evalS (Block []) = return ()
 evalS (Block (s:ss)) = do 
   evalS s
   evalS (Block ss)


> evalP (Program ds ss) = runState (evalS (Block ss)) $ initVars ds

> initVars ds = Map.mkMap . map (\(VarDec t var)->(var,initVal t)) $ ds
 
> initVal BoolT = BoolValue True
> initVal IntT = IntValue 0
> initVal FloatT = FloatValue 0.0
> initVal CharT = CharValue ' '


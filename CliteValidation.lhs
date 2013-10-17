
> module CliteValidation (validProgram, transProgram, typeOf, transform, valid) where

> import CliteAbstractTypes
> import Map

> type TypeMap = Map Variable Type

Type rules 6.6.* : typeOf function for expressions.

> class Typeable a where
>   typeOf :: TypeMap -> a -> Type 

> instance Typeable Value where
>   typeOf _ (IntValue _) = IntT
>   typeOf _ (BoolValue _) = BoolT
>   typeOf _ (CharValue _) = CharT
>   typeOf _ (FloatValue _) = FloatT

> instance Typeable Expr where
>   typeOf tm (Unary Neg p) = typeOf tm p
>   typeOf _  (Unary Not _) = BoolT
>   typeOf _  (Unary ToFloat _) = FloatT
>   typeOf _  (Unary ToInt _) = IntT
>   typeOf _  (Binary (RelBin _) _ _) = BoolT
>   typeOf _  (Binary (BooBin _) _ _) = BoolT
>   typeOf tm (Binary (AriBin _) t1 _) = typeOf tm t1
>   typeOf tm (ValExpr val) = typeOf tm val
>   typeOf tm (VarExpr var) = typeOf tm var

> instance Typeable Variable where
>   typeOf tm var = case get var tm of
>     Nothing -> error "variable not in typemap"
>     Just t  -> t

> class Validatable a where
>   valid :: TypeMap -> a -> Bool 

> instance Validatable Variable where
>   valid tm var = case get var tm of
>     Nothing -> False
>     Just _  -> True

> instance Validatable Value where
>   valid _ _ = True

Type rules 6.5.*

> instance Validatable Expr where
>   valid tm (Unary op t) = valid tm t && case op of
>     Not -> tt `elem` [BoolT]
>     Neg -> tt `elem` [IntT,FloatT]
>     ToFloat -> tt `elem` [IntT]
>     ToInt -> tt `elem` [CharT,FloatT]
>     ToChar -> tt `elem` [IntT]
>     where tt = typeOf tm t
>   valid tm (ValExpr t) = valid tm t
>   valid tm (VarExpr t) = valid tm t
>   valid tm (Binary op t1 t2) = valid tm t1 
>     && valid tm t2 
>     && case op of
>       AriBin _ -> typeOf tm t1 == typeOf tm t2 
>         && typeOf tm t1 `elem` [FloatT, IntT]
>       RelBin _ -> typeOf tm t1 == typeOf tm t2
>       BooBin _ ->  typeOf tm t1 == BoolT
>         && typeOf tm t2 == BoolT

Type rules 6.4.*

> instance Validatable Statement where
>   valid _  Skip = True
>   valid tm (Conditional e s1 s2) = valid tm e
>     && typeOf tm e == BoolT
>     && valid tm s1 
>     && valid tm s2
>   valid tm (Loop e s) = valid tm e
>     && typeOf tm e == BoolT
>     && valid tm s
>   valid tm (Block ss) = all (valid tm) ss
>   valid tm (Assignment t s) = valid tm t
>     && valid tm s
>     && case typeOf tm t of
>          FloatT -> typeOf tm s `elem` [FloatT, IntT]
>          IntT   -> typeOf tm s `elem` [IntT, CharT]
>          tt     -> typeOf tm s == tt

Type rules 6.3.*

> instance Validatable Program where
>   valid _ (Program ds ss) = all (valid (typing ds)) ss

> typing :: Declarations -> TypeMap
> typing = mkMap . map (\(VarDec t v)->(v,t))  

> validProgram :: Program -> Bool
> validProgram p = valid (mkMap []) p

Transformations: implicit type conversions and type specific operations.

> class Transformable a where
>   transform :: TypeMap -> a -> a

> instance Transformable Statement where
>   transform tm (Assignment t s) = case typeOf tm t of
>     FloatT -> case typeOf tm s of
>       IntT -> Assignment t (Unary I2f $ ts)
>       _    -> Assignment t ts
>     IntT   -> case typeOf tm s of
>       CharT -> Assignment t (Unary C2i $ ts)
>       _     -> Assignment t ts
>     _ -> Assignment t ts
>     where ts = transform tm s
>   transform _  Skip = Skip
>   transform tm (Block ss) = Block $ map (transform tm) ss
>   transform tm (Conditional e s1 s2) = Conditional
>     (transform tm e) (transform tm s1) (transform tm s2)
>   transform tm (Loop e s) = Loop 
>     (transform tm e) (transform tm s)

> instance Transformable Expr where 
>   transform _ o@(ValExpr _) = o
>   transform _ o@(VarExpr _) = o
>   transform tm (Binary op e1 e2) = case typeOf tm e1 of
>     IntT   -> Binary (intSpec op)   te1 te2
>     FloatT -> Binary (floatSpec op) te1 te2
>     _ -> Binary op te1 te2
>     where te1 = transform tm e1
>           te2 = transform tm e2
>   transform tm (Unary op e) = case op of
>     ToFloat -> case tt of
>       IntT -> Unary I2f te
>       _ -> error "static type checker is a failure"
>     ToInt   -> case tt of 
>       CharT -> Unary C2i te
>       FloatT -> Unary F2i te
>       _ -> error "static type checker is a failure"
>     ToChar -> case tt of
>       IntT -> Unary I2c te
>       _ -> error "static type checker is a failure"
>     Neg     -> case tt of
>       IntT -> Unary INeg te
>       FloatT -> Unary FNeg te
>       _ -> error "static type checker is a failure"
>     Not -> Unary op te -- op same it is not overload
>     where te = transform tm e
>           tt = typeOf tm e

> intSpec (AriBin (Gop x)) = AriBin (Sop IntT x)
> intSpec op = op

> floatSpec (AriBin (Gop x)) = AriBin (Sop FloatT x)
> floatSpec op = op

> instance Transformable Program where
>   transform _ (Program ds ss) = Program 
>     ds (map (transform (typing ds)) ss)

> transProgram :: Program -> Program
> transProgram p = transform (mkMap []) p




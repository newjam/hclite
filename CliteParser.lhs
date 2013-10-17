
We only care about exporting the Read instance, so in final product maybe have it export nothing, but for debugging it is useful to have all the subtypes implemented.

 module CliteParser ( ) where

> module CliteParser where

> import AppParse
> import CliteAbstractTypes
> import Control.Applicative hiding ((<|>),many)
> import Data.List

This uses AppParse, an module of parser combinators for an applicative parser.

Each nonTerminal in the grammar gets its own parser function. that function returns an abstract syntax object.

> ident = token ( Variable <$> ((\l cs-> l:cs) <$> letter <*>  many (letter<|>digit)) )

> literal = floatLit <|> intLit <|> boolLit <|> charLit

> floatLit = token (FloatValue <$> (read <$> float))
> intLit = token (IntValue <$> (read <$> integer))
> boolLit = token (BoolValue <$> (tokenize "true" True <|> tokenize "false" False))
> charLit = token ((\_ c _ -> CharValue c) <$> symbol "\'" <*> eat <*> symbol "\'")

> integer = many1 digit

> float = (\x y z->x++y++z)<$>integer<*>string "."<*>integer

> addOp = AriBin <$> (tokenize "+" (Gop Add) 
>   <|> tokenize "-" (Gop Sub))

> mulOp = AriBin <$> (tokenize "*" (Gop Mul) 
>   <|> tokenize "/" (Gop Div))

> factor = primary <|> (Unary <$> unaryOp <*> primary)

> unaryOp = tokenize "-" Neg <|> tokenize "!" Not

> primary = paren expression
>   <|> (Unary   <$> (mkCast <$> typep) <*> paren expression)
>   <|> (ValExpr <$> literal)
>   <|> (VarExpr <$> ident) 

> paren p = surround "(" p ")"

> surround l p r = (\_ x _ -> x) <$> symbol l <*> p <*> symbol r

> mkCast FloatT = ToFloat
> mkCast IntT   = ToInt
> mkCast _      = error "invalid type conversion"

> term = factor `chainl1` (Binary <$> mulOp)

> addition = term `chainl1` (Binary <$> addOp)

> relOp = RelBin <$> ( tokenize ">=" Gte
>      <|> tokenize "<=" Lte
>      <|> tokenize ">"  Gt
>      <|> tokenize "<"  Lt )

> relation = (prefixify 
>  <$> addition <*> (Binary <$> relOp) <*> addition)
>  <|> addition 

> eqOp = RelBin <$> (tokenize "==" Eq <|> tokenize "!=" Neq) 

> equality = (prefixify 
>  <$> relation <*> (Binary <$> eqOp) <*> relation)
>  <|> relation

> conjunction = equality `chainl1` (Binary <$> andOp)

> expression = conjunction `chainl1` (Binary <$> orOp)

> andOp = BooBin <$> tokenize "&&" And
> orOp  = BooBin <$> tokenize "||" Or 

> typep =  tokenize "int" IntT
>      <|> tokenize "bool" BoolT
>      <|> tokenize "float" FloatT
>      <|> tokenize "char" CharT

> statement = skip <|> block <|> assignment <|> ifstatement <|> while

> assignment = (\v _ e _-> Assignment v e)
>   <$> ident 
>   <*> symbol "=" 
>   <*> expression
>   <*> symbol ";"

> skip = tokenize ";" Skip

> while = (\_ e s-> Loop e s)
>   <$> symbol "while" 
>   <*> paren expression
>   <*> statement

> ifstatement = ifelse <|> (ifonly <*> pure Skip)

> ifelse = ifonly <*> ((\_ s->s) <$> symbol "else" <*> statement)

> ifonly = (\_ e s1 -> Conditional e s1)
>   <$> symbol "if"
>   <*> paren expression
>   <*> statement

> block = Block <$> surround "{" statements "}"

> statements = many statement 

> declaration = (\t vs _->map (VarDec t) vs) 
>   <$> typep
>   <*> ident `sepBy` symbol ","
>   <*> symbol ";"

> declarations = concat <$> many declaration

> program = (\_ _ _ _ _ ds ss _->Program ds ss)
>   <$> symbol "int"
>   <*> symbol "main"
>   <*> symbol "("
>   <*> symbol ")"
>   <*> symbol "{"
>   <*> declarations
>   <*> statements
>   <*> symbol "}"


> extract [("",p)] = p
> extract _        = error "vague parse error"

> swap (x,y) = (y,x)

> instance Read Program where
>   readsPrec i cs = map swap (parse program (deComment cs))
>     where deComment = concat . map (concat . takeWhile (not.isPrefixOf "//") . words) . lines

 infixify op x y =  Binary <$> x <*> op <*> y

 test = \x op y -> Binary op x y

 data E = B O E E
        | L Int
   deriving Show
 data O = A | M
  deriving Show

 t = l `chainl1` (B<$>o)

 l = token (L <$> integer)
 o = tokenize "+" A <|> tokenize "*" M

comments aren't a part of the grammar, so they have to be filtered out before parsing...




> module Main where

> import CliteAbstractTypes
> import CliteParser
> import CliteValidation
> import CliteSemantics
> import Map
> import Samples
> import AppParse

> import Control.Monad.State

> main = report t08

> report s = do
>   putStrLn "==== Original ===="
>   putStrLn s
>   p <- return (read s :: Program)
>   v <- return (validProgram p)
>   putStrLn "==== Validity ===="
>   putStrLn (show v)
>   tp <- return (transProgram p)
>   putStrLn "==== Transformed ===="
>   putStrLn . foldr1 (\x y->x++"\n"++y) . toStr $ tp
>   putStrLn "==== Results ====="
>   let ((), sm) = evalP tp
>   putStrLn . foldr (\x y->show x++"\n"++y) "" . toList $ sm 

*Main> report t10
==== Original ====
int main() {
  float a, x, result;
  a = 9.1;
  x = 1.0;
  while (x*x > a+0.0001 || x*x < a-0.0001 )
    x = (x + a/x)/2.0;
   result = x;
 }
==== Validity ====
True
==== Transformed ====
FloatTVariable "a"
FloatTVariable "x"
FloatTVariable "result"
AssignTo Variable "a"
     FloatValue {floatVal = 9.1}
AssignTo Variable "x"
     FloatValue {floatVal = 1.0}
While
     BooBin Or
          RelBin Gt
               AriBin (Sop FloatT Mul)
                    Variable "x"
                    Variable "x"
               AriBin (Sop FloatT Add)
                    Variable "a"
                    FloatValue {floatVal = 1.0e-4}
          RelBin Lt
               AriBin (Sop FloatT Mul)
                    Variable "x"
                    Variable "x"
               AriBin (Sop FloatT Sub)
                    Variable "a"
                    FloatValue {floatVal = 1.0e-4}
Do
     AssignTo Variable "x"
          AriBin (Sop FloatT Div)
               AriBin (Sop FloatT Add)
                    Variable "x"
                    AriBin (Sop FloatT Div)
                         Variable "a"
                         Variable "x"
               FloatValue {floatVal = 2.0}
AssignTo Variable "result"
     Variable "x"
==== Results =====
(Variable "a",FloatValue {floatVal = 9.1})
(Variable "result",FloatValue {floatVal = 3.0166206})
(Variable "x",FloatValue {floatVal = 3.0166206})


\documentclass{article}
\usepackage{verbatim}
\long\def\ignore#1{}
\usepackage{listings}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
      basicstyle=\small\ttfamily,
      flexiblecolumns=false,
      basewidth={0.5em,0.45em},
      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
               {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
               {\\\\}{{\char`\\\char`\\}}1
               {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
               {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2 
               {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
               {>>}{{>>}}2 {>>=}{{>>=}}2
               {|}{{$\mid$}}1               
    }

\begin{document}

\title{Applicative Parsing}
\author{James Newman}
\maketitle

\ignore{
\begin{code}
module AppParse (Parser, eat, sat, char, string, many, many1, choose, space, token, symbol, tokenize, (<|>), letter, digit, parse, chainl1, prefixify, sepBy) where

import Control.Applicative hiding ((<|>), many)
import Char
import Data.List hiding (concat)
import Data.Traversable
import Data.Foldable
import Data.Monoid
import Prelude hiding (concatMap, concat)
\end{code}
}

The standard method for parsing in Haskell uses the \texttt{Monad} typeclass, as explained by Hutton in ``Monadic Parsing in Haskell.''
However, many parsers can actually be implemented using only the \texttt{Applicative} typeclass.
This is more desirable than using monads because we limit complexity by not bothering with annoying or confusing \texttt{Monad} things.
We do lose some power over monadic parsing, namely the ability to make decisions that change the flow of the computation based on the results of parsing. I in no way independently discoverved applicative parsing, only heard it was possible and decided to try and implement it.

These notes document the creation of an applicative parser following the outline of Hutton's paper.

\section{Parser definition and Parser Combinators}

Here is the \texttt{Parser} newtype.

\begin{code}
newtype Parser a = Parser (String -> [(String,a)])
parse (Parser f) cs = f cs
\end{code}

With this definition, we can create two primitive parser functions.
\texttt{eat} consumes a single character.
\texttt{sat} consumes a single character if it matches a predicate.

\begin{code}
eat :: Parser Char
eat = Parser (\cs -> case cs of
  ""     -> []
  (c:cs) -> [(cs,c)] )
sat :: (Char->Bool) -> Parser Char
sat p = Parser (\cs -> case cs of
  ""     -> []
  (c:cs) -> if p c then [(cs,c)] else [] )
\end{code}

Now we can use these functions:

\begin{code}
char :: Char -> Parser Char
char c = sat (c ==)
digit = sat isDigit
letter = sat isAlpha
\end{code}

Now we would like to be able to combine and sequence parsers to construct more complex parser combinators.
This is done by implementing \texttt{Functor} and \texttt{Applicative} for Parsers.
\begin{code}
instance Functor Parser where
  fmap f (Parser g) = Parser (\cs -> map (fmap f) (g cs))
instance Applicative Parser where
  pure x = Parser (\cs -> [(cs,x)])
  (Parser f) <*> (Parser g) = Parser (\cs-> 
    [(cs'', f' x) | (cs', f')<-f cs, (cs'',x)<-g cs' ])
\end{code}

Since we've defined \texttt{Applicative} for \texttt{Parser}, we can now traverse over structures that contain parsers.
For example, \texttt{string} creates a parser for each char in a string, then sequences those \texttt{Parser Char}'s into a \texttt{Parser String}.

\begin{code}
string :: String -> Parser String
string = traverse char
\end{code}

Another useful thing we would like to do with \texttt{Parser}'s is combine results of two parsers into a single parser with both results. 
This is for grammar productions such as $A\to B|C$.i
\texttt{<|>} is an alias function to closer resemble CFG notation.

\begin{code}
instance Monoid (Parser a) where
  mempty = Parser (\cs->[])
  Parser p `mappend` Parser q = Parser (\cs-> p cs ++ q cs )
p <|> q = choose $ p `mappend` q
\end{code}

\texttt{choose} will create a new parser with only one possible parse from a parser that may have multiple parse.

\begin{code}
choose :: Parser a -> Parser a
choose (Parser p) = Parser (\cs-> case p cs of
  []     -> []
  (p:ps) -> [p] )
\end{code}

With these definitions we can create even more useful parser combinators.
First, \texttt{many} and \texttt{many1}, which take a parser of a type and return a parser for a list of that type.

\begin{code}
many :: Parser a -> Parser [a]
many  p = manyOp p (inits)
many1 p = manyOp p (tail.inits)

manyOp p f = choose $ Parser (\cs-> 
  reverse . concat . takeWhile (not . null) . map (\p->parse p cs) . map sequenceA . f . repeat $ p )
\end{code}
For instance, \texttt{space}, which parses whitespace, is defined using many.
\begin{code}
space :: Parser String
space = many (sat isSpace)
\end{code}

\texttt{token} eats up space after a parser, without effecting the value of the parser. 
\texttt{symbol} creates a parser for a string with following whitespace.
\texttt{tokenize} parses a provided string, and if succesfully parsed, will return a provided object.

\begin{code}
token p      = (\x _ -> x) <$> p <*> space
symbol       = token . string
tokenize s t = const t <$> symbol s
\end{code}

%\texttt{chainl1} takes a \texttt{Parser a} p and a function Parse

\begin{code}
prefixify x op y = op x y
p `chainl1` op = ( prefixify 
  <$> p <*> op <*> (p `chainl1` op)) <|> p
\end{code}

\begin{code}
sepBy p s = (\a as->a:as) 
  <$> p
  <*> many ((\_->id)<$>s<*>p)
\end{code}

\section{Applicative Expression Parser}

We now have enough parser combinators to concisely implement a parser for a simple expression language.

The language and evaluator:
\begin{code}
data Expr = Lit Int
          | Bin Bop Expr Expr
  deriving (Show, Eq)
data Bop  = Add | Mul
  deriving (Show, Eq)

eval :: Expr -> Int
eval (Lit x) = x
eval (Bin bop e1 e2) = case bop of
  Add -> eval e1 + eval e2
  Mul -> eval e1 * eval e2

evalParse s = let [("",e)] = parse expr s in eval e
\end{code}

The parser very looks very similar to the language definition.

\begin{code}
expr =  bin <|> lit

bin = Bin <$> bop <*> expr <*> expr

bop = add <|> mul
add = tokenize "+" Add
mul = tokenize "*" Mul

lit = Lit <$> int
int :: Parser Int
int = read <$> (token . many1 . sat $ isDigit)
\end{code}

\end{document}

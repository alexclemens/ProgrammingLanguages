> {-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

> module Main where

> import Control.Applicative
> import Data.Char
>
> import qualified Data.Map as Map
> import Data.Map (Map)

> --import Text.Megaparsec
> --import Data.Text (Text)
> --import qualified Data.Text as T
> import Control.Monad (void)
> import Data.Void
> --import qualified Text.Megaparsec.Char.Lexer as L


> type VarName = String
>
> data LExp = 
>     Var VarName 
>    | App LExp LExp 
>    | Lambda VarName LExp


% > instance Show LExp where
% >   show (Var v) = v
% >   show (Lambda v e) = "lambda (" ++ show v ++ "." ++ show e ++ ")"
% >   show (App e1 e2) = "" ++ show e1 ++ " " ++ show e2 ++ ""


> data Token =
>     TNum Int
>   | TVar String
>   | TLParen
>   | TRParen
>   | TEq
>   | TIf
>   | TLambda
>   | TLet
>   | TIn
>   | TDot
>   deriving (Show, Eq)


> lexer :: String -> [Token]
> lexer [] = []
> lexer (w:s) | isSpace w = lexer (dropWhile isSpace s)
> lexer ('=':s) = TEq:lexer s
> lexer ('.':s) = TDot:lexer s
> lexer ('(':s) = TLParen:lexer s
> lexer (')':s) = TRParen:lexer s


> lexer s | isAlpha (head s) =
>   let (id1,s') = span isAlphaNum s in
>	   let keywords = ["lambda", "let", "in"] in
>	     if elem id1 keywords
>	       then case id1 of
>	         "lambda" -> TLambda: lexer s'
>	         "let" -> TLet: lexer s'
>	         "in" -> TIn: lexer s'
>	       else TVar id1:lexer s'
> lexer (n:_) = error $ "Lexer error: unexpected character " ++ [n]




IDEAS

Following the form of a labda tree that we used to track free variables
	The idea here is that we have a tree like structure, but we wouldn't know how to handle "let in" cases
	This might only work on very simple lambda functions maybe as a part of the AST, but certainly is not a complete soln
Multi lambda expressions
	check to see if the dot comes next, if not we have another lambda
		we have to find a way to convert this to something more uniform in the parser (ie lambda x y. x = lambda x. lambda y . x)
If we see a left paren, we need a matching right paren
	if we have a let, we need a corresponding in

Ask about potential inputs for testing



PARSER STARTS HERE



>-- type Parser = Parsec Void String
> newtype Parser a = Parser { parse :: String -> Maybe (a,String) }


>-- identifier :: Parser Text
>-- identifier = T.pack <$> some alphaNumChar


> variable :: Parser LExp
> variable = Var <$> identifier


> application :: Parser LExp
> application = between (space *> char '(') (space *> char ')') $ do
>         space
>         l <- LExp
>         space
>         r <- LExp
>         space
>         return (App l r)
    
This is unfinished
% > lambdaAbstraction :: Parser LExp
% > lambdaAbstraction = between (space *> char '(') (space *do char)



> eof :: Parser ()
> eof = Parser $ \s -> if null s then Just ((),[]) else Nothing

% > spaceConsume :: Parser ()
% > spaceConsume = L.space space1 lineCmnt blockCmnt
% >   where
% >     lineCmnt  = L.skipLineComment "//"
% >     blockCmnt = L.skipBlockComment "/*" "*/"

% > parens :: Parser a -> Parser a
% > parens p = (char '(' *> p) <* char ')'

% > lambdaParser :: Parser LExp
% > lambdaParser = between spaceConsume eof stmt

> parseStmts :: [Token] -> Either String (Stmt,[Token])
> parseStmts ts = case parseStmt ts of
>   Right (stmts,ts') -> case ts' of
>     (TDot:ts'') -> Right (stmts,TDot:ts'')
>     (TIn:ts'') -> Right (stmts,TIn:ts'')
>     [] -> Right (stmts,[])
>     _ -> Left $ "Expected . before another statement. Found: " ++ show ts'
>   Left  e          -> Left e


> parseStmts' :: Stmt -> [Token] -> Either String (Stmt,[Token])
> parseStmts' lhs ts = case parseStmts ts of
>   Right (stmt,ts') -> Right (LExp lhs stmt, ts')
>   Left e -> Left e
>
>
> parseStmt :: [Token] -> Either String (Stmt,[Token])
> parseStmt (TLet:ts) = case parseLExp ts of
>   Left  e           -> Left e
>   Right (bexp, ts') -> parseLetStmt lexp ts'

> parseStmt (TId id:ts) = case parseAExpAssign id ts of
>   Right (aexp, ts') -> Right (Assign id aexp, ts')
>   Left  e           -> Left e
> parseStmt ts = Left $ "Not a valid statement. Found: " ++ show ts




> parseLetStmt :: Parser LExp
> parseLetStmt = do
>   rword "let"
>   stmt1  <- LExp
>   rword "in"
>   stmt2 <- stmt
>   return (If cond stmt1 stmt2)





> tryParse :: ([Token] -> Either String (a,[Token])) -> String -> a
> tryParse parser s =
>   case parser $ lexer s of
>     Right (e,[]) -> e
>     Right (_,ts) -> error $ "Parse error: expected EOF, found: " ++ show ts
>     Left e -> error $ "Parser error: " ++ e


> lambdaSyntax :: String -> LExp
> lambdaSyntax = tryParse parseStmts

> main :: IO ()
> main = return ()





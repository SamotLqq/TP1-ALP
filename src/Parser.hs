module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until"]
    , reservedOpNames = [ "++"
                        , "+"
                        , "--"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )


--Parser de variables
variableParser :: Parser (Exp Int)
variableParser = do v <- identifier lis
                    do  reservedOp lis "++"
                        return (VarInc v)
                        <|> do  reservedOp lis "--"
                                return (VarDec v)
                                <|> return (Var v)   


binaryOp2:: Parser (Exp Int -> Exp Int -> Exp Int)
binaryOp2 = do reservedOp lis "*"
               return (\a -> \b -> (Times a b))
               <|> do reservedOp lis "/"
                      return (\a -> \b -> (Div a b))



binaryOp1 :: Parser (Exp Int -> Exp Int -> Exp Int)
binaryOp1 = do reservedOp lis "+"
               return (\a -> \b -> (Plus a b))
               <|> do reservedOp lis "-"
                      return (\a -> \b -> (Minus a b))
                      
intexp2 :: Parser (Exp Int)
intexp2 = do e <- chainl1 intexpbase binaryOp2
             return e 

intexpbase :: Parser(Exp Int)
intexpbase = do n <- natural lis
                return (Const (fromIntegral n))
                <|> do e <- parens lis intexp
                       return e
                       <|> do reservedOp lis "-"
                              e <- intexpbase
                              return (UMinus e)
                              <|> do v <- variableParser
                                     return v
                                  

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp = do e <- chainl1 intexp2 binaryOp1
            return e

boolBinaryOp :: Parser (Exp Int -> Exp Int -> Exp Bool)
boolBinaryOp = do reservedOp lis "=="
                  return (\a -> \b -> (Eq a b))
                  <|> do reservedOp lis "!="
                         return (\a -> \b -> (NEq a b))
                         <|> do reservedOp lis ">"
                                return (\a -> \b -> (Gt a b))
                                <|> do reservedOp lis "<"
                                       return (\a -> \b -> (Lt a b))

boolAnd :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
boolAnd = do reservedOp lis "&&"
             return (\a -> \b -> (And a b))

boolOr :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
boolOr = do reservedOp lis "||"
            return (\a -> \b -> (Or a b))

boolBase :: Parser (Exp Bool)
boolBase = do reserved lis "true"
              return BTrue
              <|> do reserved lis "false"
                     return BFalse
                     <|> do e <- parens lis boolexp
                            return e            
                            <|> do reservedOp lis "!"
                                   e <- boolBase
                                   return (Not e)
                                   <|> do e <- boolexpInt
                                          return e

boolexpAnd :: Parser (Exp Bool)
boolexpAnd = do e <- chainl1 boolBase boolAnd
                return e

boolexpOr :: Parser (Exp Bool)
boolexpOr = do e <- chainl1 boolexpAnd boolOr
               return e

boolexpInt :: Parser (Exp Bool)
boolexpInt = do e1 <- intexp
                f <- boolBinaryOp
                e2 <- intexp
                return (f e1 e2)

------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = do e <- boolexpOr
             return e

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = undefined


------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError (Exp Bool)
parseComm = parse (totParser boolexp)

parseprueba :: SourceName -> String -> Exp Bool
parseprueba b a = case parse (totParser boolexp) b a of
                Right x -> x
                _ -> (BFalse)

parseprueba2 :: SourceName -> String -> Exp Int
parseprueba2 b a = case parse (totParser intexp) b a of
                Right x -> x
                _ -> (Var "error")
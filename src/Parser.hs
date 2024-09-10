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


-----------------------------------
--- Parser de expresiones enteras
-----------------------------------


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
                                  

intexp :: Parser (Exp Int)
intexp = do e <- chainl1 intexp2 binaryOp1
            return e


------------------------------------
--- Parser de expresiones booleanas
------------------------------------



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


boolexp :: Parser (Exp Bool)
boolexp = do e <- boolexpOr
             return e

-----------------------------------
--- Parser de comandos
-----------------------------------

commBase :: Parser Comm
commBase = do reserved lis "skip"
              return Skip
              <|> do v <- identifier lis
                     reservedOp lis "="
                     e <- intexp
                     return (Let v e)
                     <|> do reserved lis "if"
                            b <- boolexp
                            reserved lis "then"
                            c <- braces lis comm
                            reserved lis "else"
                            c2 <- braces lis comm
                            return (IfThenElse b c c2)
                            <|> do reserved lis "repeat"
                                   c <- braces lis comm
                                   reserved lis "until"
                                   b <- boolexp
                                   return (RepeatUntil c b)

commSeq :: Parser (Comm -> Comm -> Comm)
commSeq = do reservedOp lis ";"
             return (\c -> \c1 -> (Seq c c1))


comm :: Parser Comm
comm = do c <- chainl1 commBase commSeq
          return c


------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)

parseBool :: SourceName -> String -> Either ParseError (Exp Bool)
parseBool = parse (totParser boolexp)

parseInt = parse (totParser intexp)

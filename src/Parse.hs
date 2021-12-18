{-# LANGUAGE RecordWildCards #-}
module Parse where

import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Expr
import           Text.Parsec.Language (javaStyle)
import           Text.Parsec.String
import qualified Text.Parsec.Token    as Token

import           Imp                  (AExp (..), BExp (..), Cmd (..), Name (..))

-- pretending Haskell has a good module system…
Token.TokenParser {..} = Token.makeTokenParser javaStyle

binary name fun = Infix (fun <$ reservedOp name) AssocLeft

name = Name <$> identifier

aexp :: Parser AExp
aexp = buildExpressionParser table term
 where term =  Lit . fromIntegral <$> integer
           <|> Var <$> name
           <|> parens aexp
           <|> between (char '{') (char '}') cmd >>= (\x -> name >>= (\y -> return $ Bracket x y))
           <|> SetA <$> (name <* reservedOp ":=") <*> aexp
           <|> name >>= (\n -> parens $ many aexp >>= (\y -> return $ Call n y))
           <|> between (char '[') (char ']') aexp >>= (\y -> case opt (reservedOp ":=" >* aexp) of
             Nothing -> Load y
             Just x -> Store y x)
             <|> char '&' >> name >>= (\x -> return $ FnPtr x 0)
             <|> string "all" >> (return All)
             <|> string "bound" >> bexp >>= (\x -> return $ Bound x)
       table = [ [ binary "+" (:+:), binary "-" (:-:) ]
               , [ binary "*" (:*:), binary "/" (:/:) ]
               ]

bexp :: Parser BExp
bexp = buildExpressionParser table term
  where term =  True' <$ reserved "true"
            <|> False' <$ reserved "false"
            <|> try ((:<=:) <$> (aexp <* reservedOp "<=") <*> aexp)
            <|> try ((:==:) <$> (aexp <* reservedOp "==") <*> aexp)
            <|> try (parens bexp)
        table = [ [ Prefix (Not <$ reservedOp "!") ]
                , [ binary "&&" (:&:), binary "||" (:|:) ]
                ]

cmd :: Parser Cmd
cmd = foldl Seq Skip <$> (statement `sepBy1` symbol ";")
  where statement =  If <$> (reserved "if" *> bexp)
                        <*> braces cmd
                        <*> (reserved "else" *> braces cmd)
                 <|> While <$> (reserved "while" *> bexp)
                           <*> braces cmd
                 <|> Set <$> (name <* reservedOp ":=") <*> aexp
                 <|> Do <$> aexp
                 <|> string "fn" >> name >>= (\n -> parens $ many name >>= (\a -> braces cmd >>= (\b -> return $ Fn n b a)))

parseCmd = parse (cmd <* eof)

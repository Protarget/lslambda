module LSLambda.AST.Parser (parseAST) where
    
import LSLambda.AST
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

astLanguageDef :: P.LanguageDef st
astLanguageDef = P.LanguageDef {
    P.commentStart = "",
    P.commentEnd = "",
    P.commentLine = "",
    P.nestedComments = False,
    P.identStart = letter <|> oneOf ":!#$%&*+./<=>?@\\^|-~",
    P.identLetter = alphaNum <|> oneOf ":!#$%&*+./<=>?@\\^|-~",
    P.opStart = oneOf "",
    P.opLetter = oneOf "",
    P.reservedNames = ["lambda", "let"],
    P.reservedOpNames = [],
    P.caseSensitive = True
}

lexer = P.makeTokenParser astLanguageDef

tokenParens = P.parens lexer
tokenIdentifier = P.identifier lexer
tokenLambda = P.reserved lexer "lambda"
tokenLet = P.reserved lexer "let"
tokenNumber = P.naturalOrFloat lexer
tokenString = P.stringLiteral lexer

lambdaParser = tokenParens $ do
    tokenLambda
    p <- tokenParens $ many tokenIdentifier
    b <- valueParser
    return (LambdaNode p b ())
    
letParser = tokenParens $ do
    tokenLet
    n <- tokenIdentifier
    v <- valueParser
    b <- valueParser
    return (LetNode n v b ())
    
applicationParser = tokenParens $ do
    f <- valueParser
    p <- many valueParser
    return $ ApplyNode f p ()
    
stringParser = do
    s <- tokenString
    return $ LiteralNode (LiteralString s) ()
    
numberParser = do
    n <- tokenNumber
    case n of
        Left i -> return $ LiteralNode (LiteralInteger i) ()
        Right i -> return $ LiteralNode (LiteralFloat i) ()
        
variableParser = do
    i <- tokenIdentifier
    return (VariableNode i ())

valueParser = choice [try lambdaParser, try letParser, try applicationParser, stringParser, numberParser, variableParser]

parseAST :: String -> SyntaxNode ()
parseAST c = case parse valueParser "" c of
    Left e -> error (show e)
    Right v -> v
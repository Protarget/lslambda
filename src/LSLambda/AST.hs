{-# LANGUAGE DeriveFunctor #-}

module LSLambda.AST where
    
data LiteralValue = LiteralInteger Integer | LiteralFloat Double | LiteralString String deriving (Eq, Ord, Show)
data SyntaxNode a = LiteralNode LiteralValue a | VariableNode String a | LetNode String (SyntaxNode a) (SyntaxNode a) a | LambdaNode [String] (SyntaxNode a) a | ApplyNode (SyntaxNode a) [SyntaxNode a] a deriving (Eq, Ord, Show, Functor)
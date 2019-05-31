{-# LANGUAGE FlexibleInstances #-}

module LSLambda.Type (
    TypeVariableName(..),
    Type(..),
    FreeTypeVariables(..),
    TypeSubstitution(..),
    TypeConstraint(..),
    CanApplyTypeSubstitution(..),
    HasFreeTypeVariables(..),
    substitute,
    (.|>),
    (<|.),
    unify,
    unifyConstraints,
    constrain,
    ) where
    
import Data.Map as M
import Data.Set as S
import Data.Foldable as F
    
-- We wrap the type variable names in a wrapper so that we can distinguish between the full type ADT and type variables
newtype TypeVariableName = TypeVariableName String deriving (Eq, Ord, Show)

-- The type grammar, featuring:
-- * Type Constructors taking arbitrary numbers of type parameters
-- * Type Variables
-- * Functions taking a tuple of values and returning a single value
data Type = TypeConstructor String [Type] | TypeVariable TypeVariableName | TypeFunction [Type] Type deriving (Eq, Ord, Show)

-- Sets of free type variables
type FreeTypeVariables = S.Set TypeVariableName

-- Represents a translation that can be applied to an object to replace type variables with concrete types
type TypeSubstitution = M.Map TypeVariableName Type

-- Represents a constraint specifying that two types should be unified
data TypeConstraint = TypeConstraint Type Type deriving (Eq, Ord, Show)

-- Represents an error that can take place during type unification
data TypeUnificationError = TypeMismatchError Type Type | TypeLengthMismatch deriving (Eq, Ord, Show)

-- Something that can have a type substitution applied to it
class CanApplyTypeSubstitution a where
    applySubstitution :: TypeSubstitution -> a -> a
    ($|>) :: TypeSubstitution -> a -> a
    ($|>) = applySubstitution
    (<|$) :: a -> TypeSubstitution -> a
    (<|$) = flip applySubstitution
    
-- Obviously, we can apply a type substitution directly to types
instance CanApplyTypeSubstitution Type where
    applySubstitution s (TypeFunction p r) = TypeFunction (fmap (applySubstitution s) p) (applySubstitution s r)
    applySubstitution s (TypeConstructor n tp) = TypeConstructor n (fmap (applySubstitution s) tp)
    applySubstitution s t@(TypeVariable n) = M.findWithDefault t n s

-- We can also apply a type substitution to any functor containing types by the obvious method
instance (Functor f, CanApplyTypeSubstitution a) => CanApplyTypeSubstitution (f a) where
    applySubstitution s = fmap (applySubstitution s)
    
-- We can apply a substitution to a type constraint becausae its a simple pair of types
instance CanApplyTypeSubstitution TypeConstraint where
    applySubstitution s (TypeConstraint a b) = TypeConstraint (s $|> a) (s $|> b)
    
-- Something that can have 0 or more free type variables
class HasFreeTypeVariables a where
    getFreeTypeVariables :: a -> FreeTypeVariables
    
-- In a raw type, all variables are free
instance HasFreeTypeVariables Type where
    getFreeTypeVariables (TypeVariable t) = S.singleton t
    getFreeTypeVariables (TypeConstructor _ tvs) = F.foldl (S.union) (S.empty) (fmap getFreeTypeVariables tvs)
    getFreeTypeVariables (TypeFunction pts rt) = S.union (F.foldl (S.union) (S.empty) (fmap getFreeTypeVariables pts)) (getFreeTypeVariables rt)
    
-- Similarly, in a foldable functor of types, all variables are free
instance (Foldable f, Functor f, HasFreeTypeVariables a) => HasFreeTypeVariables (f a) where
    getFreeTypeVariables x = F.foldl (S.union) (S.empty) (fmap getFreeTypeVariables x)
    
-- Create a substitution mapping type variable for a type
substitute :: TypeVariableName -> Type -> TypeSubstitution
substitute = M.singleton

-- Create a constraint specifying that two types should be unified
constrain :: Type -> Type -> TypeConstraint
constrain = TypeConstraint

-- Compose one substitution with another
substitutionCompose :: TypeSubstitution -> TypeSubstitution -> TypeSubstitution
substitutionCompose a b = M.union (a $|> b) a

-- Shorthand for composition
(.|>) = substitutionCompose
(<|.) = flip substitutionCompose

-- Attempt to unify two types, producing either a substitution that would make them equivalent or an error
unify :: Type -> Type -> Either TypeUnificationError TypeSubstitution
unify a b
    | a == b = Right M.empty
unify (TypeVariable a) b = Right (substitute a b)
unify a (TypeVariable b) = Right (substitute b a)
unify (TypeFunction ps1 r1) (TypeFunction ps2 r2) = unifyMany (ps1 ++ [r1]) (ps2 ++ [r2])
unify (TypeConstructor _ ta@(_:_)) (TypeConstructor _ tb@(_:_)) = unifyMany ta tb
unify a b = Left (TypeMismatchError a b)

-- Apply a zipped unification over two lists of types 
unifyMany :: [Type] -> [Type] -> Either TypeUnificationError TypeSubstitution
unifyMany [] [] = Right M.empty
unifyMany [] a = Left TypeLengthMismatch
unifyMany a [] = Left TypeLengthMismatch
unifyMany (a:as) (b:bs) = do
    s1 <- unify a b -- Unify the head of the list
    s2 <- unifyMany (applySubstitution s1 as) (applySubstitution s1 bs) -- Apply the substitution to the tail of the list and unify the remainder
    return (s2 .|> s1) -- Compose the resulting substitutions
    
-- Given a list of constraints, create either a substitution that can satisfy them or an error
unifyConstraints :: [TypeConstraint] -> Either TypeUnificationError TypeSubstitution
unifyConstraints = unifyConstraintsRepeated (M.empty)

unifyConstraintsRepeated :: TypeSubstitution -> [TypeConstraint] -> Either TypeUnificationError TypeSubstitution
unifyConstraintsRepeated s [] = Right s
unifyConstraintsRepeated s ((TypeConstraint a b):cs) = do
    ns <- unify a b
    unifyConstraintsRepeated (ns .|> s) (ns $|> cs)
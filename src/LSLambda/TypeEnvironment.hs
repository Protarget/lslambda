module LSLambda.TypeEnvironment where
    
import LSLambda.Type
import Data.Map as M
import Data.Set as S
import Data.Foldable as F

-- The type scheme is a collection of bound variables and a type in which they exist
data TypeScheme = TypeScheme FreeTypeVariables Type deriving (Eq, Ord, Show)

-- A type environment is a map of symbol names to type schemes
type TypeEnvironment = M.Map String TypeScheme

-- We apply substitution only to variables not bound by the type scheme
instance CanApplyTypeSubstitution TypeScheme where
    applySubstitution s (TypeScheme as t) = TypeScheme as (applySubstitution ns t)
        where
            ns = F.foldr M.delete s as
            
-- The type scheme binds its variables, so we return only the free variables from the type
instance HasFreeTypeVariables TypeScheme where
    getFreeTypeVariables (TypeScheme b f) = S.difference (getFreeTypeVariables f) b
    
-- Given a type environment and a type, create a type scheme from a type by binding type variables to the environment
generalize :: TypeEnvironment -> Type -> TypeScheme
generalize e t = TypeScheme ft t
    where
        ft = S.difference (getFreeTypeVariables t) (getFreeTypeVariables e)
{-# LANGUAGE OverloadedStrings, NamedFieldPuns, FlexibleInstances #-}

-- trying to clean up the intermediate language around furhter ideas:
-- everything is a tuple
-- types are first class
-- manipulate tuples as we like
-- etc

module DotNet.Tuples where

import Data.Functor.Identity
import Control.Monad.IO.Class

import DotNet.Syntax

-- since we want to be REALLY flexible in type constraints, they can be:
-- specific type
-- typeclass(es)
-- tuple with specific fields and type constraints in turn
-- etc
-- mutually recursive with tuple - as tuple itself can be a type constraint
data RHSConstraint = TypeConstraint Type | TupleConstraint Tuple | KindConstraint Kind

-- tuple - we manipulate THESE both in our language and at runtime, central object of sorts
-- e.g., all VALUES are Tuples etc
-- It's basically list of fields with types, either named or unnamed
-- we ENCOURAGE naming fields as it leads to BETTER ERROR MESSAGES and reduces
-- writing functions, e.g. head and tail are automatic for lists
data Tuple = UnnamedTuple [RHSConstraint] | NamedTuple [(Name, RHSConstraint)]

{-
type List a = Nil + (::) head:a * tail:(List a);
-}
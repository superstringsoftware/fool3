{-
New concept:
- Records with named fields (mandatory!)
- Everything is a Record! (type class - record of functions, type family - same plus data type etc)
- Functions from values, types, kinds... - so basically, type families with dependent types on steroids
- Effects for, well, effects (no need in IO monad)
- Manipulate records directly (instead of currying), partial application can be supported that way too 
------------------------------
For the surface language though:
- Types and Kinds(? not initially) are first class, so can have functions on them as well as values
- Type Families that include classes, but use only one keyword for it
- Records for product types (do we allow anonymous?), EXTENSIBLE (so it's like inheritance)
- Sum Types? Extensible from the start? Apparently it complicates type checking?
- Mutability and stuff via effects (Realms?)
- Objects???
-}

type Bool {
    True, False
}

type Maybe a:* {
    Nothing
    Just :a
}

type List a:* {
    ([])
    (::) :a :(List a)
}

type Person {
    fname, lname : String;
    age : Int;
    dob : Date;
}

class Eq a:* {
    (==):Bool { x, y : a } = not (x /= y)
  , (/=):Bool { x, y : a } = not (x == y) 
}

class Functor f:*->* {
    fmap:f b { g:a->b, arg:f a };
}

∀n (n:Int, n > 0) => (
fact 1 = 1
fact n = n * fact (n-1)
(fact n) ∊ Int
)

∀a,x,y (a:*, x:a, y:a, OrdPair x y) => (x>y)

type OrdPair {
    OrdPair x:a y:a = if (x>y) then {x,y} else {y,x}
}
-- fact:Int n (∀n ∊ Int, n > 0) = 

-- Examples
data List a:Type 
    Nil
    (::) head:a tail:(List a)
-- or
data List a:Type = { Nil, (::) head:a tail:(List a) }

count lst:(List a) = cons lst?
    Nil => 0
    otherwise => 1 + count lst.tail
-- or
count Nil = 0
count x::xs = 1 + count xs
-- shoot, anonymous pattern matching is much cleaner and more concise

typef Semigroup a:Type 
    (+) {x:a, y:a} -> a
    -- or
    x:a + y:a -> a
    -- constraint
    forall x:a, y:a, z:a => x + (y + z) == (x + y) + z 

typef Monoid a:(exists Semigroup a)
    Z0:a
    -- constraints
    forall x:a => {
        Z0 + x == x, x + Z0 == x
    }

-- implementation
Monoid Int = {
    (I# x) + (I# y) = I# (x+y),
    Z0 = 0
}


-- Mutability via dependent type families
typef Mutable {record:Type, idt:Type, re:Realm} = {
    data MutableRecord = record * (id:idt) -- extending "record" type with the field id of type idt - so it's a new type
    effect new {r:record, id:idt} -> MutableRecord 
    effect new r:record -> MutableRecord -- implicit id
    effect read mr:MutableRecord -> record
    effect delete mr:MutableRecord -> ()
    effect update {mr:MutableRecord, uf: record->record } -> MutableRecord
}

-- different Mutable implementations (partial?)
typef MemoryMutable record:Type = Mutable {record, MemRef, MemoryRealm} = {
    new r:record = MutableRecord {r, &r}

}

-- So, we have concrete types which are all Sum Types even if they contain only 1 constructor
-- AND : type functions, which produce a concrete type
-- so, should we even distinguish between normal functions and type functions??

fact {n:Int} -> Int where n > 0 -- signature with a constraint, function from int to int
fact 0 = 1
fact n = n * fact (n-1)

List {a:Type} -> Type -- signature with a constraint, function from type to type
List a = {Nil, (::) {head:a, tail:List a} }

Maybe {a:Type} -> Type
Maybe a = {Nothing, Just :a}

Person:Type
Person = Person {name:String, age:Int, dob:Date}

Monoid {a:Type} -> Class
Monoid a = {
    Z0:a,
    (+) {x:a, y:a} -> a
}
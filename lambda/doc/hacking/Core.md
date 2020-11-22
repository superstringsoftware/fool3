# Core language concepts

Since the architecture of our compiler can support multiple "Realms" - compilation targets with additional functionality around it - as well as surface languages, the core of it is, well, in the Core language and machinery.

Just as Haskell is based on the classic STG paper by SPJ, our Core language and machinery is based on the *Record-based lambda calculus* (working title which will probably change). The idea is that we base all types and values manipulations on typed records - collections of fields. This way, we can provide all the same benefits as "regular" lambda-calculus (mostly, currying), while making operational mechanics a bit easier to write, and, what's probably more important, easier to compile to non-low level targets (e.g., .Net or JavaScript). 

## Basic concepts

### Records

Main object is the record:
```Haskell
Record = ConsTag [(name, type, value)ᵢ] : rectype
```
Record is a list of triples of `name`, `type`, `value` fields, with an optional constructor tag for value records and a type for the record as a whole, where:
-  `name` is the name of the field. Can be empty for anonymous records. If any field is anonymous, all the record will be anonymous.
-  `type` is the type of the field
-  `value` is the value of the field. Can be used to provide values when calling a function, or give default values inside the function definition etc.
-  `ConsTag` is only used for the value records - when a record is contructed by a constructor function (as opposed to e.g. record of parameters for the function call or a typeclass record etc)
-  `rectype` is the type of the record as a whole

This object allows us to represent pretty much anything in our language: values, function arguments, arguments in the function application, typeclasses, even modules.

To represent this in Haskell, we use 2 types of records:
```Haskell
data Field = Field {
    fname :: Name,
    ftype :: Type,
    fval  :: Expr
}
-- regular record is simply list of Fields
type Record = [Field] 
-- full record that also has a constructor tag and a type, used primarily for values
data FullRecord = FullRecord {
    rec     :: Record,
    rtype   :: Type,
    consTag :: ConsTag
}
```

### Lambdas
Second main object is a `lambda`, representing any function or value in our Core language. This includes type definitions, constructor functions, "regular" functions, typeclasses etc. Since types are first class objects in our language, we can have functions dependent and pattern matched on types of the arguments, not only constructors; also, return type of the function can depend on its arguments (*do we want to treat such functions separately though? they wouldn't be composable generally, and we do want our arrows to compose...*)
```Haskell
f = λ { xᵢ[[:tᵢ] = valᵢ] } . <expr> : type
```
So, a function is given a record of arguments and returns an expression. When we apply a function, we simply compare the record that is given to the function as an argument with the record that the function expects and proceed accordingly: does it typecheck? is the number of arguments the same as the function arity? etc.

### Pattern matching
Pattern match is simply a function from the argument to `Bool`. If it returns `True`, the pattern is matched. We are using the regular pattern matches like in haskell to deconstruct the values, but we also pattern match on *types* - this is used for typeclasses and potentially implicit argument functions - as well as different predicates, e.g.:
```Haskell
f : Int -> Int
f (∀x>0 => x) = sqrt x

-- typeclass function internal representation:
g x:Int y:Int = x + y
g x:String y:String = "Hello, " ++ x 
```
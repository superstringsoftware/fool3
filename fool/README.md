## FOOL3 = Functional Object Oriented Low-Level Language

The name is a bit of a joke, obviously. Currently, it's very much functional, might be reasonably low-level for better re-use of existing c-libraries, and most likely never really OO. In it's current state, it is quite useful for teaching Algebraic Data Types and lambda calculus, doing it in a user-friendly visual way (it is colored in the terminal :)):

#### Source Code
```haskell
data Bool = True | False;
data Maybe a = Just a | Nothing;
data List a = Cell a (List a) | Nil;
data Either a b = Left a | Right b;

fact n = if n == 0 then 1 else n * fact (n - 1);
square x = x * x;
quad x = square x * square x;
```

#### Inspecting Core
```haskell
Bool = Bool {True, False} 
Either = λa:*. λb:*. Either {λ:a. Left {v0} , λ:b. Right {v0} } 
List = λa:*. List {λ:a. λ:(List a). Cell {v0, v1} , Nil} 
Maybe = λa:*. Maybe {λ:a. Just {v0} , Nothing} 

fact = λn:?. if (==) n 0 then 1 else (*) n fact (-) n 1
quad = λx:?. (*) square x square x
square = λx:?. (*) x x
```

#### Tracing function calls step-by-step
```
λfool3. square 3
Received expressions: 
FlApp (SymId "square") (PInt 3)
Converted to:
square 3
[1]	(λx:?. (*) x x)(3)
[2]	((*) 3)(3)
[3]	9.0

λfool3. fact 2
Received expressions: 
FlApp (SymId "fact") (PInt 2)
Converted to:
fact 2
[1]	(λn:?. if (==) n 0 then 1 else (*) n fact (-) n 1)(2)
[2]	if (==) 2 0 then 1 else (*) 2 fact (-) 2 1
[3]	((*) 2)(λn:?. if (==) n 0 then 1 else (*) n fact (-) n 1 1.0)
[4]	((*) 2)(if (==) 1.0 0 then 1 else (*) 1.0 fact (-) 1.0 1)
[5]	((*) 2)((*) 1.0 λn:?. if (==) n 0 then 1 else (*) n fact (-) n 1 0.0)
[6]	((*) 2)((*) 1.0 if (==) 0.0 0 then 1 else (*) 0.0 fact (-) 0.0 1)
[7]	((*) 2)((*) 1.0 1)
[8]	((*) 2)(1.0)
[9]	2.0
```


### Old Design Principles / Ideas to explore

- Functional math-heavy foundation with efficient vectors and static ADT, pure, immutable etc
- Dependent types
- *Everything* is a function (including types, which are first class values)
- Dynamic product types (can form tuples of different types on the fly) -- subtyping should get there?
- Mutable things support via Actions (vs pure Functions). Should References be implemented via Actions?
- Built in Graph functionality support between records, with Arrows that are themselves records - for better real life / enterprise data modeling
- Built-in security (or on top?)
- Built-in computational nodes support - application can be aware of different containers, vms, servers, clients etc where parts of its' code are running and where the data lives
- Pubsub, reactive and event based support (how??? have no idea at this point, needs design)
- Compilation to C (eventually llvm), .Net and JS - via different *node targets*
- VERY EASY FFI with C - to reuse all the libraries there are
- OO part: Objects can contain base data types (records / Sum Types) and support mutation of state via Actions --> e.g., implement GVM and Nodes via Objects?

## Installation

Simply clone it, build it with `stack` (you need haskell for this obviously), and run fool-exe:

```
stack setup
stack build
stack exec fool-exe
```

Don't think anybody needs a binary at this stage, but once the language develops, will provide.

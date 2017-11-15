FOOL = Functional Object Oriented Language
===========================================

- Functional math-heavy foundation with efficient vectors and static ADT, pure, immutable etc
- Dependent types (e.g., for Vector Float 4 support etc?)
- Dynamic product types (can form tuples of different types on the fly) -- subtyping should get there?
- Mutable things support via Actions (vs pure Functions). Should References be implemented via Actions?
- Records can have functions as members *if* these functions only operate on the record fields and are pure (e.g., Person = fname, lname : String; name = lname + fname : String;)
- Built in Graph functionality support between records, with Arrows that are themselves records
- Type function based support for Persistence - if we have a record Person, type MongoCollection Person stores it there and handles all functions etc
- Built-in security (or on top?)
- Built-in computational nodes support - application can be aware of different containers, vms, servers, clients etc where parts of its' code are running and where the data lives
- Pubsub, reactive and event based support (how??? have no idea at this point, needs design)
- Compilation to C (eventually llvm) and JS
- VERY EASY FFI with C - to reuse all the libraries there are
- OO part: Objects can contain base data types (records / Sum Types) and support mutation of state via Actions --> e.g., implement GVM and Nodes via Objects?

Ideas to ponder on
-----------------------------

- Can we treat Actions as mapping pure data to / from a separate Category - e.g. printing to stdout is mapping pure data to terminal, showing on screen - same to computer screen (or Browser rendering engine), getting input from keyboard - same but mapping from Keyboard to our pure datatypes etc.?
- How about .Net compilation for easy windows integration?
- How about low-level functional approach - abstracting llvm / CPU a little bit, so that then we can write an OS easily?


Initial code ideas with math-based syntax (needs unicode...)
--------------------------------------------------------

-- vector:
map (f, X::Vector) ≡ <f (x) | ∀x ∊ X>

-- list
map (f, X::List) ≡ [f (x) | ∀x ∊ X]

-- generic container, from whom we should be able to derive both Vector and List:
map (f, X) ≡ {f (x) | ∀x ∊ X} -- it just means whatever container type is X the result is the same container type

-- range in a vector - needs to be defined via memcpy() or similar for efficiency
range (k,n,X) = <xᵢ | ∀i ∊ {k..n}>

-- hashtable, one of the options where f touches only values not keys
-- using '{}' for generic container?
map f X ≡ {key x → f (value x) | ∀x ∊ X}

-- iteration / 'for' cycle
∀i ∊ {k..n} action(i,...)

-- build a Vector with a generator function
v = <gen(i) | ∀i ∊ {k..n}>
-- e.g., random vector of size n
v = <random | ∀i ∊ {0..n}>
-- since i is not used in a function call, can shorten:
v = <random | {0..n}>
-- also we probably don't need the "forall" quantifier here

-- how about simply -
v = <random | times n>

example
- generate vector of random ints
- map a function on it
- print the result


extern random, print

map (f, X) ≡ {f (x) | ∀x ∊ X}
square (x) ≡ x * x

v1 = <random | times 10>
v2 = map (square, v1)

print (v1, v2)

---------------------------
-- Records
data Address = city, street, country : String;
data Person = fname, lname : String; address : Address; dob : Date;

-- can form records on the fly, it's a dynamic product type after all,
-- useful e.g. in function calls like print above - can give arbitrary tuple
-- as an argument and it just calls print on all of them in order

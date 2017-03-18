FOOL = Functional Object Oriented Language
===========================================

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

v1 = <random | times n>
v2 = map (f, v1)

print (v1, v2)

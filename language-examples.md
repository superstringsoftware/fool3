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

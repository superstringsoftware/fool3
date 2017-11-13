# Notes about RTS

### Internal values representation

To represent algebraic datatype values -

```
data D = C1 a b c | C2 d e
```

we will want to use 32 byte word encoding:

* 24 bytes: index of the type in the global type table (this way we'll be able to support dynamic and dependent types)
* 8 bytes: index of the constructor *within* the type (don't think anybody would construct a type with more than 256 constructors??)
* what if it's a 1-constructor type, should we just unpack it altogether?

Since e.g. ghc erases type info but keeps the constructor index encoded and checked anyway, why not store the same for types?

Then we'll have - primitive value types (numbers, small bytes etc) stored directly following the encoding byte, anything more complex or other ADTs, or Arrays - as a reference.

> **NB:** need to find a way to store numbers, bytes and other primitives as UNBOXED in all cases automatically

### Memory and garbage collection

Use similar approach to GHC - preallocate a huge byte array for heap and another as a stack. Then simply increment pointers to free space and periodically garbage collect and defragment the heap. Stack is automatic.

> **Q:** how do we handle threads from the start? Should we think about having separate memory spaces for threads from the start? How to implement STM and TVar / MVar?

-- starting from an even lower level than base - primitive types and ops,
-- which still need to be declared but with a keyword and without implementation (it's hidden)

primitive type Int
primitive type Float
primitive type Byte
primitive type Pointer

primitive type IntArray
primitive type FloatArray
primitive type ByteArray

-- can use the primops, but they are implemented in the RTS
primop allocateIntArray :: Int -> IntArray
primop allocateFloatArray :: Int -> FloatArray
primop allocateByteArray :: Int -> ByteArray

-- depending on the type of the node, can expose other operations, e.g.:
-- baremetal machine - C Arrays
-- .Net target - .Net arrays etc

-- unsafe because no bounds checking
unsafe primop (_) :: Array a -> a -- indexing an array: v_2 (_ can be changed to subscript in the editor) - somewhat ugly, just use '!'?

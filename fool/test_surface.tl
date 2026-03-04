// =============================================================================
// tulam — Comprehensive Surface Language Test
// =============================================================================
// This file exercises EVERY surface language feature, whether implemented or not.
// If it parses and runs, the feature works. If it fails, we know what's next.
// =============================================================================

// =============================================================================
// SECTION 1: COMMENTS
// =============================================================================

// Single-line comment

/* Multi-line
   comment */

// =============================================================================
// SECTION 2: BASIC SUM TYPES
// =============================================================================

// 2.1 Simple enumeration (no parameters, nullary constructors)
type Bool = { True, False };

// 2.2 Recursive sum type with one argument
type Nat = {
  Z,
  Succ(n:Nat)
};

// 2.3 Parameterized sum type (one type param)
type Maybe(a:Type) = { Nothing, Just(x:a) };

// 2.4 Parameterized sum type (two type params)
type Either(a:Type, b:Type) = { Left(x:a), Right(y:b) };

// 2.5 Recursive parameterized type
type List(a:Type) = { Nil, Cons(head:a, tail:List(a)) };

// 2.6 Multiple constructor arguments
type Tree(a:Type) = {
  Leaf(val:a),
  Branch(left:Tree(a), right:Tree(a))
};

// 2.7 Constructor wrapping a value
type ConstructorTag = { ConstructorTag(n:Nat) };

// =============================================================================
// SECTION 3: GADTs (per-constructor return types)
// =============================================================================

// 3.1 Length-indexed vector
type Vec(a:Type, n:Nat) = {
    VNil : Vec(a, Z),
    VCons(head:a, tail:Vec(a, n)) : Vec(a, Succ(n))
};

// 3.2 Propositional equality
type PropEqT(a:Type, x:a, y:a) = {
    Refl : PropEqT(a, x, x)
};

// 3.3 Well-typed expression GADT
type Expr(a:Type) = {
    LitI(n:Nat) : Expr(Nat),
    LitB(b:Bool) : Expr(Bool),
    Add(x:Expr(Nat), y:Expr(Nat)) : Expr(Nat),
    IfE(cond:Expr(Bool), then_:Expr(a), else_:Expr(a)) : Expr(a)
};

// =============================================================================
// SECTION 4: RECORDS (single-constructor product types)
// =============================================================================

// 4.1 Basic record
record Point = { x:Nat, y:Nat };

// 4.2 Parameterized record
record Pair(a:Type, b:Type) = { fst:a, snd:b };

// 4.3 Record spread (include fields from another record)
record Point3D = { ..Point, z:Nat };

// 4.4 Parameterized record with spread
record TaggedPoint(a:Type) = { ..Point, tag:a };

// 4.5 Deeply nested spread
record Point4D = { ..Point3D, w:Nat };

// =============================================================================
// SECTION 5: FUNCTIONS — basic
// =============================================================================

// 5.1 Identity (polymorphic, untyped body)
function id(x:a) : a = x;

// 5.2 Constant function
function const(x:a, y:b) : a = x;

// 5.3 Function returning a list literal
function f(x) = [x, 18, 29];

// 5.4 Type introspection primitives
function typeOf(ex:tp) : Type = tp;
function consOf(ex:tp) : ConstructorTag = primop#;

// 5.5 No return type annotation
function g(x, y) = x;

// =============================================================================
// SECTION 6: FUNCTIONS — pattern matching
// =============================================================================

// 6.1 Single-argument pattern match
function not(b:Bool) : Bool = {
    {True}  -> False,
    {False} -> True
};

// 6.2 Multi-argument pattern match
function eq(x:Nat, y:Nat) : Bool = {
    {Z, Z}             -> True,
    {Z, n}             -> False,
    {n, Z}             -> False,
    {Succ(m), Succ(n)} -> eq(m, n)
};

// 6.3 Recursive multi-arg function
function plus(x:Nat, y:Nat) : Nat = {
    {Z, n}       -> n,
    {Succ(n), m} -> Succ(plus(n, m))
};

// 6.4 Deep nested pattern
function deep(x:Nat) : Nat = {
    {Succ(Succ(n))} -> n,
    {n}             -> n
};

// 6.5 Pattern matching on parameterized type
function isNothing(x:Maybe(a)) : Bool = {
    {Nothing} -> True,
    {Just(v)} -> False
};

// 6.6 Pattern on List
function head(xs:List(a)) : Maybe(a) = {
    {Nil}          -> Nothing,
    {Cons(h, t)}   -> Just(h)
};

function tail(xs:List(a)) : List(a) = {
    {Nil}          -> Nil,
    {Cons(h, t)}   -> t
};

function length(xs:List(a)) : Nat = {
    {Nil}          -> Z,
    {Cons(h, t)}   -> Succ(length(t))
};

// =============================================================================
// SECTION 7: OPERATORS AS FUNCTIONS
// =============================================================================

// 7.1 Named operator function
function (+)(x:Nat, y:Nat) : Nat = plus(x, y);

// 7.2 Subtraction (partial, Nat)
function (-)(x:Nat, y:Nat) : Nat = {
    {n, Z}             -> n,
    {Z, n}             -> Z,
    {Succ(n), Succ(m)} -> n - m
};

// 7.3 Multiplication
function (*)(x:Nat, y:Nat) : Nat = {
    {Z, n}       -> Z,
    {Succ(n), m} -> m + n * m
};

// =============================================================================
// SECTION 8: IF / THEN / ELSE
// =============================================================================

// 8.1 Simple conditional
function testIf(b:Bool) : Nat = if b then Succ(Z) else Z;

// 8.2 Nested conditionals
function clamp(x:Nat) : Nat =
    if eq(x, Z) then Z
    else if eq(x, Succ(Succ(Succ(Z)))) then Succ(Succ(Z))
    else x;

// 8.3 Conditional in a let body
function testIfLet(b:Bool) : Nat =
    let one = Succ(Z) in if b then plus(one, one) else one;

// =============================================================================
// SECTION 9: LET / IN
// =============================================================================

// 9.1 Single let binding
function testLet(n:Nat) : Nat = let one = Succ(Z) in plus(n, one);

// 9.2 Multiple let bindings
function testLet2(n:Nat) : Nat =
    let a = Succ(Z), b = Succ(Succ(Z)) in plus(a, plus(b, n));

// 9.3 Typed let binding
function testLetTyped(n:Nat) : Nat =
    let x:Nat = Succ(Z) in plus(x, n);

// 9.4 Nested let/in
function testLetNested(n:Nat) : Nat =
    let x = Succ(Z) in let y = Succ(x) in plus(y, n);

// =============================================================================
// SECTION 10: LITERALS AND CONTAINERS
// =============================================================================

// 10.1 Integer literals
function intLit() : Nat = 42;

// 10.2 Float literals
function floatLit() = 3.14;

// 10.3 String literals
function strLit() = "hello world";

// 10.4 List literal
function listLit() = [1, 2, 3];

// 10.5 Tuple literal
function tupleLit() = {1, 2, 3};

// 10.6 Vector literal
function vecLit() = <1, 2, 3>;

// 10.7 Empty containers
function emptyList() = [];
function emptyTuple() = {};

// =============================================================================
// SECTION 11: BINARY AND UNARY OPERATORS
// =============================================================================

// 11.1 Primitive operators
function testPrimAdd(x, y) = x +# y;
function testPrimSub(x, y) = x -# y;
function testPrimMul(x, y) = x *# y;
function testPrimDiv(x, y) = x /# y;

// 11.2 Custom operator usage
function double(x:Nat) : Nat = x + x;

// 11.3 Operator precedence: * before +
function testPrec(x, y, z) = x + y * z;

// 11.4 Equality operator
function testEq(x:Nat, y:Nat) : Bool = x == y;

// =============================================================================
// SECTION 12: STRUCTURES (TYPECLASSES) — general keyword
// =============================================================================

// 12.1 Basic structure with default implementations
structure Show(a:Type) = {
    function show(x:a) : List(Nat)
};

// 12.2 Structure instance
instance Show(Bool) = {
    function show(x:Bool) : List(Nat) = {
        {True}  -> Cons(Succ(Z), Nil),
        {False} -> Cons(Z, Nil)
    }
};

// =============================================================================
// SECTION 13: ALGEBRA / TRAIT (single-type structures)
// =============================================================================

// 13.1 Algebra with laws
// NOTE: can't use === as function name — it's a reserved operator for propositional equality
algebra Eq2(a:Type) = {
    function eq2(x:a, y:a) : Bool,
    function neq2(x:a, y:a) : Bool = not(eq2(x, y)),
    law reflexivity(x:a) = eq2(x, x) === True,
    law symmetry(x:a, y:a) = eq2(x, y) === eq2(y, x)
};

// 13.2 Trait (alias for algebra)
trait Semigroup(a:Type) = {
    function combine(x:a, y:a) : a,
    law associativity(x:a, y:a, z:a) =
        combine(x, combine(y, z)) === combine(combine(x, y), z)
};

// 13.3 Semigroup instance
instance Semigroup(Nat) = {
    function combine(x:Nat, y:Nat) : Nat = plus(x, y)
};

// =============================================================================
// SECTION 14: MORPHISM / BRIDGE (multi-type structures)
// =============================================================================

// 14.1 Morphism (two type params)
morphism Convertible(a:Type, b:Type) = {
    function convert(x:a) : b
};

// 14.2 Bridge (alias for morphism)
bridge Printable(a:Type, b:Type) = {
    function render(x:a) : b
};

// 14.3 Convertible instances
instance Convertible(Bool, Nat) = {
    function convert(x:Bool) : Nat = {
        {True}  -> Succ(Z),
        {False} -> Z
    }
};

// =============================================================================
// SECTION 15: EXTENDS (structure inheritance)
// =============================================================================

// 15.1 Single extends
algebra Monoid(a:Type) extends Semigroup(a) = {
    function empty() : a,
    law leftIdentity(x:a)  = combine(empty(), x) === x,
    law rightIdentity(x:a) = combine(x, empty()) === x
};

// 15.2 Instance of child structure
instance Monoid(Nat) = {
    function combine(x:Nat, y:Nat) : Nat = plus(x, y),
    function empty() : Nat = Z
};

// 15.3 Deeper extends chain
algebra Group(a:Type) extends Monoid(a) = {
    function inverse(x:a) : a,
    law leftInverse(x:a)  = combine(inverse(x), x) === empty(),
    law rightInverse(x:a) = combine(x, inverse(x)) === empty()
};

// =============================================================================
// SECTION 16: REQUIRES (external constraints)
// =============================================================================

// 16.1 Structure with requires
morphism OrdConvertible(a:Type, b:Type) requires Eq2(a), Eq2(b) = {
    function ordConvert(x:a) : b
};

// 16.2 Instance with requires
instance Convertible(Nat, Bool) requires Eq2(Nat) = {
    function convert(x:Nat) : Bool = eq(x, Z)
};

// =============================================================================
// SECTION 17: LAW DECLARATIONS
// =============================================================================

// Laws are tested inline in structures above (sections 13, 15).
// Here we test specific law forms:

// 17.1 Algebra with implication law (==>)
algebra Ord(a:Type) extends Eq2(a) = {
    function compare(x:a, y:a) : Nat,
    function (<)(x:a, y:a) : Bool,
    function (>)(x:a, y:a) : Bool,
    law transitivity(x:a, y:a, z:a) =
        (eq2(x, y) === True) ==> (eq2(y, z) === True) ==> (eq2(x, z) === True)
};

// =============================================================================
// SECTION 18: ARROW TYPES IN TYPE ANNOTATIONS
// =============================================================================

// 18.1 Simple arrow type parameter
function apply(f:Nat -> Bool, x:Nat) : Bool = f(x);

// 18.2 Multi-arrow (right-associative)
function compose(f:b -> c, g:a -> b, x:a) : c = f(g(x));

// 18.3 Parenthesized arrow in parameter
function applyHO(f:(Nat -> Nat) -> Nat, g:Nat -> Nat) : Nat = f(g);

// 18.4 Arrow in return type
function mkAdder(n:Nat) : Nat -> Nat = plus(n);

// =============================================================================
// SECTION 19: TYPE APPLICATION IN TYPE POSITIONS
// =============================================================================

// 19.1 Concrete type application
function fromMaybe(def:Nat, x:Maybe(Nat)) : Nat = {
    {def, Nothing} -> def,
    {def, Just(v)} -> v
};

// 19.2 Nested type application
function pairOfMaybe(x:Pair(Maybe(Nat), Maybe(Bool))) = x;

// 19.3 Arrow with type application
function mapMaybe(f:a -> b, x:Maybe(a)) : Maybe(b) = {
    {f, Nothing} -> Nothing,
    {f, Just(v)} -> Just(f(v))
};

// =============================================================================
// SECTION 20: UNIVERSE HIERARCHY
// =============================================================================

// 20.1 Type (U 0)
function typeId(t:Type) : Type = t;

// 20.2 Type1 (U 1) — kind of type constructors
function kindId(k:Type1) : Type1 = k;

// 20.3 Type2 (U 2)
function superKindId(k:Type2) : Type2 = k;

// =============================================================================
// SECTION 21: ACTIONS (do-notation / sequencing)
// =============================================================================

// 21.1 Simple action
action main = {
    one = Succ(Z),
    three = Succ(Succ(one)),
    res = plus(three, one),
    print#(res)
};

// 21.2 Action with type annotation
action testAction : Nat = {
    x = Succ(Z),
    y = Succ(Succ(Z)),
    plus(x, y)
};

// =============================================================================
// SECTION 22: BUILT-IN / PRIMITIVE OPERATIONS
// =============================================================================

// 22.1 Primitive print
function testPrint(x) = print#(x);

// 22.2 Primitive concat
function testConcat(x, y) = concat#(x, y);

// =============================================================================
// SECTION 23: HIGHER-KINDED TYPES (future)
// =============================================================================

// 23.1 Functor algebra (requires f:Type1)
algebra Functor(f:Type1) = {
    function fmap(g:a -> b, x:f(a)) : f(b),
    law identity(x:f(a)) = fmap(id, x) === x
};

// 23.2 Functor instance for Maybe
instance Functor(Maybe) = {
    function fmap(g, x) = {
        {g, Nothing} -> Nothing,
        {g, Just(v)} -> Just(g(v))
    }
};

// 23.3 Applicative
algebra Applicative(f:Type1) extends Functor(f) = {
    function pure(x:a) : f(a),
    function ap(ff:f(a -> b), fa:f(a)) : f(b)
};

// 23.4 Monad
algebra Monad(m:Type1) extends Applicative(m) = {
    function bind(x:m(a), f:a -> m(b)) : m(b),
    law leftUnit(x:a, f:a -> m(b))  = bind(pure(x), f) === f(x),
    law rightUnit(x:m(a))           = bind(x, pure) === x
};

// =============================================================================
// SECTION 24: CATEGORIES AND ARROWS (future — standard library structures)
// =============================================================================

// 24.1 Category
algebra Category(arr:Type2) = {
    function catId() : arr(a, a),
    function catCompose(f:arr(b, c), g:arr(a, b)) : arr(a, c),
    law leftId(f:arr(a, b))  = catCompose(catId(), f) === f,
    law rightId(f:arr(a, b)) = catCompose(f, catId()) === f,
    law assoc(f:arr(c, d), g:arr(b, c), h:arr(a, b)) =
        catCompose(f, catCompose(g, h)) === catCompose(catCompose(f, g), h)
};

// 24.2 Arrow extends Category
algebra Arrow(arr:Type2) extends Category(arr) = {
    function arr(f:a -> b) : arr(a, b),
    function first(f:arr(a, b)) : arr({a, c}, {b, c})
};

// =============================================================================
// SECTION 25: NATURAL TRANSFORMATIONS (future keyword)
// =============================================================================

// Natural transformations would use a `natural` keyword — not yet a reserved word.
// Placeholder as a plain function:
function safeHead(xs:List(a)) : Maybe(a) = {
    {Nil}        -> Nothing,
    {Cons(h, t)} -> Just(h)
};

// =============================================================================
// SECTION 26: COMPLEX / COMBINED FEATURES
// =============================================================================

// 26.1 Let + if + pattern matching
function abs(x:Nat, y:Nat) : Nat =
    let diff = x - y in
    if eq(diff, Z) then y - x else diff;

// 26.2 Nested function application
function quadruple(x:Nat) : Nat = double(double(x));

// 26.3 Multiple structure instances for same type
algebra Eq(a:Type) = {
    function (==)(x, y:a) : Bool = not (x != y),
    function (!=)(x, y:a) : Bool = not (x == y),
    law reflexivity(x:a) = (x == x) === True,
    law symmetry(x:a, y:a) = (x == y) === (y == x)
};

instance Eq(Nat) = {
    function (==)(x:Nat, y:Nat) : Bool = eq(x, y)
};

instance Eq(Bool) = {
    function (==)(x:Bool, y:Bool) : Bool = {
        {True, True}   -> True,
        {False, False} -> True,
        {True, False}  -> False,
        {False, True}  -> False
    }
};

// 26.4 Polymorphic function with arrow types and application
function flip(f:a -> b -> c, x:b, y:a) : c = f(y, x);

// 26.5 Record construction and field access in function
function origin() : Point = Point(Z, Z);
function getX(p:Point) : Nat = p;

// 26.6 Parameterized record construction
function mkPair(x:a, y:b) : Pair(a, b) = Pair(x, y);

// =============================================================================
// SECTION 27: EDGE CASES
// =============================================================================

// 27.1 Parenthesized expression as function
function testParenApp(f, x) = (f)(x);

// 27.2 Empty-arg function
function unit() = {};

// 27.3 Deeply nested constructor patterns
function isTwo(n:Nat) : Bool = {
    {Succ(Succ(Z))} -> True,
    {n}             -> False
};

// 27.4 String with escape sequences
function escaped() = "hello\nworld\"test\\end";

// 27.5 Operator in parentheses as argument
function applyOp(x:Nat, y:Nat) : Nat = (+)(x, y);

// =============================================================================
// END OF SURFACE LANGUAGE TEST
// =============================================================================

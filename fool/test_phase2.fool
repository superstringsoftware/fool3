// Phase 2 test file

// Test records
record Point = { x:Nat, y:Nat };
record Pair(a:Type, b:Type) = { fst:a, snd:b };

// Test record spread
record Point3D = { ..Point, z:Nat };

// Test if/then/else in a function
function testIf(b:Bool) : Nat = if b then Succ(Z) else Z;

// Test let/in in a function
function testLet(n:Nat) : Nat = let one = Succ(Z) in plus(n, one);

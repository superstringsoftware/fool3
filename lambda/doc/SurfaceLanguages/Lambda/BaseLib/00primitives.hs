-- Primitive types and operations. To be mapped differently to different Realms.

Int     : Type
Float   : Type
Double  : Type
Byte    : Type
Pointer : Type

Array : Type = \ a:Type
allocateArray : Array a = \n:Int 

primPlusInt  : Int = \x,y:Int
primMulInt   : Int = \x,y:Int
primMinusInt : Int = \x,y:Int
primDivInt   : Int = \x,y:Int

primPlusFloat  : Float = \x,y:Float
primMulFloat   : Float = \x,y:Float
primMinusFloat : Float = \x,y:Float
primDivFloat   : Float = \x,y:Float

primPlusDouble  : Double = \x,y:Double
primMulDouble   : Double = \x,y:Double
primMinusDouble : Double = \x,y:Double
primDivDouble   : Double = \x,y:Double

-- List is primitive as we want to optimize how it's done in different realms.
List : Type = \a:Type . {
    ([]);
    (::) = :a :(List a)
}
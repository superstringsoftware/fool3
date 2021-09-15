-- Primitive types and operations. To be mapped differently to different Realms.

Int#     : PrimType
Float#   : PrimType
Double#  : PrimType
Byte#    : PrimType
Pointer# : PrimType

Array# : PrimType
allocateArray# (n:Int, a:PrimType)

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


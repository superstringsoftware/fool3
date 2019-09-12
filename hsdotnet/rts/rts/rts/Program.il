.assembly extern mscorlib
{
  .ver 4:0:0:0
  .publickeytoken = (B7 7A 5C 56 19 34 E0 89 ) // .z\V.4..
}
.assembly extern System
{
  .ver 4:0:0:0
  .publickeytoken = (B7 7A 5C 56 19 34 E0 89 ) // .z\V.4..
}
.assembly 'Program'
{
  .custom instance void class [mscorlib]System.Runtime.CompilerServices.RuntimeCompatibilityAttribute::'.ctor'() =  (
		01 00 01 00 54 02 16 57 72 61 70 4E 6F 6E 45 78   // ....T..WrapNonEx
		63 65 70 74 69 6F 6E 54 68 72 6F 77 73 01       ) // ceptionThrows.

  .hash algorithm 0x00008004
  .ver  0:0:0:0
}
.module Program.exe // GUID = {C24887B2-5E4E-48E7-B879-8F375E22410A}


.namespace rts
{
  .class public auto ansi beforefieldinit ValueBase
  	extends [mscorlib]System.Object
  {
    .field  public  int32 __CONSTAG__

    // method line 1
    .method public hidebysig specialname rtspecialname 
           instance default void '.ctor' (int32 ct)  cil managed 
    {
        // Method begins at RVA 0x2050
	// Code size 14 (0xe)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void object::'.ctor'()
	IL_0006:  ldarg.0 
	IL_0007:  ldarg.1 
	IL_0008:  stfld int32 rts.ValueBase::__CONSTAG__
	IL_000d:  ret 
    } // end of method ValueBase::.ctor

    // method line 2
    .method public hidebysig specialname rtspecialname 
           instance default void '.ctor' ()  cil managed 
    {
        // Method begins at RVA 0x205f
	// Code size 14 (0xe)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void object::'.ctor'()
	IL_0006:  ldarg.0 
	IL_0007:  ldc.i4.1 
	IL_0008:  stfld int32 rts.ValueBase::__CONSTAG__
	IL_000d:  ret 
    } // end of method ValueBase::.ctor

  } // end of class rts.ValueBase
}

.namespace rts
{
  .class public auto ansi beforefieldinit List`1<A>
  	extends rts.ValueBase
  {

    // method line 3
    .method public hidebysig specialname rtspecialname 
           instance default void '.ctor' ()  cil managed 
    {
        // Method begins at RVA 0x206e
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class rts.ValueBase::'.ctor'()
	IL_0006:  ret 
    } // end of method List`1::.ctor

  } // end of class rts.List`1
}

.namespace rts
{
  .class public auto ansi beforefieldinit Nothing`1<A>
  	extends class rts.List`1<!0>
  {

    // method line 4
    .method public hidebysig specialname rtspecialname 
           instance default void '.ctor' ()  cil managed 
    {
        // Method begins at RVA 0x2076
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class rts.List`1<!A>::'.ctor'()
	IL_0006:  ret 
    } // end of method Nothing`1::.ctor

  } // end of class rts.Nothing`1
}

.namespace rts
{
  .class public auto ansi beforefieldinit Just`1<A>
  	extends class rts.List`1<!0>
  {
    .field  public  !0 _F0_
    .field  public  class rts.List`1<!0> _F1_

    // method line 5
    .method public hidebysig specialname rtspecialname 
           instance default void '.ctor' (!A x, class rts.List`1<!A> xs)  cil managed 
    {
        // Method begins at RVA 0x207e
	// Code size 28 (0x1c)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void class rts.List`1<!A>::'.ctor'()
	IL_0006:  ldarg.0 
	IL_0007:  ldc.i4.2 
	IL_0008:  stfld int32 rts.ValueBase::__CONSTAG__
	IL_000d:  ldarg.0 
	IL_000e:  ldarg.1 
	IL_000f:  stfld !0 class rts.Just`1<!0>::_F0_
	IL_0014:  ldarg.0 
	IL_0015:  ldarg.2 
	IL_0016:  stfld class rts.List`1<!0> class rts.Just`1<!0>::_F1_
	IL_001b:  ret 
    } // end of method Just`1::.ctor

  } // end of class rts.Just`1
}

.namespace rts
{
  .class private auto ansi abstract sealed beforefieldinit Option1
  	extends [mscorlib]System.Object
  {

    // method line 6
    .method public static hidebysig 
           default class rts.List`1<!!B> map1<A,B> (class [mscorlib]System.Func`2<!!A, !!B> f, class rts.List`1<!!A> xs)  cil managed 
    {
        // Method begins at RVA 0x209c
	// Code size 49 (0x31)
	.maxstack 3
	.locals init (
		class rts.Just`1<!!A>	V_0)
	IL_0000:  ldarg.1 
	IL_0001:  isinst class rts.Just`1<!!0>
	IL_0006:  stloc.0 
	IL_0007:  ldloc.0 
	IL_0008:  brtrue IL_0013

	IL_000d:  newobj instance void class rts.Nothing`1<!!B>::'.ctor'()
	IL_0012:  ret 
	IL_0013:  ldarg.0 
	IL_0014:  ldloc.0 
	IL_0015:  ldfld !0 class rts.Just`1<!!0>::_F0_
	IL_001a:  callvirt instance !1 class [mscorlib]System.Func`2<!!A, !!B>::Invoke(!0)
	IL_001f:  ldarg.0 
	IL_0020:  ldloc.0 
	IL_0021:  ldfld class rts.List`1<!0> class rts.Just`1<!!0>::_F1_
	IL_0026:  call class rts.List`1<!!1> class rts.Option1::map1<!!0, !!1> (class [mscorlib]System.Func`2<!!0,!!1>, class rts.List`1<!!0>)
	IL_002b:  newobj instance void class rts.Just`1<!!B>::'.ctor'(!0, class rts.List`1<!0>)
	IL_0030:  ret 
    } // end of method Option1::map1

    // method line 7
    .method public static hidebysig 
           default class rts.List`1<!!B> map2<A,B> (class [mscorlib]System.Func`2<!!A, !!B> f, class rts.List`1<!!A> xs)  cil managed 
    {
        // Method begins at RVA 0x20dc
	// Code size 62 (0x3e)
	.maxstack 3
	.locals init (
		int32	V_0,
		class rts.Just`1<!!A>	V_1)
	IL_0000:  ldarg.1 
	IL_0001:  ldfld int32 rts.ValueBase::__CONSTAG__
	IL_0006:  stloc.0 
	IL_0007:  ldloc.0 
	IL_0008:  ldc.i4.1 
	IL_0009:  beq IL_0013

	IL_000e:  br IL_0019

	IL_0013:  newobj instance void class rts.Nothing`1<!!B>::'.ctor'()
	IL_0018:  ret 
	IL_0019:  ldarg.1 
	IL_001a:  isinst class rts.Just`1<!!0>
	IL_001f:  stloc.1 
	IL_0020:  ldarg.0 
	IL_0021:  ldloc.1 
	IL_0022:  ldfld !0 class rts.Just`1<!!0>::_F0_
	IL_0027:  callvirt instance !1 class [mscorlib]System.Func`2<!!A, !!B>::Invoke(!0)
	IL_002c:  ldarg.0 
	IL_002d:  ldloc.1 
	IL_002e:  ldfld class rts.List`1<!0> class rts.Just`1<!!0>::_F1_
	IL_0033:  call class rts.List`1<!!1> class rts.Option1::map1<!!0, !!1> (class [mscorlib]System.Func`2<!!0,!!1>, class rts.List`1<!!0>)
	IL_0038:  newobj instance void class rts.Just`1<!!B>::'.ctor'(!0, class rts.List`1<!0>)
	IL_003d:  ret 
    } // end of method Option1::map2

    // method line 8
    .method public static hidebysig 
           default class rts.List`1<int32> generate (int32 n)  cil managed 
    {
        // Method begins at RVA 0x2128
	// Code size 38 (0x26)
	.maxstack 3
	.locals init (
		class [mscorlib]System.Random	V_0)
	IL_0000:  newobj instance void class [mscorlib]System.Random::'.ctor'()
	IL_0005:  stloc.0 
	IL_0006:  ldarg.0 
	IL_0007:  brtrue IL_0012

	IL_000c:  newobj instance void class rts.Nothing`1<int32>::'.ctor'()
	IL_0011:  ret 
	IL_0012:  ldloc.0 
	IL_0013:  callvirt instance int32 class [mscorlib]System.Random::Next()
	IL_0018:  ldarg.0 
	IL_0019:  ldc.i4.1 
	IL_001a:  sub 
	IL_001b:  call class rts.List`1<int32> class rts.Option1::generate(int32)
	IL_0020:  newobj instance void class rts.Just`1<int32>::'.ctor'(!0, class rts.List`1<!0>)
	IL_0025:  ret 
    } // end of method Option1::generate

  } // end of class rts.Option1
}

.namespace rts
{
  .class private auto ansi beforefieldinit Program
  	extends [mscorlib]System.Object
  {
    .field  private static  class [mscorlib]System.Func`2<int32,int32> '<>f__mg$cache0'
    .custom instance void class [mscorlib]System.Runtime.CompilerServices.CompilerGeneratedAttribute::'.ctor'() =  (01 00 00 00 ) // ....

    .field  private static  class [mscorlib]System.Func`2<int32,int32> '<>f__mg$cache1'
    .custom instance void class [mscorlib]System.Runtime.CompilerServices.CompilerGeneratedAttribute::'.ctor'() =  (01 00 00 00 ) // ....


    // method line 9
    .method public hidebysig specialname rtspecialname 
           instance default void '.ctor' ()  cil managed 
    {
        // Method begins at RVA 0x215a
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void object::'.ctor'()
	IL_0006:  ret 
    } // end of method Program::.ctor

    // method line 10
    .method private static hidebysig 
           default int32 f (int32 x)  cil managed 
    {
        // Method begins at RVA 0x2162
	// Code size 5 (0x5)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  ldc.i4.s 0x0a
	IL_0003:  add 
	IL_0004:  ret 
    } // end of method Program::f

    // method line 11
    .method private static hidebysig 
           default void Main (string[] args)  cil managed 
    {
        // Method begins at RVA 0x2168
	.entrypoint
	// Code size 235 (0xeb)
	.maxstack 2
	.locals init (
		class [System]System.Diagnostics.Stopwatch	V_0,
		class rts.List`1<int32>	V_1,
		int64	V_2,
		class rts.List`1<int32>	V_3,
		int64	V_4,
		class rts.List`1<int32>	V_5,
		int64	V_6)
	IL_0000:  ldstr "Hello World!"
	IL_0005:  call void class [mscorlib]System.Console::WriteLine(string)
	IL_000a:  call class [System]System.Diagnostics.Stopwatch class [System]System.Diagnostics.Stopwatch::StartNew()
	IL_000f:  stloc.0 
	IL_0010:  ldc.i4.s 0x64
	IL_0012:  call class rts.List`1<int32> class rts.Option1::generate(int32)
	IL_0017:  stloc.1 
	IL_0018:  ldloc.0 
	IL_0019:  callvirt instance void class [System]System.Diagnostics.Stopwatch::Stop()
	IL_001e:  ldloc.0 
	IL_001f:  callvirt instance int64 class [System]System.Diagnostics.Stopwatch::get_ElapsedMilliseconds()
	IL_0024:  ldc.i4 1000
	IL_0029:  conv.i8 
	IL_002a:  div 
	IL_002b:  stloc.2 
	IL_002c:  call class [System]System.Diagnostics.Stopwatch class [System]System.Diagnostics.Stopwatch::StartNew()
	IL_0031:  stloc.0 
	IL_0032:  ldsfld class [mscorlib]System.Func`2<int32,int32> rts.Program::'<>f__mg$cache0'
	IL_0037:  brtrue.s IL_004a

	IL_0039:  ldnull 
	IL_003a:  ldftn int32 class rts.Program::f(int32)
	IL_0040:  newobj instance void class [mscorlib]System.Func`2<int32, int32>::'.ctor'(object, native int)
	IL_0045:  stsfld class [mscorlib]System.Func`2<int32,int32> rts.Program::'<>f__mg$cache0'
	IL_004a:  ldsfld class [mscorlib]System.Func`2<int32,int32> rts.Program::'<>f__mg$cache0'
	IL_004f:  ldloc.1 
	IL_0050:  call class rts.List`1<!!1> class rts.Option1::map1<int32, int32> (class [mscorlib]System.Func`2<!!0,!!1>, class rts.List`1<!!0>)
	IL_0055:  stloc.3 
	IL_0056:  ldloc.0 
	IL_0057:  callvirt instance int64 class [System]System.Diagnostics.Stopwatch::get_ElapsedMilliseconds()
	IL_005c:  ldc.i4 1000
	IL_0061:  conv.i8 
	IL_0062:  div 
	IL_0063:  stloc.s 4
	IL_0065:  call class [System]System.Diagnostics.Stopwatch class [System]System.Diagnostics.Stopwatch::StartNew()
	IL_006a:  stloc.0 
	IL_006b:  ldsfld class [mscorlib]System.Func`2<int32,int32> rts.Program::'<>f__mg$cache1'
	IL_0070:  brtrue.s IL_0083

	IL_0072:  ldnull 
	IL_0073:  ldftn int32 class rts.Program::f(int32)
	IL_0079:  newobj instance void class [mscorlib]System.Func`2<int32, int32>::'.ctor'(object, native int)
	IL_007e:  stsfld class [mscorlib]System.Func`2<int32,int32> rts.Program::'<>f__mg$cache1'
	IL_0083:  ldsfld class [mscorlib]System.Func`2<int32,int32> rts.Program::'<>f__mg$cache1'
	IL_0088:  ldloc.1 
	IL_0089:  call class rts.List`1<!!1> class rts.Option1::map2<int32, int32> (class [mscorlib]System.Func`2<!!0,!!1>, class rts.List`1<!!0>)
	IL_008e:  stloc.s 5
	IL_0090:  ldloc.0 
	IL_0091:  callvirt instance int64 class [System]System.Diagnostics.Stopwatch::get_ElapsedMilliseconds()
	IL_0096:  ldc.i4 1000
	IL_009b:  conv.i8 
	IL_009c:  div 
	IL_009d:  stloc.s 6
	IL_009f:  ldstr "Test results:"
	IL_00a4:  call void class [mscorlib]System.Console::WriteLine(string)
	IL_00a9:  ldstr "Generation,    s: "
	IL_00ae:  ldloc.2 
	IL_00af:  box [mscorlib]System.Int64
	IL_00b4:  call string string::Concat(object, object)
	IL_00b9:  call void class [mscorlib]System.Console::WriteLine(string)
	IL_00be:  ldstr "Via classes,   s: "
	IL_00c3:  ldloc.s 4
	IL_00c5:  box [mscorlib]System.Int64
	IL_00ca:  call string string::Concat(object, object)
	IL_00cf:  call void class [mscorlib]System.Console::WriteLine(string)
	IL_00d4:  ldstr "Via cons tags, s: "
	IL_00d9:  ldloc.s 6
	IL_00db:  box [mscorlib]System.Int64
	IL_00e0:  call string string::Concat(object, object)
	IL_00e5:  call void class [mscorlib]System.Console::WriteLine(string)
	IL_00ea:  ret 
    } // end of method Program::Main

  } // end of class rts.Program
}


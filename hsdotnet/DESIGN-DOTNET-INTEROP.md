# Design Document: .NET Interop for hsdotnet

## 1. Problem Statement

hsdotnet compiles Haskell to C# via GHC's STG intermediate representation. Currently, the compiled code lives entirely in the STG closure world (CLOSURE, FUN, THUNK, CON, PAP). To be useful, programs must be able to interact with .NET libraries — UI frameworks (WinForms, WPF, MAUI, Avalonia), data access (ADO.NET, EF Core), networking (HttpClient), and the entire BCL.

.NET objects are mutable, side-effectful, and object-oriented. These properties are fundamentally at odds with Haskell's pure, lazy, functional model. We need a principled bridge between the two worlds that:

1. Feels natural to Haskell programmers (monadic, type-safe, composable)
2. Allows full use of pure Haskell logic alongside .NET calls
3. Requires minimal boilerplate from the programmer
4. Is implementable incrementally on top of the existing compiler

## 2. Design Decisions

### 2.1 Monad, Not Arrow

.NET interop is fundamentally about sequencing imperative operations: create object, set property, attach event handler, call method. This maps directly to monadic `do`-notation.

Arrows would provide static dataflow analysis (useful for FRP-style reactive UI), but impose heavy syntactic overhead (`proc`, `arr`, `>>>`) and conceptual complexity. An FRP layer can be built *on top of* the monadic base later if desired. The base interop layer is monadic.

### 2.2 Distinct `DotNet` Monad (Newtype Over `IO`)

```haskell
newtype DotNet a = DotNet (IO a)
  deriving (Functor, Applicative, Monad)
```

Rather than putting everything in `IO`, a distinct `DotNet` monad:
- Gives the compiler a clear signal that a block contains .NET interop
- Allows future restriction of what operations are available in callbacks
- Opens the door to a `runDotNetPure` escape hatch for provably pure .NET operations (e.g., `System.Math`) using ST-style rank-2 types
- Is trivially liftable: `liftIO :: IO a -> DotNet a` and `runDotNet :: DotNet a -> IO a`

### 2.3 Primops, Not a Separate Compilation Mode

DotNet combinators (`new`, `.#`, `.=`, `.@`, `invoke`) compile through the **normal STG pipeline** as primops, not as a special "direct C#" compilation mode.

Rationale:
- No "two worlds" problem — pure Haskell and DotNet code compose seamlessly
- Higher-order functions work naturally (`mapM_`, `forM`, `when`, `filterM` over .NET operations)
- Laziness is preserved — a thunk can contain a .NET call that triggers on force
- The compiler needs only new primop entries, not a new codegen path
- Performance is fine — CLOSURE dispatch is nanoseconds; .NET calls are microseconds+

### 2.4 Typed References as CLOSUREs

A `Ref t` is a CLOSURE wrapping a .NET object. This is the single most important design decision — it makes .NET objects first-class Haskell values.

```haskell
newtype Ref (t :: *) = Ref# Addr#
```

Because `Ref t` is a CLOSURE, it works everywhere a Haskell value works:
- Store in lists: `[Ref Button]`
- Wrap in Maybe: `Maybe (Ref Form)`
- Pass to higher-order functions: `mapM_ close forms`
- Return from pure functions: `selectButton :: [Ref Button] -> Int -> Ref Button`
- Pattern match: `case mref of Nothing -> ...; Just btn -> ...`

### 2.5 String-Based Member Access (V1), Optional Type Safety (V2)

V1 uses literal strings for .NET member names. The strings are consumed at compile time — they become C# identifiers, not runtime values. Typos are caught by the C# compiler, not the Haskell compiler.

V2 (optional, later) adds type-level safety via type classes and a code generator that scans .NET assemblies.

### 2.6 Explicit Marshalling at Boundaries

Haskell values (CLOSURE trees) and .NET values (System.Object) are different runtime representations. Conversion is explicit at every boundary via `toNet`/`fromNet` functions. No hidden marshalling.

## 3. Programmer-Facing API

### 3.1 Module Structure

```
DotNet                     -- Core monad, Ref type, combinators
DotNet.Marshal             -- toNet, fromNet, marshalling utilities
DotNet.WinForms            -- Phantom types for System.Windows.Forms
DotNet.Avalonia            -- Phantom types for Avalonia UI
DotNet.Collections         -- Phantom types for System.Collections.Generic
...                        -- One module per .NET namespace as needed
```

### 3.2 Core Types

```haskell
module DotNet where

-- The interop monad
newtype DotNet a = DotNet (IO a)
  deriving (Functor, Applicative, Monad)

runDotNet :: DotNet a -> IO a
liftIO    :: IO a -> DotNet a

-- Typed reference to a .NET object (phantom type parameter)
newtype Ref (t :: *) = Ref# Addr#

-- Null reference check
isNull  :: Ref t -> Bool
nullRef :: Ref t
```

### 3.3 Core Combinators

```haskell
-- Object construction
new :: forall t. DotNet (Ref t)

-- Property access
(.#)  :: Ref t -> String -> DotNet (Ref a)        -- get property (returns Ref)
(.#%) :: Ref t -> String -> DotNet a               -- get property (returns marshalled Haskell value)
(.=)  :: Ref t -> String -> Ref a -> DotNet ()     -- set property (from Ref)
(.=%) :: Ref t -> String -> a -> DotNet ()         -- set property (from Haskell value, auto-marshal)

-- Method invocation
call  :: Ref t -> String -> [Ref a] -> DotNet (Ref b)     -- instance method
call_ :: Ref t -> String -> [Ref a] -> DotNet ()           -- instance method, void return

-- Static method invocation
static :: forall t. String -> [Ref a] -> DotNet (Ref b)   -- static method
static_ :: forall t. String -> [Ref a] -> DotNet ()        -- static method, void return

-- Event subscription
on    :: Ref t -> String -> (Ref evt -> DotNet ()) -> DotNet ()
onRaw :: Ref t -> String -> (Ref sender -> Ref evt -> DotNet ()) -> DotNet ()

-- Chained access (obj.Prop.Method(...))
(..>) :: DotNet (Ref t) -> String -> DotNet (Ref a)       -- chain property access
```

### 3.4 Marshalling

```haskell
module DotNet.Marshal where

-- Haskell → .NET
class ToNet a where
  toNet :: a -> DotNet (Ref t)

instance ToNet String where ...      -- [Char] → System.String
instance ToNet Int where ...         -- Int → System.Int32
instance ToNet Double where ...      -- Double → System.Double
instance ToNet Bool where ...        -- Bool → System.Boolean
instance ToNet [a] where ...         -- [a] → System.Collections.Generic.List<T>

-- .NET → Haskell
class FromNet a where
  fromNet :: Ref t -> DotNet a

instance FromNet String where ...    -- System.String → [Char]
instance FromNet Int where ...       -- System.Int32 → Int
instance FromNet Double where ...    -- System.Double → Double
instance FromNet Bool where ...      -- System.Boolean → Bool
```

### 3.5 Phantom Type Modules

These are trivial — one `data` declaration per .NET class. The programmer can write their own or use pre-made ones:

```haskell
module DotNet.WinForms where

data Button
data Form
data TextBox
data Label
data CheckBox
data ListBox
data Panel
data MenuStrip
data ToolStripMenuItem
data Application
data EventArgs
data MouseEventArgs
data KeyEventArgs
data Control           -- base type
data MessageBox
data DialogResult
```

No methods, no properties, no pragmas. Just phantom types for `Ref` disambiguation.

### 3.6 Subtyping

.NET inheritance is modeled via a type class:

```haskell
class SubType (sub :: *) (super :: *)

instance SubType Button Control
instance SubType TextBox Control
instance SubType Label Control
instance SubType Form Control
instance SubType MouseEventArgs EventArgs
-- Transitivity: could be derived, or explicit chains

upcast :: SubType sub super => Ref sub -> Ref super
-- Zero-cost: same underlying pointer, only type changes
```

This enables polymorphic functions:

```haskell
addControl :: SubType c Control => Ref Form -> Ref c -> DotNet ()
addControl form ctrl = call_ form "Controls.Add" [upcast ctrl]
```

## 4. Usage Examples

### 4.1 Simple WinForms Application

```haskell
import DotNet
import DotNet.Marshal
import DotNet.WinForms

main :: IO ()
main = runDotNet $ do
  -- Create form
  form <- new @Form
  form .=% "Text" "My Haskell App"
  form .=% "Width" (400 :: Int)
  form .=% "Height" (300 :: Int)

  -- Create button
  btn <- new @Button
  btn .=% "Text" "Click Me"
  btn .=% "Left" (50 :: Int)
  btn .=% "Top" (50 :: Int)

  -- Counter using IORef (pure Haskell, works naturally)
  counter <- liftIO $ newIORef (0 :: Int)

  -- Event handler: mix of pure Haskell and .NET
  on btn "Click" $ \_ -> do
    n <- liftIO $ modifyIORef' counter (+1) >> readIORef counter
    let msg = "Clicked " ++ show n ++ " times"  -- pure Haskell string manipulation
    btn .=% "Text" msg                           -- marshal and set .NET property

  -- Add button to form
  call_ form "Controls.Add" [upcast btn]

  -- Run application
  static_ @Application "Run" [upcast form]
```

### 4.2 Pure Haskell Logic Driving .NET UI

```haskell
-- Pure Haskell function — no DotNet monad, no IO
layoutGrid :: Int -> Int -> [(Int, Int)]
layoutGrid cols spacing =
  [ (col * spacing, row * spacing)
  | i <- [0..]
  , let (row, col) = i `divMod` cols
  ]

-- Pure Haskell data type
data TodoItem = TodoItem { title :: String, done :: Bool }

-- Mix pure logic with .NET UI construction
buildTodoList :: [TodoItem] -> Ref Panel -> DotNet ()
buildTodoList items panel = do
  let positions = layoutGrid 1 30  -- pure Haskell computation
  forM_ (zip items positions) $ \(item, (x, y)) -> do
    cb <- new @CheckBox
    cb .=% "Text" (title item)         -- Haskell String → .NET
    cb .=% "Checked" (done item)       -- Haskell Bool → .NET
    cb .=% "Left" x
    cb .=% "Top" y
    call_ panel "Controls.Add" [upcast cb]

-- Higher-order Haskell over .NET refs
setAllEnabled :: Bool -> [Ref Button] -> DotNet ()
setAllEnabled enabled = mapM_ (\b -> b .=% "Enabled" enabled)

-- Maybe with .NET refs
findByName :: String -> [Ref Control] -> DotNet (Maybe (Ref Control))
findByName target ctrls = do
  results <- filterM (\c -> do
    name <- c .#% "Name" :: DotNet String
    pure (name == target)    -- pure Haskell comparison
    ) ctrls
  pure (listToMaybe results)
```

### 4.3 Non-UI: HttpClient

```haskell
import DotNet
import DotNet.Net.Http

fetchPage :: String -> DotNet String
fetchPage url = do
  client <- new @HttpClient
  netUrl <- toNet url
  resp <- call client "GetStringAsync" [netUrl]
  -- resp is a Ref Task<string> — need await-equivalent
  result <- call resp "Result" []   -- blocks (synchronous for now)
  fromNet result

main :: IO ()
main = runDotNet $ do
  html <- fetchPage "https://example.com"
  liftIO $ putStrLn (take 200 html)   -- pure Haskell: take first 200 chars
```

## 5. Runtime System (C# RTS Additions)

### 5.1 DOTNET_REF: The Bridge Type

```csharp
// rts/rts/rts/DotNetInterop.cs
namespace SuperstringSolutions.HSNet.STG
{
    /// <summary>
    /// A CLOSURE that wraps an arbitrary .NET object.
    /// This is the bridge between the STG world and the .NET world.
    /// Ref t in Haskell = DOTNET_REF in C#.
    /// </summary>
    public class DOTNET_REF : CLOSURE
    {
        public object Value;

        public DOTNET_REF(object val)
        {
            Value = val;
        }

        // Already evaluated — a .NET reference is a value, not a computation
        public override CLOSURE ENTER => this;
        public override CLOSURE EVAL() => this;

        public override string ToString()
        {
            return Value?.ToString() ?? "null";
        }
    }
}
```

### 5.2 DOTNET Primops: The Interop Engine

```csharp
// rts/rts/rts/DotNetOps.cs
using System;
using System.Reflection;

namespace SuperstringSolutions.HSNet.STG
{
    /// <summary>
    /// Primitive operations for .NET interop.
    /// These are called from compiled Haskell code via the primop mechanism.
    /// String arguments (class names, member names) are compile-time constants
    /// baked into the generated C# code.
    /// </summary>
    public static class DOTNET
    {
        // ---- Object Construction ----

        /// <summary>
        /// Construct a new .NET object.
        /// className: fully-qualified type name (e.g., "System.Windows.Forms.Button")
        /// </summary>
        public static CLOSURE New(string className)
        {
            var type = Type.GetType(className)
                ?? throw new Exception($"DOTNET.New: type not found: {className}");
            var obj = Activator.CreateInstance(type);
            return new DOTNET_REF(obj);
        }

        /// <summary>
        /// Construct with arguments.
        /// args: array of CLOSUREs to be marshalled and passed to constructor.
        /// </summary>
        public static CLOSURE NewWith(string className, CLOSURE[] args)
        {
            var type = Type.GetType(className)
                ?? throw new Exception($"DOTNET.NewWith: type not found: {className}");
            var netArgs = Marshal.FromHaskellArray(args);
            var obj = Activator.CreateInstance(type, netArgs);
            return new DOTNET_REF(obj);
        }

        // ---- Property Access ----

        /// <summary>
        /// Get a property value. Returns it wrapped in DOTNET_REF.
        /// Supports dotted paths: "Controls.Count" navigates Controls then Count.
        /// </summary>
        public static CLOSURE GetProp(CLOSURE target, string propName)
        {
            var obj = Unwrap(target);
            var val = NavigateAndGet(obj, propName);
            return Marshal.ToHaskell(val);
        }

        /// <summary>
        /// Set a property value.
        /// </summary>
        public static CLOSURE SetProp(CLOSURE target, string propName, CLOSURE value)
        {
            var obj = Unwrap(target);
            var netVal = Marshal.FromHaskell(value);
            var prop = obj.GetType().GetProperty(propName)
                ?? throw new Exception($"DOTNET.SetProp: property not found: {propName} on {obj.GetType().Name}");
            prop.SetValue(obj, Convert.ChangeType(netVal, prop.PropertyType));
            return Unit;
        }

        // ---- Method Invocation ----

        /// <summary>
        /// Call an instance method.
        /// </summary>
        public static CLOSURE Call(CLOSURE target, string methodName, CLOSURE[] args)
        {
            var obj = Unwrap(target);
            var netArgs = Marshal.FromHaskellArray(args);
            var result = obj.GetType().InvokeMember(
                methodName,
                BindingFlags.InvokeMethod | BindingFlags.Public | BindingFlags.Instance,
                null, obj, netArgs);
            return result == null ? Unit : Marshal.ToHaskell(result);
        }

        /// <summary>
        /// Call a static method.
        /// </summary>
        public static CLOSURE CallStatic(string className, string methodName, CLOSURE[] args)
        {
            var type = Type.GetType(className)
                ?? throw new Exception($"DOTNET.CallStatic: type not found: {className}");
            var netArgs = Marshal.FromHaskellArray(args);
            var result = type.InvokeMember(
                methodName,
                BindingFlags.InvokeMethod | BindingFlags.Public | BindingFlags.Static,
                null, null, netArgs);
            return result == null ? Unit : Marshal.ToHaskell(result);
        }

        // ---- Event Subscription ----

        /// <summary>
        /// Subscribe a Haskell closure (FUN) to a .NET event.
        /// The handler receives (Ref EventArgs) -> DotNet ().
        /// </summary>
        public static CLOSURE Subscribe(CLOSURE target, string eventName, CLOSURE handler)
        {
            var obj = Unwrap(target);
            var evt = obj.GetType().GetEvent(eventName)
                ?? throw new Exception($"DOTNET.Subscribe: event not found: {eventName} on {obj.GetType().Name}");

            // Create delegate matching the event's signature
            // Most .NET events use EventHandler or EventHandler<T>
            EventHandler del = (sender, args) =>
            {
                var haskellHandler = STG.EVAL(handler);
                if (haskellHandler is FUN f)
                    f.Call(new CLOSURE[] { new DOTNET_REF(args) });
                else if (haskellHandler is PAP p)
                    p.Call(new CLOSURE[] { new DOTNET_REF(args) });
            };

            evt.AddEventHandler(obj, del);
            return Unit;
        }

        // ---- Helpers ----

        private static object Unwrap(CLOSURE c)
        {
            var evaled = STG.EVAL(c);
            if (evaled is DOTNET_REF r)
                return r.Value;
            throw new Exception($"DOTNET: expected DOTNET_REF, got {evaled.GetType().Name}");
        }

        /// <summary>
        /// Navigate dotted property paths: "Controls.Count" → obj.Controls.Count
        /// </summary>
        private static object NavigateAndGet(object obj, string path)
        {
            var parts = path.Split('.');
            var current = obj;
            foreach (var part in parts)
            {
                var prop = current.GetType().GetProperty(part);
                if (prop != null)
                {
                    current = prop.GetValue(current);
                    continue;
                }
                var field = current.GetType().GetField(part);
                if (field != null)
                {
                    current = field.GetValue(current);
                    continue;
                }
                throw new Exception($"DOTNET: member not found: {part} on {current.GetType().Name}");
            }
            return current;
        }

        /// <summary>
        /// Unit value — represents () / void return.
        /// </summary>
        public static readonly CLOSURE Unit = new CON(0, CLOSURE.EMPTY);
    }
}
```

### 5.3 Marshal: Type Conversion Layer

```csharp
// rts/rts/rts/Marshal.cs
using System;
using System.Collections.Generic;

namespace SuperstringSolutions.HSNet.STG
{
    /// <summary>
    /// Bidirectional marshalling between Haskell CLOSUREs and .NET objects.
    /// This is the boundary layer — every value crossing between worlds goes through here.
    /// </summary>
    public static class Marshal
    {
        // ---- .NET → Haskell ----

        /// <summary>
        /// Wrap a .NET value in the appropriate CLOSURE type.
        /// Primitives become CONPRIM, objects become DOTNET_REF.
        /// </summary>
        public static CLOSURE ToHaskell(object val)
        {
            if (val == null)
                return DOTNET.Unit;

            return val switch
            {
                int i       => new CONPRIM<int>(i),
                long l      => new CONPRIM<long>(l),
                float f     => new CONPRIM<float>(f),
                double d    => new CONPRIM<double>(d),
                bool b      => new CONPRIM<int>(b ? 1 : 0),   // GHC Bool = Int# tag
                char c      => new CONPRIM<char>(c),
                string s    => new DOTNET_REF(s),              // keep as .NET string
                CLOSURE c   => c,                              // already a CLOSURE
                _           => new DOTNET_REF(val),            // opaque .NET object
            };
        }

        // ---- Haskell → .NET ----

        /// <summary>
        /// Extract a .NET value from a CLOSURE.
        /// Forces evaluation (EVAL), then unwraps.
        /// </summary>
        public static object FromHaskell(CLOSURE c)
        {
            var evaled = STG.EVAL(c);

            return evaled switch
            {
                CONPRIM<int> i    => i.Val,
                CONPRIM<long> l   => l.Val,
                CONPRIM<float> f  => f.Val,
                CONPRIM<double> d => d.Val,
                CONPRIM<char> ch  => ch.Val,
                DOTNET_REF r      => r.Value,
                CON con           => UnpackIfString(con),  // might be [Char]
                _                 => evaled,               // pass CLOSURE itself
            };
        }

        /// <summary>
        /// Marshal an array of Haskell CLOSUREs to .NET objects.
        /// </summary>
        public static object[] FromHaskellArray(CLOSURE[] args)
        {
            var result = new object[args.Length];
            for (int i = 0; i < args.Length; i++)
                result[i] = FromHaskell(args[i]);
            return result;
        }

        // ---- String Conversion ----

        /// <summary>
        /// Convert a Haskell [Char] (linked list of CON nodes) to a .NET string.
        /// Haskell list: CON(tag=2, [charVal, tail]) for (:), CON(tag=1, []) for []
        /// </summary>
        public static string HaskellListToString(CLOSURE list)
        {
            var sb = new System.Text.StringBuilder();
            var current = STG.EVAL(list);
            while (current is CON con && con.__CONSTAG__ == 2) // (:) constructor
            {
                var charVal = STG.EVAL(con.Vals[0]);
                if (charVal is CONPRIM<char> ch)
                    sb.Append(ch.Val);
                else if (charVal is CONPRIM<int> i)
                    sb.Append((char)i.Val);
                current = STG.EVAL(con.Vals[1]);
            }
            return sb.ToString();
        }

        /// <summary>
        /// Convert a .NET string to a Haskell [Char] (linked list).
        /// Builds from the end: foldr (:) [] chars
        /// </summary>
        public static CLOSURE StringToHaskellList(string s)
        {
            CLOSURE result = new CON(1, CLOSURE.EMPTY);  // []
            for (int i = s.Length - 1; i >= 0; i--)
            {
                result = new CON(2, new CLOSURE[] {       // (:)
                    new CONPRIM<char>(s[i]),
                    result
                });
            }
            return result;
        }

        /// <summary>
        /// Check if a CON is a [Char] list and unpack it if so.
        /// </summary>
        private static object UnpackIfString(CON con)
        {
            // Heuristic: if it looks like a cons cell with a char head, treat as string
            if (con.__CONSTAG__ == 2 && con.Vals.Length == 2)
            {
                var head = STG.EVAL(con.Vals[0]);
                if (head is CONPRIM<char>)
                    return HaskellListToString(con);
            }
            return con; // not a string, return as-is
        }
    }
}
```

## 6. Compiler Changes

### 6.1 New IR Nodes

Add to `DotNetExpr` in `src/Compiler.hs`:

```haskell
data DotNetExpr =
    -- ... existing constructors ...
    -- .NET interop operations (recognized from DotNet module primop calls)
    | DOTNETNEW    String                           -- class name
    | DOTNETGET    DotNetExpr String                 -- target, property name
    | DOTNETSET    DotNetExpr String DotNetExpr      -- target, property name, value
    | DOTNETCALL   DotNetExpr String [DotNetExpr]    -- target, method name, args
    | DOTNETSTATIC String String [DotNetExpr]        -- class name, method name, args
    | DOTNETEVENT  DotNetExpr String DotNetExpr      -- target, event name, handler
```

### 6.2 STG Pattern Recognition

In `stgExpr2DotNetExpr`, recognize calls to known `DotNet` module functions:

```haskell
-- When we see a call to a known DotNet primop, convert to our IR
stgExpr2DotNetExpr (StgOpApp (StgPrimOp op) args _tp)
  | isPrimOpDotNet op = convertDotNetPrimOp op args
  | otherwise         = PRIMOP (showGhc op, Nothing) args

-- Alternatively, recognize by name at the StgApp level:
stgExpr2DotNetExpr (StgApp fn args)
  | isDotNetCombinator fn = convertDotNetCall fn args
  | null args             = VAR (var2IdName fn)
  | otherwise             = FUNCALL (var2IdName fn) args
```

The recognition can work by:
1. **Module-qualified name matching**: if the function comes from `DotNet` module, it's interop
2. **Special primop IDs**: register DotNet operations as custom primops
3. **Pragma annotation**: `{-# DOTNET #-}` on the DotNet module triggers special treatment

Option (1) is simplest for V1 and requires no GHC modifications.

### 6.3 C# Code Generation

Add to `CSharpGen.hs`:

```haskell
instance CSharpable DotNetExpr where
    -- ... existing patterns ...

    toCSharp (DOTNETNEW className) =
        pure $ "new DOTNET_REF(new " ++ className ++ "())"

    toCSharp (DOTNETGET target propName) = do
        t <- toCSharp target
        pure $ "DOTNET.GetProp(" ++ t ++ ", \"" ++ propName ++ "\")"

    toCSharp (DOTNETSET target propName val) = do
        t <- toCSharp target
        v <- toCSharp val
        pure $ "DOTNET.SetProp(" ++ t ++ ", \"" ++ propName ++ "\", " ++ v ++ ")"

    toCSharp (DOTNETCALL target methodName args) = do
        t <- toCSharp target
        as <- mapM toCSharp args
        let argArray = "new CLOSURE[] {" ++ intercalate ", " as ++ "}"
        pure $ "DOTNET.Call(" ++ t ++ ", \"" ++ methodName ++ "\", " ++ argArray ++ ")"

    toCSharp (DOTNETSTATIC className methodName args) = do
        as <- mapM toCSharp args
        let argArray = "new CLOSURE[] {" ++ intercalate ", " as ++ "}"
        pure $ "DOTNET.CallStatic(\"" ++ className ++ "\", \"" ++ methodName ++ "\", " ++ argArray ++ ")"

    toCSharp (DOTNETEVENT target eventName handler) = do
        t <- toCSharp target
        h <- toCSharp handler
        pure $ "DOTNET.Subscribe(" ++ t ++ ", \"" ++ eventName ++ "\", " ++ h ++ ")"
```

### 6.4 Optimization: Direct Dispatch (V2)

The reflection-based approach (Section 5.2) works but is slow due to `Type.GetType()`, `GetProperty()`, `InvokeMember()` at every call.

In V2, since the compiler knows the class and member names at compile time, it can emit **direct C# calls** instead of going through the DOTNET primops:

```haskell
-- V1 (reflection-based): always works, slower
toCSharp (DOTNETNEW "System.Windows.Forms.Button") =
    pure "DOTNET.New(\"System.Windows.Forms.Button\")"

-- V2 (direct dispatch): fast, requires knowing the type
toCSharp (DOTNETNEW "System.Windows.Forms.Button") =
    pure "new DOTNET_REF(new System.Windows.Forms.Button())"

-- V2 property set with known type:
toCSharp (DOTNETSET target "Text" val) =
    -- Instead of reflection, emit direct property access:
    t <- toCSharp target
    v <- toCSharp val
    pure $ "((System.Windows.Forms.Button)((DOTNET_REF)" ++ t ++ ").Value).Text = (string)Marshal.FromHaskell(" ++ v ++ ")"
```

This is a pure compiler optimization — the semantics are identical, just faster. Can be done incrementally for hot paths.

## 7. The DotNet Haskell Module (Baselib)

This lives in `baselib/` and is the Haskell source the programmer imports. The compiler recognizes calls to functions from this module.

### 7.1 File: `baselib/DotNet.hs`

```haskell
module DotNet (
    DotNet, runDotNet, liftIO,
    Ref,
    new, (.#), (.#%), (.=), (.=%),
    call, call_,
    static, static_,
    on,
    upcast,
    toNet, fromNet
) where

-- These types exist for the programmer's benefit.
-- The compiler recognizes them and generates appropriate code.

-- | The .NET interop monad.
newtype DotNet a = DotNet (IO a)

runDotNet :: DotNet a -> IO a
runDotNet (DotNet m) = m

liftIO :: IO a -> DotNet a
liftIO = DotNet

-- | Typed reference to a .NET object.
newtype Ref t = Ref Addr#

-- | Construct a new .NET object of type t.
-- The compiler resolves t to a .NET class name via the phantom type.
new :: forall t. DotNet (Ref t)
new = DotNet (dotnet_new# undefined)  -- primop, compiler replaces

-- ... etc. The actual implementations are primops;
-- the Haskell source serves as type signatures and documentation.
-- The compiler pattern-matches on calls to these functions
-- and emits the corresponding DOTNET IR nodes.
```

### 7.2 Phantom Type Modules

Example: `baselib/DotNet/WinForms.hs`

```haskell
module DotNet.WinForms where

-- Each data type maps to a .NET class via naming convention:
-- Module path DotNet.WinForms + type name Button
--   → "System.Windows.Forms.Button"
-- The compiler performs this mapping.

data Button
data Form
data TextBox
data Label
data CheckBox
data ComboBox
data ListBox
data Panel
data GroupBox
data TabControl
data TabPage
data MenuStrip
data ToolStripMenuItem
data StatusStrip
data ToolStrip
data DataGridView
data TreeView
data ListView
data PictureBox
data ProgressBar
data Timer
data Application
data MessageBox
data EventArgs
data MouseEventArgs
data KeyEventArgs
data PaintEventArgs
data FormClosingEventArgs
data Control
data DialogResult
data DockStyle
data AnchorStyles
data MessageBoxButtons
data MessageBoxIcon
data Keys
```

This is the ONLY file needed to access the entire WinForms API. One line per class. No method declarations, no property signatures, no pragmas.

The compiler maps phantom types to .NET class names by convention:
- Module `DotNet.WinForms` → namespace `System.Windows.Forms`
- Type `Button` → class `Button`
- Full: `System.Windows.Forms.Button`

Custom mappings for non-standard names can use a simple pragma:

```haskell
{-# DOTNET_NS "System.Windows.Forms" #-}  -- override namespace for this module
```

## 8. Implementation Phases

### Phase 1: Foundation (Minimum Viable Interop)

**Goal**: `new @Button`, `.=% "Text" "hello"`, `on "Click"` work end-to-end.

| Step | What | Where | Effort |
|------|------|-------|--------|
| 1.1 | Add `DOTNET_REF` class | `rts/rts/rts/DotNetInterop.cs` | Small |
| 1.2 | Add `DOTNET` static class (New, GetProp, SetProp, Call, Subscribe) | `rts/rts/rts/DotNetOps.cs` | Medium |
| 1.3 | Add `Marshal` class (ToHaskell, FromHaskell, string conversion) | `rts/rts/rts/Marshal.cs` | Medium |
| 1.4 | Add DOTNET* IR nodes to `DotNetExpr` | `src/Compiler.hs` | Small |
| 1.5 | Add CSharpable instances for DOTNET* nodes | `src/CSharpGen.hs` | Small |
| 1.6 | Recognition of DotNet module calls in STG | `src/Compiler.hs` | Medium |
| 1.7 | Write `baselib/DotNet.hs` core module | `baselib/DotNet.hs` | Small |
| 1.8 | Write `baselib/DotNet/WinForms.hs` phantom types | `baselib/DotNet/WinForms.hs` | Small |
| 1.9 | End-to-end test: compile & run a WinForms hello world | `Example.hs` | Medium |

**Dependencies**: Requires Phase 1 of the implementation roadmap (basic STG compilation working — CON field access, literals, PAP, THUNK eval). DotNet interop builds on a working base compiler.

### Phase 2: Usability

| Step | What |
|------|------|
| 2.1 | Direct dispatch optimization (skip reflection for known types) |
| 2.2 | Constructor arguments: `newWith @Font ["Arial", 12]` |
| 2.3 | Enum support: `.=% "Dock" DockFill` mapping to .NET enum values |
| 2.4 | Generic event signatures (beyond EventHandler) |
| 2.5 | Async/Task support: `await :: Ref (Task a) -> DotNet (Ref a)` |
| 2.6 | Array/List interop: Haskell lists ↔ .NET IEnumerable/List<T> |
| 2.7 | Auto-generate phantom type modules from .NET assembly metadata |

### Phase 3: Type Safety & Performance

| Step | What |
|------|------|
| 3.1 | Type-class based property/method checking (HasProp, HasMethod) |
| 3.2 | Assembly scanner tool: .NET DLL → Haskell type-class instances |
| 3.3 | Interface-based polymorphism (IDisposable, IEnumerable, etc.) |
| 3.4 | `using` combinator for IDisposable: `using (new @Font) $ \f -> ...` |
| 3.5 | Nullable reference tracking: `Ref t` vs `Ref (Nullable t)` |
| 3.6 | Direct IL generation bypassing C# for hot interop paths |

## 9. Open Questions

### 9.1 Reflection vs Direct Dispatch Tradeoff

V1 uses reflection (`GetProperty`, `InvokeMember`). This is:
- **Pro**: Works for any .NET class without compile-time knowledge
- **Con**: Slower (~100x), no compile-time member name checking

V2 direct dispatch requires the compiler to know the .NET type at compile time. This is possible when the phantom type is concrete (`Ref Button`) but not when it's polymorphic (`SubType c Control => Ref c`). Likely need both paths: direct for concrete types, reflection for polymorphic.

### 9.2 Threading Model

.NET UI frameworks require a single UI thread (STA). Haskell's RTS uses green threads. For V1, everything runs on one thread (no Haskell concurrency). Later: need a message-pump integration where `DotNet` actions are dispatched to the UI thread.

### 9.3 Memory Management

DOTNET_REF wraps a .NET object in a CLOSURE. The .NET GC manages the object's lifetime. As long as the DOTNET_REF is reachable from Haskell (via the STG heap), the .NET object stays alive. When the DOTNET_REF becomes garbage, the .NET object becomes collectible. This should work naturally — no special attention needed.

### 9.4 Exception Handling

.NET methods throw exceptions. These should be caught in the DOTNET primops and converted to Haskell exceptions (once the exception mechanism exists per Phase 3 of the implementation roadmap). For V1: .NET exceptions propagate uncaught and crash — acceptable for initial development.

### 9.5 Type Name Resolution

`Type.GetType("System.Windows.Forms.Button")` requires the assembly-qualified name for types not in `mscorlib`. Options:
- Require full assembly-qualified names (ugly)
- Scan loaded assemblies at startup
- Use `Assembly.LoadFrom()` + `assembly.GetType()` with assembly references in a config

### 9.6 Lazy Evaluation at Boundaries

When passing a Haskell value to .NET (e.g., `.=% "Text" someExpensiveComputation`), the marshalling layer calls `STG.EVAL()` which forces the thunk. This is correct — .NET expects evaluated values. But it means the `.=%` call bears the cost of any deferred computation. This is inherent and unavoidable, but worth documenting for programmers.

## 10. Summary

The design bridges Haskell's pure functional world with .NET's imperative OOP world via:

1. **`DotNet` monad** — sequences side-effectful .NET operations
2. **`Ref t` as CLOSURE** — .NET objects are first-class Haskell values
3. **String-based member access** — zero boilerplate, caught by C# compiler
4. **Primop-based implementation** — goes through normal STG pipeline, composes with all Haskell features
5. **Explicit marshalling** — `toNet`/`fromNet` at boundaries, no hidden cost
6. **Phantom type modules** — one line per .NET class, no method/property declarations
7. **Reflection-based V1, direct dispatch V2** — works immediately, optimizable later

The programmer writes pure Haskell plus `DotNet` monadic blocks. No pragmas, no FFI declarations, no code generation required. The full power of Haskell (algebraic data types, pattern matching, higher-order functions, laziness, type classes) is available alongside .NET interop.

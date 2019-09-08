RTS notes for .Net (CLR)
-------------------------------------

OPTIMIZATIONS
-----------------------

* For types with just one Unit () constructor (Maybe, List etc) - optimize Unit to null in c#, avoids extra inheritance


Very early ideas.
--------------------

CLR is inherently *stateful*, with pretty good types (classes) support. Basic typeclasses can be modelled as interfaces in a very straightforward way, where we run into problems is typeclasses that take other types as parameters ("type functions") - and from where the real power of Haskell starts.

Let's say we have our basic Functor and implementations for List and Maybe:

```haskell
class Functor f where
    fmap :: a -> b -> f a -> f b

instance Functor List where 
    fmap _ [] = []
    fmap g (x:xs) = (g x) : (fmap g xs)

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap g (Just x) = Just (g x)
```

Let's also say we want in our language to support our purely functional list as well as C# built in generic List<T> that already supports a bunch of interfaces, and we want them both to be **Functors**. Ignore mutability for now, as C# list supports mutability by default and it's a bigger issue, but focusing on syntaxis only for now. Also we need to take into account that any nullable type in C# is in essence Maybe - so should we implement Maybe using built in CLR machinery then?

So, how do we implement it in CLR?! In effect, we want to be able to write something like:

```C#
public interface Functor<F> {
    F<B> fmap<A,B> (Func<A,B> f, A x);
} 

// One way to do it - define a function inside the class itself, NOT very functional:
public class List<A> : ... Functor<List> {
    List<B> fmap(Func<A,B> f, A x) {
        ...
    }
}

public class Maybe<A> : ... Functor<Maybe> {
    Maybe<B> fmap(Func<A,B> f, A x) {
        if (x == null) return null;
        else return Just(f(x));
    }
}

// the other way to do it, define different implementations for Functor interface itself, more closely following haskell:
// can we implement it via constraints???
public class FunctorList<A> : List<A>, Functor<F> where F:List<A> {
    List<B> fmap(Func<A,B> f, A x) {
        ...
    }
}
```

Or something like it. The above is cumbersome and ugly, so maybe we can start with coming up with a nice C# extension to support Type Function (or typeclasses with parameters) and then take it from there??

Following Haskell approach more closely we can have a class Functor that defines fmaps_... for all classes that support it, so fmap_List, fmap_Maybe etc. Then have a dictionary that puts a correct function call while compiling. So, in pseudo C#:

```C#
public type interface Functor<F> where F : F<A> {
    static F<B> fmap(Func<A,B> f, F<A> x); // in C# - we can lose the last argument and make this function part of the class, so:
    F<B> fmap(Func<A,B> f);
}

// then
public class FunctorList<A> : List<A>, Functor<F> {
    List<B> fmap(Func<A,B> f) {
        //... map over all elements of this applying function f
    } 
}
```

Looking at the above, we can actually forget all these complicated interfaces and simply implement function with the same name in every class that we want to support it. Not really, as parameter <B> is nowhere to be seen so we'll need to have it defined explicitly in fmap, so it gets uglier and uglier, but it can be done, yes.

And yes, we don't need the interface as we NEVER pass a typeclass anywhere in Haskell (except as a constraint??? - but we can turn constraint into specific ask for a specific function in the typechecker).

So then yes, we don't need explicit syntaxis for type parameter typeclasses.

```C#
public class List<A> {
    private A _x;
    private List<A> _xs;

    public static List<A> Nil = null;

    public List(A x, List<A> xs) {
        _x = x;
        _xs = xs;
    }

    
}


```
-- another attempt at the surface language syntax
{x1 ... xn} - tuple, main thing

Maybe (a:*) = Nothing + Just (x:a).{x}
List (a:*) = [] + (:) (x:a, xs: List(a)).{x, xs}

-- constructors above explicitly return a tuple, but we can have more complex contstructor functions for dependent types, e.g.:
-- type where 1st pair element is always bigger than the second - complex constructor function
DepPair (Num a => a) = DepPair (x,y: a ). if (x>y) then {x, y} else {y, x}
-- is * misleading in Product types? What if it's aactual multiplication??? Maybe comma is still better, or space

-- for clarity, regular types can be rewritten as
Maybe (a:*) = Nothing + Just {:a}
List (a:*) = [] + (:) {:a * :List(a)}

-- Also, use round brackets for tuples? Since we want to be consistent in function calls as well?

length:Int ls:[a] = ls ? [] = [] | _:xs = 1 + length(xs)
map:[b] (f:a->b, ls:[a]) = ls ? [] = [] | x:xs = f(x):map(f, xs) 

-- every function has a *named* tuple as arguments, so that than we can do some crazy partial applications, e.g.:
f (n1:t1, n2:t2, n3:t3) = ...
g = f(n2 = val) -- partial application of f by setting n2 to a specific value

--typeclasses and type families use the same syntax, simply when we have a type function there - it's a family
class Functor (f:*->*) = 
    fmap:f(b) (g:a->b, x:f(a))

-- think through, need id type, an io monade where the actions are taking place etc
class Persistent(record:*) = 
    Entity (record:*):* -- data constructor
    create (r:record):Entity(record)
    update (er:Entity(record), ur: <record) : Entity(record) -- <record - means should have *some* fields from the record type
    delete (er:Entity(record)) : m ()

-- some records ideas
Person = Person {
    name, lname : String
  * age : Int
  * address : { -- nested record without explicit type. Can model js objects easily like that!
      street : String
    * house : Int
  }
}

-- then, we can define interesting things such as:
>Person - any type that is built by * Person with something else
<Person - any type that has *some* fields from Person

however, we may not need it - e.g., update operation above, instead of passing ur:<record we can
actually pass a function that converts record to record and build it on the fly. It is probably more functional
and cleaner way anyway?

E.g., compare:
update (eperson, {name = "Anton", age = 32}) - and then update handles change logic, to:
update (eperson, \r -> r {name = "Anton", age = 32}) - actually, it's pretty much the same, but we don't need the
subtyping stuff, but only a function record->record. Does seem better in our land.

Ok, if we are doing records with {} we need to do unnamed tuples like this as well, so:

Maybe (a:*) = Nothing + Just {:a}
List (a:*) = [] + (::) {:a * :List(a)}
-- which means we can easily have a named list:
List (a:*) = [] + (::) {fst:a * snd:List(a)}
etc.
Either (a,b:*) = Left {:a} + Right {:b}

Can we then say that our functions are ALL functions of ONE argument - a tuple. Currying is achieved via setting
different tuple values to something.

f(x,y,z) = x + y + z
g = f(y=3) --> g(x,z) = x + 3 + z
h = f(2,4) --> h(z) = 2 + 4 + z - reduce to 6 + z! (call by need, how?? modify code at runtime???)

If it's a tuple then it's angled brackets and no comas:
-- in case we have a named list type
map { f:a->b * ls:[a] } : [b] = ls ? [] = [] | otherwise = f ls.fst :: map { f * ls.snd }
-- or with pattern matching:
map { f:a->b * ls:[a] } : [b] = ls ? [] = [] | x :: xs = f x :: map { f * xs }

Ok, there might be something there - make the whole thing explicitly tuple centric and allow building tuples 
and addressing it's elements as we see fit (unlike Haskell).

Then, interesting type inference rules, e.g. if we have a function without explicit type signatures that does something like

print r = "We have " + r.name + ", " + show r.age + " years old"

type checker shall deduce type for r: {name : String, age: a in Show } - and then we should be able to use print
on ALL records that contain these 2 fields!!! Now, that's polymorphism! 

The type above is basically a *type constraint*!! Which means we can naturally introduce any kinds of constraints to 
the type checker and it should work. So, for r in reality we'll have "*, has_fields (name:String, age:a in Show)".

What more can we do with constraints? Not just at the type level, but at the field level.
Basically, constraints are functions from the type to Bool which we run at the static typechecking stage.
Do we keep them in dynamics - e.g., for checking user input?? E.g.

Person = Person {
    name : String
  * age : Int where (age > 0) & (age < 120)
  * zip : String where isValidZip
}

then we check constrains statically (where possible) and dynamically - we throw an exception if a constraint isn't met.
Or not an exception, but return Either Person Error or something similar.

Also, with subtyping and products and tuples we can do Entity ideas easier, e.g.:

IdEntity (record:*, idType:*) = record * {id : idType}
EPerson = IdEntity (Person, Int) -- id : Int, then all person fields. Do we need it? Or should we store Person explicitly?

Based on the definition above, IdEntity can be used EVERYWHERE where a record would normally be used due to subtyping!!!
This may be interesting.

Post = Post {
    title : String
  * body  : Text
  * dateCreated : Date
}

relation wrote  = Person -> [Post] -- 1 to many relationship
relation author = Post -> Person -- 1:1 relationship, must have an author

Now, we need to have some (type?) function that takes mentioned types and relationships and creates 
other types that allow us to represent and persist these relationships.

E.g., in this case it's enough to put Person Id to the Post to implement both. But what can be deriving algos?
1:1 gives id implicitly?
# candid
Dependently typed programming language with timeless referential transparently.

We functional programming language aficionados enjoy our referential
transparency. We point to our functions and say "here, I know that whenever I
see this expression it will always return the same value". Then you want to use
some package that requires a newer version of some other package, and suddenly
your code doesn't even type check much less return the same value. Or there's a
new version of the programming language, and old code doesn't compile on it
anymore.

This is an attempt to fix all of that, by reworking a number of assumptions
that we make about the mechanics of programming.

## Inspirations

### [Morte](https://hackage.haskell.org/package/morte/docs/Morte-Tutorial.html)

Morte is a minimalist implementation of the [calculus of
constructions](https://en.wikipedia.org/wiki/Calculus_of_constructions). Every
type checked program is total, and thus will terminate in finite time. Morte
uses this property to optimise it's input by evaluating it to the extent
possible. Morte doesn't do any type inference, just type checking and that is
complete, so any complete set of code that compiles now will always compile.

Unfortunately, Morte cannot be implemented in itself, not being total. Morte is
also based on the traditional text file full of names in the write-parse-type
check-compile loop.

### [Unison](https://unisonweb.org/)

Unison is much more of a traditional functional programming language is some
respects, with unbounded recursion and type inference. However, it's purpose is
to ease distributed computation, and it needs to be able to send functions to
other nodes to evaluate, and know that they have all the same pieces.

To this end, Unison identifies functions by a hash if it's content. If you have
`a x = x b` and you make a new version of `b`, the system will need to make a
new version of `a` also. However, if you have the hash of a, there's no
changing it. The hash will always point to the same function.

To this end, Unison strives to do away with the notion of textual source code.
Instead of editing a text file, the goal is to be editing abstract syntax trees
directly. This introduces quite a shift from the traditional programming
process, there is no more parsing of source code.

## Candid

Both these systems have a level of timeless referential transparency. Morte
with it's check-only type system, and Unison with it's immutable hash-tree
codebase. Yet if you have a Morte URL reference, you can't be sure it's the
same data without keeping extra information. And how strong is the guarantee
that Unison's type inference won't ever change?

Candid strives to do both, given a hash, it will always expand to the same
code, and if that code type checked before, it will type check to at least the
same degree later, and it will evaluate to the same result (given the same
inputs of course). There are no circumstances where the compiler is guessing at
what the programmer means, so there's no leeway to change what assumptions the
language makes.

### The Calculus

One might represent Candid's (and Morte's) inner calculus something like this:

```idris
data Expr
  = Lam (inType : Expr) (body : Expr)    -- inType ⇒ body
  | Ref (n : Nat)                        -- n
  | App (func : Expr) (arg : Expr)       -- func arg
  | Pi (inType : Expr) (outType : Expr)  -- inType → outType
  | Star                                 -- *
```

* `Lam` encloses a function, apply an argument (type `inType`)
  and it returns `body` with references to it replaced by that argument.

* `Ref` refers back `n` arrows, to a `Lam` or a `Pi`,
  to take the place of an argument on function application.

* `App` applies the argument `arg` to the function `func`.

* `Pi` is the type of `Lam`, where `inType`s match
  and `outType` is the derived type of the `body`.

* `Star` represents a type, informally this includes the type of `Star` and
  a `Pi` with an output of `Star` - though reality is more complicated.

With these one may define functions:

```
id      = * ⇒ 0 ⇒ 0
```

`id` is the simplest function, it simply returns it's argument. However, as
every `Lam` requires a input type, it takes an additional argument first that
specifies the type. Candid replaces polymorphism with specialization. Notice
also how the two `0`s refer to different arguments, the first one refers the
first argument (which is a type) and the second to the second (which is a value
of that type).

```
Unit    = * → 0 → 1
```

`Unit` is the type of `id`, so named because `id` is the only one function of
that type. Here both `0` and `1` refer to the first argument, the type of the
value given to and returned by `id`.

```
Boolean = * → 0 → 1 → 2
True    = * ⇒ 0 ⇒ 1 ⇒ 1
False   = * ⇒ 0 ⇒ 1 ⇒ 0
```

`Boolean` has exactly two inhabitants, `True` and `False`, which one is which
is purely arbitrary, but, for our purposes, `True` returns the first value and
`False` returns the second, and thus function application:

```
or     = Boolean ⇒ Boolean ⇒ 1 Boolean True 0
and    = Boolean ⇒ Boolean ⇒ 1 Boolean 0 False
```

`or` takes two `Boolean`s uses the first one to choose between `True` and the
second. Sugaring this to something more familiar:

```idris
or : Bool -> Bool -> Bool
or a b = case a of
              True -> True
              False -> b
```

`and` could be sugared in exactly the same way.

Both are of the type `Boolean → Boolean → Boolean`. Note that above we don't
make any allowances to refer to things by name in the data structure up above.
`Boolean` here is just a placeholder for `* → 0 → 1 → 2`.

For more examples of what you can do with this sort of minimal calculus of
constructions, albeit with names instead of numbers, see the Morte
[Tutorial](https://hackage.haskell.org/package/morte/docs/Morte-Tutorial.html).

#### Hashes

```idris
  | Hash (hash : Vect 8 Bits32)
```

This is the first thing (other using natural number indices rather than names)
that differentiates Candid from Morte. `Boolean` can also be represented by the
256-bit hash `941ad446211f88e385d2b95be6336328b1708e77a11dca3e783a240d9dfadca5`.
Hashes replace well-typed closed sub-expressions, and indeed, introduce no
semantic differences what-so-ever, but are like a provable form of Morte's URL
references. Hashes are the basis of Candid "source code" as a pure immutable
data structure, a directed acyclic graph (albeit representing a cyclic one).

With just the above `Expr` type, we can only represent total functions, and only
total functions at that. This is by design, and is very helpful in writing
correct code. However, this also means we can't implement Candid in Candid.

#### Recursion

```idris
  | Rec (n : Nat)                        -- @n
  | Type (outType : Expr) (body : Expr)  -- outType | body
```

So we get primitive recursion. `Rec` is a lot like `Ref`, but instead of being
replaced with the argument, it is replaced with the function. `Type` is here to
assist the type checker, so it can calculate the output types of a `Rec`.

```
Nat = * → 0 → (@1 → 2) → 2
Zero = * ⇒ 0 ⇒ (Nat → 2) ⇒ 1
Succ = Nat ⇒ * ⇒ 0 ⇒ (Nat → 2) ⇒ 0 3
add = Nat ⇒ Nat ⇒ Nat | 0 Nat 1 (@1 (Succ 1))
```

This is the recursive version of Nat, which sugars to:

```idris
data Nat = Zero | Succ Nat
add : Nat -> Nat -> Nat
add n m = case m of
               Zero -> n
               Succ p -> add (Succ n) p
```

Now all (or nearly all) the interesting things to do with `Nat`s can be done
without this sort of recursion using `Nat = * → 0 → (1 → 2) → 2` on which
operations are provably total by dint of type checking. So consider...

```
Expr = * →
       0 →
       1 →
       (Nat → 3) →
       (Nat → 4) →
       (@4 → @5 → 6) →
       (@5 → @6 → 7) →
       (@6 → @7 → 8) →
       (@7 → @8 → 9) →
       8
```

### Editor

Notice that there're a lot of handy features missing here. No variable names or
function names, just numbers and hashes! No type inference, type classes, or
implicit arguments! All that bookkeeping overhead passing all these types
around! Are you sure this isn't one of those esoteric languages? Well, that's
where the editor comes in.

Presented above is the data structure, how code in stored inside the compiler,
the interpreter, the code store, and the editor. It is, at the same time, quite
verbose and yet terse and inscrutable. Therefore editors also store names to
give functions and arguments, comments, presentation data, everything they need
to transform that data structure into something humans can work with.

Editors can hide any type annotation that is normally implicit and inferred,
they can infer that type information to start with. They can present numbers
with digits instead of as functions, gave names to your functions and
arguments, expand and collapse expressions, pretty print the math, and
automatically derive sufficiently constrained functions by their type.

By editing directed acyclic graphs rather than blobs of text, there are no
syntax errors, no reformatting, type checking may be done at edit time,
*compilation* can be done incrementally at edit time, all valid type checked
expressions are efficiently saved in the code store until garbage collection -
long term undo history at the granularity of functional changes.

And we can still have all the magic that comes of parsing and syntactic sugar,
of type inference and implicit arguments, but it'll be magic you can look at,
magic you can change by tweaking your editor, not the compiler, transparent
magic, magic revealed, candid magic.

### Structurally Equivalent Types

One of the boons of using de Bruijn indices is that we get α-equivalence for
free. That is, checking whether two expressions have the same structure,
disregarding names. This also means that:

```idris
data Ordering = LT | EQ | GT
```

is exactly the same as

```idris
data Axis = X | Y | Z
```

and any other enumerated type with three values:

```
Ternary = * → 0 → 1 → 2 → 3
Zero = * ⇒ 0 ⇒ 1 ⇒ 2 ⇒ 2
One = * ⇒ 0 ⇒ 1 ⇒ 2 ⇒ 1
Two = * ⇒ 0 ⇒ 1 ⇒ 2 ⇒ 0
```

This means that our type checker loses the power to distinguish between `Axis`
and `Ordering`.  There are a number of ways to regain this power, and one
requires nothing extra from the language, introducing a singleton into the type.

There are an infinite number of singleton types. `* → 0 → 1` is the simplest and
most obvious, but `* → * → 1 → 2` is much the same. While one could go on
forever just adding intermediate type arguments, consider the type:

```
* → * → * → * → (4 → 3 → 2 → 4 → 6) → 1 → 2
```

So long as the arguments to the function and the value argument are not all of
the same type, there is obviously only one possible function with that type, in
this case:

```
* ⇒ * ⇒ * ⇒ * ⇒ (4 → 3 → 2 → 4 → 6) ⇒ 1 ⇒ 0
```

With $n$ type arguments and $m$ arguments to the function, this yields
$n^{m+2}-n$ unique singleton types, 4092 for the model above.

So to make a type unique, one simply prepends a unique singleton type.

```
Axis = (* → * → * → * → (4 → 3 → 2 → 4 → 6) → 1 → 2) → * → 0 → 1 → 2 → 3
X = (* → * → * → * → (4 → 3 → 2 → 4 → 6) → 1 → 2) ⇒ * ⇒ 0 ⇒ 1 ⇒ 2 ⇒ 2
```

As there is only one possible value for the singleton type, it can be filled in
automatically (and hidden) by a sufficiently smart editor. During type checking
this will ensure that `X` isn't used in place of `LT` or vice versa, and it will
be optimized away with partial evaluation during compilation, producing the
exact same code as `Axis = *  → 0 → 1 → 2 → 3`.

Note this also allows one to specify a function:

```
(* → 0 → 1 → 2 → 2) ⇒ (* → * → * → * → (4 → 3 → 2 → 4 → 6) → 1 → 2) ⇒ 1
```

which turns an `Ordering` into an `Axis`, much like the lone constructor in Haskell's `newtype`.

### Type Inference

If there is a hole in the expression, it is helpful to be able to infer the type
of the expressions that fit in the hole. Though we call this type inference, it
takes place in the editor rather than the complier and charges or improvements
do not change any already written programs.

We start with an expression and a path to the sub-expression which we want to
infer the type of, and an expected output type. In general we follow the path,
changing the expected output type based on the root of our current expression.

* `Pi` expects a type for both arguments.
* `Lam` expects a type for it's type and anything for the body.
* `Type` expects a type for it's type and that type for it's body.
* `App` is a little more complicated. The type of the function must be a `Pi`
  from the type of the argument to the expected type. So if we're looking for
  that we need to figure out the type of the argument first. Conversely to infer
  the type of the argument we need to determine the type of the function, and we
  expect that function's input type.

When we reach the end of the path we simply return the expected type. Thus our
result can be anything, any type, a value of some type, or a Pi from one of
those to another. with this type pattern we can match or generate expression
to fit in the hole.

### What to do about Universes

Informally, `★` is the type of `★` and `★ → ★` and the like. In reality, stars
are paramaterized over natural numbers, such that the type of `★ₙ`  is `★ₙ₊₁`.
This is to prevent inconsistancy due to Girard's Paradox.

In the current implementations, `★` is either of type `□` or (paradoxically) of
type `★`. Eventually there will be universe inference, which will result in some
ultimately invalid programs that currently type check fail to type check.  This
is the sole exception to Candid's forward compatibility guarentee - if there is
a bug in Candid that results in an inconsistant logic, programs that depend on
that bug will not work in future versions.

The problem with `★` is having of type `□` is that one cannot use the
conventional definition of `Bool` as a dependant type, and cannot even convert a
value `Bool` to a type-level `Bool`:

```
Bool = t:★ → true:t → false:t → t
True = t:★ ⇒ true:t ⇒ false:t ⇒ true
False = t:★ ⇒ true:t ⇒ false:t ⇒ false

<s>x:Bool → x ★ Bool Nat</s>

Bool★ = true:★ → false:★ → ★
True★ = true:★ ⇒ false:★ ⇒ true
False★ = true:★ ⇒ false:★ ⇒ false

<s>Bool★fromBool = bool:Bool ⇒ bool Bool★ True★ False★</s>
```

With universe inference it just looks like this:

```
Boolₙ = t:★ₙ → true:t → false:t → t
Trueₙ = t:★ₙ ⇒ true:t ⇒ false:t ⇒ true
Falseₙ = t:★ₙ ⇒ true:t ⇒ false:t ⇒ false

x:Boolₙ₊₊ → x ★ₙ Boolₙ Natₙ
```

### Programs for Free

Consider the type `Map`, which is the type of a `map` function for the type `m`:

```
Map = m:(★ → ★) ⇒ t:★ → s:★ → fun:(t → s) → val:(m t) → m s
```

For each type `m` there will be a few possible programs that satisfy that type.
For instance given the type `Maybe` and it's constructors `Nothing` and `Just`:

```
Maybe = t:★ ⇒ ★
	s:★ → nothing:s → just:(t → s) → s
Nothing = t:★ ⇒ Maybe t
	s:★ ⇒ nothing:s ⇒ just:(t → s) ⇒ nothing
Just = t:★ ⇒ x:t ⇒ Maybe t
	s:★ ⇒ nothing:s ⇒ just:(t → s) ⇒ just x
```

Our `map` function will have the type `Map Maybe`:

```
Map Maybe = ★
	t:★ → s:★ → fun:(t → s) → val:(Maybe t) → Maybe s
```

We need a `Maybe s`, which we can get via `Nothing s` and `Just s y`, where `y`
is of type `s`. so one obvious (and wrong) option is:

```
map' = t:★ ⇒ s:★ ⇒ fun:(t → s) ⇒ val:(Maybe t) ⇒ Maybe s
	Nothing s
```

However we also have `fun:(t → s)` and `val:(Maybe t)`. Either `val` is a
`Nothing t`, in which case our only option is to use `Nothing s`, or it is a
`Just t x`, in which case we have have two options, `Nothing s` and `(x:t ⇒ Just
s (fun x))`. This points to the actual `map` function between `Maybe`s:

```
map = t:★ ⇒ s:★ ⇒ fun:(t → s) ⇒ val:(Maybe t) ⇒ Maybe s
	val (Maybe s) (Nothing s) (x:t ⇒ Just s (fun x))
```

The heuristic here is to prefer solutions that use more arguments. If the type
specifies that we take an argument, we probably want to use it in our program.
Note that, as all of this takes place in the editor, it is just as easy to
present the user with ten suggestions as one. The heuristic can just be used to
order the suggestions. On the other hand, if there is only one possible program
with a particular type, the editor could just fill it in.

Sometimes though, there are more than just a few options.

```
List = t:★ ⇒ ★
	r:★ → nil:r → cons:(r → t → r) → r
Nil = t:★ ⇒ List t
	r:★ ⇒ nil:r ⇒ cons:(r → t → r) ⇒ nil
Cons = t:★ ⇒ a:t ⇒ list:(List t) ⇒ List t
	r:★ ⇒ nil:r ⇒ cons:(r → t → r) ⇒ cons (list r nil cons) a
```

The type `t:★ → xs:(List t) → List t` has countably infinite different
inhabitants. At the simplest there are `Nil t` and `xs`, for a little more
effort any other duplication and/or permutation of `xs` works too. Thus we
cannot present every solution - rather we must take the first few results of a
breadth first search for solutions.

Note that if there are infinite inhabitants of `List t → List t` than there are
likewise infinite inhabitants of `List t → List s`. Note also that there are
infinite programs equivalent to any other program, simply by encapsulating the
program in an `id`. A properly defined breadth first search will relegate those
solutions to later on, where they will usually not even be enumerated.

### Complier

* partial evaluation / specialization
* automatic source fetching
* extendible core data types
* automatic core data types

### Runtime

* type system enforced capabilities
* forward (and limited backward) compatibility via translation layer specialization

## Unlicensed

[This is free and unencumbered software released into the public domain.](https://unlicense.org/)

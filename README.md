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
  = Lam (inType : Expr) (body : Expr)    -- inType → body
  | Ref (n : Nat)                        -- n
  | App (func : Expr) (arg : Expr)       -- func arg
  | Pi (inType : Expr) (outType : Expr)  -- inType ⇒ outType
  | Star                                 -- *
  | Box                                  -- □
```

* `Lam` encloses a function, apply an argument (type `inType`)
  and it returns `body` with references to it replaced by that argument.

* `Ref` refers back `n` arrows, to a `Lam` or a `Pi`,
  to take the place of an argument on function application.

* `App` applies the argument `arg` to the function `func`.

* `Pi` is the type of `Lam`, where `inType`s match
  and `outType` is the derived type of the `body`.

* `Star` is the type of a `Pi`.

* `Box` is the type of a `Star`.

With these one may define functions:

```
id      = * → 0 → 0
```

`id` is the simplest function, it simply returns it's argument. However, as
every `Lam` requires a input type, it takes an additional argument first that
specifies the type. Candid replaces polymorphism with specialization. Notice
also how the two `0`s refer to different arguments, the first one refers the
first argument (which is a type) and the second to the second (which is a value
of that type).

```
Unit    = * ⇒ 0 ⇒ 1
```

`Unit` is the type of `id`, so named because `id` is the only one function of
that type. Here both `0` and `1` refer to the first argument, the type of the
value given to and returned by `id`.

```
Boolean = * ⇒ 0 ⇒ 1 ⇒ 2
True    = * → 0 → 1 → 1
False   = * → 0 → 1 ⇒ 0
```

`Boolean` has exactly two inhabitants, `True` and `False`, which one is which
is purely arbitrary, but, for our purposes, `True` returns the first value and
`False` returns the second, and thus function application:

```
or     = Boolean → Boolean → 1 Boolean True 0
and    = Boolean → Boolean → 1 Boolean 0 False
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

Both are of the type `Boolean ⇒ Boolean ⇒ Boolean`. Note that above we don't
make any allowances to refer to things by name in the data structure up above.
`Boolean` here is just a placeholder for `* ⇒ 0 ⇒ 1 ⇒ 2`.

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
Nat = * ⇒ 0 ⇒ (@1 ⇒ 2) ⇒ 2
Zero = * → 0 → (Nat ⇒ 2) → 1
Succ = Nat → * → 0 → (Nat ⇒ 2) → 0 3
add = Nat → Nat → Nat | 0 Nat 1 (@1 (Succ 1))
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
without this sort of recursion using `Nat = * ⇒ 0 ⇒ (1 ⇒ 2) ⇒ 2` on which
operations are provably total by dint of type checking. So consider...

```
Expr = * ⇒
       0 ⇒
       1 ⇒
       (Nat ⇒ 3) ⇒
       (Nat ⇒ 4) ⇒
       (@4 ⇒ @5 ⇒ 6) ⇒
       (@5 ⇒ @6 ⇒ 7) ⇒
       (@6 ⇒ @7 ⇒ 8) ⇒
       (@7 ⇒ @8 ⇒ 9) ⇒
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

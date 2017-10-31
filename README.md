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

One might represent Candid's instances of inner calculus something like this:

```
Unit    = π * ⇒ π 0 ⇒ 1
```

Where `Unit` is the type of functions that take any type `*`, take a any value
of that type (numbers count back across $n+1$ arrows, so `0` refers back to the
first `π`), and return a value of that type.

```
id      = λ * ⇒ λ 0 ⇒ 0
```

Likewise `id` is the only function inhabiting that type, it's the identity
function, returning the value that it recieved.

```
Boolean = π * ⇒ π 0 ⇒ π 1 ⇒ 2
true    = λ * ⇒ λ 0 ⇒ λ 1 ⇒ 1
false   = λ * ⇒ λ 0 ⇒ λ 1 ⇒ 0
```

`Boolean` has exactly two inhabitants, `true` and `false`, which one is which
is purely arbitrary, but, for our purposes, `true` returns the first value and
`false` returns the second, and so:

```
or     = λ Boolean ⇒ λ Boolean ⇒ 1 Boolean true 0
and    = λ Boolean ⇒ λ Boolean ⇒ 1 Boolean 0 false
```

That is, `or` takes two booleans and gives `Boolean`, `true`, and the second
value to the first. If the first is `true` it will return `true`, if it is
`false` it will return the second value. Type checking simply consists of
making sure every `λ` and `π` have a `π` or `*` (or reference to one) as their
type, and that every instance of function application gives an argument of the
correct type.

Note that there are no argument names, just numbers, the names in the above
examples are merely for show, `Boolean` simply means `(π * ⇒ π 0 ⇒ π 1 ⇒ 2)`.

For more examples of what you can do with this sort of minimal calculus of
constructions, albeit with names instead of numbers, see the Morte
[Tutorial](https://hackage.haskell.org/package/morte/docs/Morte-Tutorial.html).

There is also a recursion primitive, which acts much like the references, it
points back across $n+1$ arrows to a `π` or `λ`. Except, instead of referencing
the argument, it represents a copy of the whole expression, so during function
application when a `λ` is removed, a recur would be replaced with that `λ`.
This allows non-total functions in the form of infinite recursion, but also
allows the implementation of Candid in itself, while still eliminating dynamic
type errors.

Additionally, a hash expression stands in for an sub-expression that doesn't
reference or recur it's parent expressions, known as a closed expression. It is
[very likely](https://en.wikipedia.org/wiki/Birthday_attack#Mathematics) a
unique identifier, computed from the content of the expression it represents.
When an expression is stored, all closed sub-expressions are represented with
hashes.

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

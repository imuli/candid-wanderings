## Names

The core dataype from the editor through the compilier looks like this:

```idris
data Expr
  = Box
  | Star
  | Hole
  | Refer (n : Nat)
  | Recur (n : Nat)
  | Apply (func : Expr) (arg : Expr)
  | Lambda (inType : Expr) (body : Expr)
  | Pi (inType : Expr) (outType : Expr)
  | Type (outType : Expr) (body : Expr)
  | Hash (hash : Bits 256)
```

This makes no accomodation for human-readability.
Humans like names for functions, arguments, and even temporary values.
Comments would be great too.

The options seem threefold:

1. Adding new values to `Expr`:
   ```idris
     | Note (note : String) (body : Expr)
     | Name (name : String) (body : Expr)
   ```

2. Adding more arguments to old values:
   ```idris
     | Apply (func : Expr) (arg : Expr) {comment : String}
     | Lambda (inType : Expr) (body : Expr) {funcName : String} {argName : String}
     | Pi (inType : Expr) (outType : Expr) {typeName : String} {argName : String}
     | Hash (hash : Bits 256) {name : String}
   ```

3. External metadata:
   ```idris
   data MetaExpr
     = Func (hash : Bits 256) (name : String)
     | Arg (hash : Bits 256) (name : String)
     | Note (hash : Bits 256) (note : String)
   data MetaData = Meta (hash : Bits 256) (data : [MetaExpr])
   ```
   where `MetaData` points to a closed expression with hash `hash` and
   each `MetaExpr` points to an open sub-expression with a `hash` of both
   the enclosing context and the sub-expression in question.

The other consideration here is that the internal and storage datatypes face different pressures
and they don't need to be the same. We want to:

* Preserve names through β-reduction and type checking. (2)
* Edit expressions without churning hash calculations. (1,2)
* Simplify expression manipulation code. (2)

* Allow different users to share code while using their own names for the same expression. (3)
* Annotate and write code in multiple languages. (3)
* Host an append-only archive of valid expressions. (3)

This points to using (2) for the internal representation and (3) for the external representation.

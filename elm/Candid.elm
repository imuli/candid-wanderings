module Candid exposing (..)

import Either exposing (..)
import List exposing (head, drop)
import Blake2s1 exposing (..)

type Expr
  = Star
  | Hole
  | Pi String String Expr Expr -- name argname type body
  | Lam String String Expr Expr -- name argname type body
  | App String Expr Expr -- name function argument
  | Ref Int -- count
  | Rec Int -- count
  | Note String Expr -- comment body
  | Type String Expr Expr -- name type body
  | Hash String Blake2s1 -- name hash

{- the greatest Refs or Recs beyond the number of enclosing Lams and Pis -}
closed : Expr -> Int
closed expr = case expr of
  Star -> -1
  Hole -> -1
  Hash _ _ -> -1
  Ref n -> n
  Rec n -> n
  Note _ b -> closed b
  Type _ _ b -> closed b
  Pi _ _ t b -> max (closed t) (closed b - 1)
  Lam _ _ t b -> max (closed t) (closed b - 1)
  App _ f a -> max (closed f) (closed a)

{- hash an expression -}
hash : Expr -> Blake2s1
hash expr = case expr of
  Star        -> Blake2s1.hash Blake2s1.zero Blake2s1.zero -1 0 0 1
  Hole        -> Blake2s1.hash Blake2s1.zero Blake2s1.zero -1 0 0 0
  Ref n       -> Blake2s1.hash Blake2s1.zero Blake2s1.zero 1 0 0 n
  Rec n       -> Blake2s1.hash Blake2s1.zero Blake2s1.zero 2 0 0 n
  Hash _ h    -> h
  Note _ b    -> hash b
  App _ f a   -> Blake2s1.hash (hash f) (hash a) 0 0 0 1
  Pi _ _ t b  -> Blake2s1.hash (hash t) (hash b) 0 0 0 3
  Type _ t b  -> Blake2s1.hash (hash t) (hash b) 0 0 1 0
  Lam _ _ t b -> Blake2s1.hash (hash t) (hash b) 0 0 0 2

{- recurse over an expression, poking refs and recs -}
over : (Int -> Int -> Expr) -> (Int -> Int -> Expr) -> Int -> Expr -> Expr
over ref rec c expr = case expr of
  Star        -> expr
  Hole        -> expr
  Ref n       -> ref n c
  Rec n       -> rec n c
  Hash _ _    -> expr
  Note s x    -> Note s (over ref rec c x)
  App n f a   -> App n (over ref rec c f) (over ref rec c a)
  Type n t b  -> Type n (over ref rec c t) (over ref rec c b)
  Pi n a t b  -> Pi n a (over ref rec c t) (over ref rec (c+1) b)
  Lam n a t b -> Lam n a (over ref rec c t) (over ref rec (c+1) b)

{- shift open refs and recs in an expression by z -}
shift : Int -> Expr -> Expr
shift z = over (\n c -> Ref <| if n >= c then (n + z) else n)
               (\n c -> Rec <| if n >= c then (n + z) else n) 0

{- excute one β-reduction: replace refs and recs inside an expression -}
replace : Expr -> Expr -> Expr -> Expr
replace ref rec exp = shift (-1) <|
  over (\n c -> if n == c then shift (c+1) ref else Ref n)
       (\n c -> if n == c then shift (c+1) rec else Rec n) 0 exp

{- reduce an expression -}
reduce : Expr -> Expr
reduce expr =
  case expr of
       Pi n a t b  -> Pi n a (reduce t) (reduce b)
       -- TODO η-reduction
       Lam n a t b -> Lam n a (reduce t) (reduce b)
       -- β-reduction
       App n f a -> let rf = reduce f
                    in case rf of
                            Lam _ _ _ b -> reduce <| replace a rf b
                            _ -> App n rf a
       Type _ _ b -> reduce b
       Note _ b -> reduce b
       _ -> expr


{- a path can identify a particular subexpression in an expression -}
type Step = Name | ArgName | TypeS | Body | Func | Arg | Value
type alias Path = List Step

type Replacement
  = S String
  | E Expr
  | N Int

sub : Replacement -> Expr -> Path -> Maybe Expr
sub repl expr path =
  case path of
    -- terminal cases
    [] -> case repl of
      E x -> Just x
      _   -> Nothing
    Name :: [] -> case (repl, expr) of
      (S s, Lam _ a t b) -> Just <| Lam s a t b
      (S s, Pi _ a t b)  -> Just <| Pi s a t b
      (S s, Type _ t b)  -> Just <| Type s t b
      (S s, App _ f a)   -> Just <| App s f a
      (S s, Hash _ h)    -> Just <| Hash s h
      (_, _)             -> Nothing
    ArgName :: [] -> case (repl, expr) of
      (S s, Lam n _ t b) -> Just <| Lam n s t b
      (S s, Pi n _ t b)  -> Just <| Pi n s t b
      (_, _)             -> Nothing
    Value :: [] -> case (repl, expr) of
      (N n, Ref _)       -> Just <| Ref n
      (N n, Rec _)       -> Just <| Rec n
      (_, _)             -> Nothing
    -- Name, ArgName, and Value must terminate the path
    Name :: _ -> Nothing
    ArgName :: _ -> Nothing
    Value :: _ -> Nothing
    -- recursive cases
    TypeS :: rest -> case expr of
      Lam n a t b -> Maybe.map (\x -> Lam n a x b) (sub repl t rest)
      Pi n a t b  -> Maybe.map (\x -> Pi n a x b) (sub repl t rest)
      Type n t b  -> Maybe.map (\x -> Type n x b) (sub repl t rest)
      _           -> Nothing
    Body :: rest -> case expr of
      Lam n a t b -> Maybe.map (\x -> Lam n a t x) (sub repl b rest)
      Pi n a t b  -> Maybe.map (\x -> Pi n a t x) (sub repl b rest)
      Type n t b  -> Maybe.map (\x -> Type n t x) (sub repl b rest)
      Note n b    -> Maybe.map (\x -> Note n x) (sub repl b rest)
      _           -> Nothing
    Func :: rest -> case expr of
      App n f a -> Maybe.map (\x -> App n x a) (sub repl f rest)
      _         -> Nothing
    Arg :: rest -> case expr of
      App n f a -> Maybe.map (\x -> App n f x) (sub repl a rest)
      _         -> Nothing

-- contextual equality
ceq : List Expr -> Expr -> List Expr -> Expr -> Bool
ceq cx x cy y =
  case (x,y) of
       -- Trivial cases
       (Star, Star) -> True
       (Hole, Hole) -> True
       -- Simple cases
       (Ref n1, Ref n2) -> n1 == n2
       (Rec n1, Rec n2) -> n1 == n2
       -- Recursive cases
       (Note _ b, _) -> ceq cx b cy y
       (_, Note _ b) -> ceq cx x cy b
       (Type _ _ b, _) -> ceq cx b cy y
       (_, Type _ _ b) -> ceq cx x cy b
       -- Birecusive cases
       (App _ f1 a1, App _ f2 a2) -> ceq cx f1 cy f2 && ceq cx a1 cy a2
       (Pi _ _ t1 b1, Pi _ _ t2 b2) -> ceq cx t1 cy t2 && ceq (x::cx) b1 (y::cy) b2
       (Lam _  _ t1 b1, Lam _ _ t2 b2) -> ceq cx t1 cy t2 && ceq (x::cx) b1 (y::cy) b2
       -- A recursive type may refer back to a Pi or Lambda
       (_, Rec n) -> ceq cy y cx x
       (Rec n, _) -> case head <| drop n cx of
                          Nothing -> False
                          Just x1 -> ceq (drop n cx) x1 cy y
       -- That's all.
       (_, _) -> False

eq : Expr -> Expr -> Bool
eq x y = ceq [] x [] y

type TypeError
  = TypeMismatch Expr Expr Expr -- application expected found
  | OpenExpression (List Expr) Expr -- context reference
  | NotAFunction Expr Expr -- function type
  | InvalidInputType Expr Expr -- pi type
  | InvalidOutputType Expr Expr -- pi type
  | UnknownHash Blake2s1 -- hash

with : Expr -> List Expr -> List Expr
with t ctx = List.map (shift 1) <| t :: ctx

typeIn : List Expr -> Expr -> Either TypeError Expr
typeIn ctx expr =
  let recur : List Expr -> Expr -> (Expr -> Either TypeError Expr) -> Either TypeError Expr
      recur c x f = andThen f (map reduce <| typeIn c x)
  in
  case expr of
       Hole -> Right Hole
       Star -> Right Star
       Ref n -> case head <| drop n ctx of
                     Nothing -> Left <| OpenExpression ctx expr
                     Just t -> Right t
       App _ f a -> recur ctx f <|
         \x -> case x of
                    Pi _ _ s t -> recur ctx a <|
                      \r -> if eq r s
                               then Right <| reduce <| replace a x t
                               else Left <| TypeMismatch expr s r
                    r      -> Left <| NotAFunction f r
       Lam _ a t f -> recur ctx t <|
         \_ -> recur (with t ctx) f <|
           \s -> Right <| Pi "" a t s
       Pi _ _ t f -> recur ctx t <|
         \x -> let right = recur (with t ctx) f <|
                             \y -> case y of
                                        Star -> Right Star
                                        _    -> Left <| InvalidOutputType expr y
               in case x of
                       Star -> right
                       _    -> Left <| InvalidInputType expr x
       Note _ b -> typeIn ctx b
       Rec _ -> Right Star
       Type _ t _ -> Right t
       Hash _ h -> Left (UnknownHash h)

typeOf : Expr -> Either TypeError Expr
typeOf expr = typeIn [] expr


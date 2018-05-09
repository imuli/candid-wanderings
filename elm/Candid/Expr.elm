module Candid.Expr exposing (..)

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
type Step = Name | ArgName | Value | Leftward | Rightward | None
type alias Path = List Step

type Replacement
  = S String
  | E Expr
  | N Int

lookup : Expr -> Path -> Maybe Replacement
lookup expr path =
  case path of
    -- terminal cases
    [] -> Just (E expr)
    Name :: [] -> case expr of
      Lam n _ _ _ -> Just (S n)
      Pi n _ _ _  -> Just (S n)
      Type n _ _  -> Just (S n)
      App n _ _   -> Just (S n)
      Hash n _    -> Just (S n)
      _           -> Nothing
    ArgName :: [] -> case expr of
      Lam _ a _ _ -> Just (S a)
      Pi _ a _ _  -> Just (S a)
      _           -> Nothing
    Value :: [] -> case expr of
      Note s _    -> Just (S s)
      Ref n       -> Just (N n)
      Rec n       -> Just (N n)
      _           -> Nothing
    -- Name, ArgName, and Value must terminate the path
    Name :: _ -> Nothing
    ArgName :: _ -> Nothing
    Value :: _ -> Nothing
    -- None always does Nothing
    None :: _ -> Nothing
    -- recursive cases
    Leftward :: rest -> case expr of
      Lam n a t b -> lookup t rest
      Pi n a t b  -> lookup t rest
      Type n t b  -> lookup t rest
      App n f a   -> lookup f rest
      _           -> Nothing
    Rightward :: rest -> case expr of
      Lam n a t b -> lookup b rest
      Pi n a t b  -> lookup b rest
      Type n t b  -> lookup b rest
      App n f a   -> lookup a rest
      Note s b    -> lookup b rest
      _           -> Nothing

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
      (S s, Note _ b)    -> Just <| Note s b
      (N n, Ref _)       -> Just <| Ref n
      (N n, Rec _)       -> Just <| Rec n
      (_, _)             -> Nothing
    -- Name, ArgName, and Value must terminate the path
    Name :: _ -> Nothing
    ArgName :: _ -> Nothing
    Value :: _ -> Nothing
    -- None always does Nothing
    None :: _ -> Nothing
    -- recursive cases
    Leftward :: rest -> case expr of
      Lam n a t b -> Maybe.map (\x -> Lam n a x b) (sub repl t rest)
      Pi n a t b  -> Maybe.map (\x -> Pi n a x b) (sub repl t rest)
      Type n t b  -> Maybe.map (\x -> Type n x b) (sub repl t rest)
      App n f a   -> Maybe.map (\x -> App n x a) (sub repl f rest)
      _           -> Nothing
    Rightward :: rest -> case expr of
      Lam n a t b -> Maybe.map (\x -> Lam n a t x) (sub repl b rest)
      Pi n a t b  -> Maybe.map (\x -> Pi n a t x) (sub repl b rest)
      Type n t b  -> Maybe.map (\x -> Type n t x) (sub repl b rest)
      App n f a   -> Maybe.map (\x -> App n f x) (sub repl a rest)
      Note s b    -> Maybe.map (\x -> Note s x) (sub repl b rest)
      _           -> Nothing

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
  = TypeMismatch (List Expr) Expr Expr Expr -- context expr expected actual
  | OpenExpression (List Expr) Expr -- context reference/recur
  | TypeInference (List Expr) Expr -- context recur
  | UnknownHash Blake2s1 -- hash
  | BadContext -- shouldn't ever come up

typecheck : Expr -> List Expr -> Bool -> Either TypeError Expr
typecheck expr ctx trust =
  let isStar parent ty = case ty of
      Star -> Right Star
      _    -> Left (TypeMismatch ctx parent Star ty)
  in case (trust, expr) of
    -- trivial cases
    (True, Rec i)    -> Left (TypeInference ctx expr)
    (True, Pi n a ty body) -> Right Star
    (True, Type n ty body) -> Right ty
    (_, Star)        -> Right Star
    (_, Hole)        -> Right Hole
    (_, Note n body) -> typecheck body ctx trust
    (_, Hash n h)    -> Left (UnknownHash h)
    -- backward looking cases
    (_, Ref i)       ->
      case head <| drop i ctx of
        Nothing -> Left (OpenExpression ctx expr)
        Just (Lam _ _ ty _) -> Right (shift (i+1) ty)
        Just (Pi _ _ ty _)  -> Right (shift (i+1) ty)
        Just _             -> Left BadContext
    (_, Rec i)       ->
      case head <| drop i ctx of
        Nothing -> Left (OpenExpression ctx expr)
        Just x  -> typecheck x (drop (i+1) ctx) True
    -- recursive cases
    (False, Pi n a ty body) ->
      typecheck ty ctx trust |>
      andThen (isStar ty) |>
      andThen (\_ -> typecheck body (expr :: ctx) trust) |>
      andThen (isStar ty)
    (False, Type n ty body) ->
      map reduce (typecheck body ctx trust) |>
      andThen (\actual ->
        if ceq ctx actual ctx ty
           then Right ty
           else Left (TypeMismatch ctx body ty actual)
      )
    (_, App n func arg) ->
      map reduce (typecheck func ctx trust) |>
      andThen (\functype -> case functype of
        Pi _ _ input output ->
          map reduce (typecheck arg ctx trust) |>
          andThen (\argtype ->
            if ceq ctx input ctx argtype
               then Right (replace arg functype output)
               else Left (TypeMismatch ctx arg input argtype)
          )
        _ -> Left (TypeMismatch ctx func (Pi "" "" Hole Hole) functype)
      )
    (_, Lam n a ty body) ->
      typecheck ty ctx trust |>
      andThen (isStar ty) |>
      andThen (\_ -> typecheck body (expr :: ctx) trust) |>
      andThen (\out -> Right (Pi "" a ty out))


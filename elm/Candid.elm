module Candid exposing (..)

import Either exposing (..)
import List exposing (head, drop)

type Expr
  = Star
  | Box
  | Hole
  | Pi String String Expr Expr -- name argname type body
  | Lam String String Expr Expr -- name argname type body
  | App Expr Expr -- function argument
  | Ref Int -- count
  | Rec Int -- count
  | Note String Expr -- comment body
  | Type Expr Expr -- type body

-- contextual equality
ceq : List Expr -> Expr -> List Expr -> Expr -> Bool
ceq cx x cy y =
  case (x,y) of
       -- Trivial cases
       (Star, Star) -> True
       (Box, Box) -> True
       (Hole, Hole) -> True
       -- Simple cases
       (Ref n1, Ref n2) -> n1 == n2
       (Rec n1, Rec n2) -> n1 == n2
       -- Recursive cases
       (Note _ b, _) -> ceq cx b cy y
       (_, Note _ b) -> ceq cx x cy b
       (Type _ b, _) -> ceq cx b cy y
       (_, Type _ b) -> ceq cx x cy b
       -- Birecusive cases
       (App f1 a1, App f2 a2) -> ceq cx f1 cy f2 && ceq cx a1 cy a2
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

over : (Int -> Int -> Expr) -> (Int -> Int -> Expr) -> Int -> Expr -> Expr
over ref rec =
  let i c exp = case exp of
                     Ref n       -> ref n c
                     Rec n       -> rec n c
                     Pi  f a t b -> Pi  f a (i c t) (i (c+1) b)
                     Lam f a t b -> Lam f a (i c t) (i (c+1) b)
                     App f b     -> App     (i c f) (i c b)
                     Note s x    -> Note s  (i c x)
                     Type t b    -> Type    (i c t) (i c b)
                     _ -> exp
  in i

shift : Int -> Int -> Expr -> Expr
shift z = over (\n c -> Ref <| if n >= c then (n + z) else n)
               (\n c -> Rec <| if n >= c then (n + z) else n)

replace : Expr -> Expr -> Expr -> Expr
replace ref rec exp = shift (-1) 0 <|
  over (\n c -> if n == c then shift (c+1) 0 ref else Ref n)
       (\n c -> if n == c then shift (c+1) 0 rec else Rec n) 0 exp

reduce : Expr -> Expr
reduce exp =
  case exp of
       Pi f a t b  -> Pi f a (reduce t) (reduce b)
       Lam f a t b -> Lam f a (reduce t) (reduce b) -- TODO η-reduction
       App f a -> let rf = reduce f
                  in case rf of
                          Lam _ _ _ b -> reduce <| replace a rf b
                          _ -> App rf a
       Type _ b -> reduce b
       Note _ b -> reduce b
       _ -> exp

type TypeError
  = TypeMismatch Expr Expr Expr -- application expected found
  | UntypedBox
  | OpenExpression (List Expr) Expr -- context reference
  | NotAFunction Expr Expr -- function type
  | InvalidInputType Expr Expr -- pi type
  | InvalidOutputType Expr Expr -- pi type

with : Expr -> List Expr -> List Expr
with t ctx = List.map (shift 1 0) <| t :: ctx

typeIn : List Expr -> Expr -> Either TypeError Expr
typeIn ctx expr =
  let recur : List Expr -> Expr -> (Expr -> Either TypeError Expr) -> Either TypeError Expr
      recur c x f = andThen f (map reduce <| typeIn c x)
  in
  case expr of
       Hole -> Right Hole
       Star -> Right Box
       Box -> Left UntypedBox
       Ref n -> case head <| drop n ctx of
                     Nothing -> Left <| OpenExpression ctx expr
                     Just t -> Right t
       App f a -> recur ctx f <|
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
                                        Box  -> Right Box
                                        _    -> Left <| InvalidOutputType expr y
               in case x of
                       Star -> right
                       Box  -> right
                       _    -> Left <| InvalidInputType expr x
       Note _ b -> typeIn ctx b
       Rec _ -> Right Star
       Type t _ -> Right t

typeOf : Expr -> Either TypeError Expr
typeOf expr = typeIn [] expr


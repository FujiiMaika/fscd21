open Syntax
open Value

(* actual calculation *)
(* Definitional interpreter for lambda-calculus with 4 delimited-control operators : eval1 *)

(* initial continuation : v -> t -> m -> v *)
let idc v t m = match t with
    TNil ->
    begin match m with
        MNil -> v
      | MCons ((c, t), m) -> c v t m
    end
  | Trail (h) -> h v TNil m

(* cons : (v -> t -> m -> v) -> t -> t *)
let rec cons h t = match t with
    TNil -> Trail (h)
  | Trail (h') -> Trail (fun v t' m -> h v (cons h' t') m)

(* apnd : t -> t -> t *)
let apnd t0 t1 = match t0 with
    TNil -> t1
  | Trail (h) -> cons h t1

(* f1 : e -> string list -> v list -> c -> t -> m -> v *)
let rec f1 e xs vs c t m = match e with
    Num (n) -> c (VNum (n)) t m 
  | Var (x) -> c (List.nth vs (Env.offset x xs)) t m
  | Op (e0, op, e1) ->
    f1 e0 xs vs (fun v0 t0 m0 ->
        f1 e1 xs vs (fun v1 t1 m1 ->
            begin match (v0, v1) with
                (VNum (n0), VNum (n1)) ->
                begin match op with
                    Plus -> c (VNum (n0 + n1)) t1 m1
                  | Minus -> c (VNum (n0 - n1)) t1 m1
                  | Times -> c (VNum (n0 * n1)) t1 m1
                  | Divide ->
                    if n1 = 0 then failwith "Division by zero"
                    else c (VNum (n0 / n1)) t1 m1
                end
              | _ -> failwith (to_string v0 ^ " or " ^ to_string v1
                               ^ " are not numbers")
            end) t0 m0) t m
  | Fun (x, e) ->
    c (VFun (fun v c' t' m' -> f1 e (x :: xs) (v :: vs) c' t' m')) t m
  | App (e0, e1) ->
    f1 e0 xs vs (fun v0 t0 m0 ->
        f1 e1 xs vs (fun v1 t1 m1 ->
            begin match v0 with
                VFun (f) -> f v1 c t1 m1
              | VContS (c', t') -> c' v1 t' (MCons ((c, t1), m1))
              | VContC (c', t') -> c' v1 (apnd t' (cons c t1)) m1
              | _ -> failwith (to_string v0
                               ^ " is not a function; it can not be applied.")
            end) t0 m0) t m
  | Shift (x, e) -> f1 e (x :: xs) (VContS (c, t) :: vs) idc TNil m
  | Control (x, e) -> f1 e (x :: xs) (VContC (c, t) :: vs) idc TNil m
  | Shift0 (x, e) ->
    begin match m with
        MCons ((c0, t0), m0) -> f1 e (x :: xs) (VContS (c, t) :: vs) c0 t0 m0
      | _ -> failwith "shift0 is used without enclosing reset"
    end
  | Control0 (x, e) ->
    begin match m with
        MCons ((c0, t0), m0) -> f1 e (x :: xs) (VContC (c, t) :: vs) c0 t0 m0
      | _ -> failwith "control0 is used without enclosing reset"
    end
  | Reset (e) -> f1 e xs vs idc TNil (MCons ((c, t), m))

(* f : e -> v *)
let f expr = f1 expr [] [] idc TNil MNil

open Syntax
open Value

(* Delinearized interpreter : eval5 *)

(* cons : (v -> t -> m -> v) -> t -> t *)
let rec cons h t = match t with
    TNil -> Trail (h)
  | Trail (h') -> Trail (fun v t' m -> h v (cons h' t') m)

(* apnd : t -> t -> t *)
let apnd t0 t1 = match t0 with
    TNil -> t1
  | Trail (h) -> cons h t1

(* run_c5 : c -> v -> s -> t -> m -> v *)
let rec run_c5 c v s t m = match (c, s) with
    (C0, []) ->
    begin match t with
        TNil ->
        begin match m with
            MNil -> v
          | MCons ((c, s, t), m) -> run_c5 c v s t m
        end
      | Trail (h) -> h v TNil m
    end
  | (CApp0 (e1, xs, c), VEnv (vs) :: s) -> f5 e1 xs vs (CApp1 (c)) (v :: s) t m
  | (CApp1 (c), v0 :: s) ->
    begin match v0 with
        VFun (f) -> f v c s t m
      | VContS (c', s', t') -> run_c5 c' v s' t' (MCons ((c, s, t), m))
      | VContC (c', s', t') ->
        run_c5 c' v s' (apnd t' (cons (fun v t m -> run_c5 c v s t m) t)) m
      | _ -> failwith (to_string v0
                       ^ " is not a function; it can not be applied.")
    end
  | (COp0 (e1, xs, op, c), VEnv (vs) :: s) ->
    f5 e1 xs vs (COp1 (op, c)) (v :: s) t m 
  | (COp1 (op, c), v0 :: s) ->
    begin match (v0, v) with
        (VNum (n0), VNum (n1)) ->
        begin match op with
            Plus -> run_c5 c (VNum (n0 + n1)) s t m
          | Minus -> run_c5 c (VNum (n0 - n1)) s t m
          | Times -> run_c5 c (VNum (n0 * n1)) s t m
          | Divide ->
            if n1 = 0 then failwith "Division by zero"
            else run_c5 c (VNum (n0 / n1)) s t m
        end
      | _ -> failwith (to_string v0 ^ " or " ^ to_string v ^ " are not numbers")
    end
  | _ -> failwith "stack or cont error"

(* f5 : e -> string list -> v list -> c -> s -> t -> m -> v *)
and f5 e xs vs c s t m = match e with
    Num (n) -> run_c5 c (VNum (n)) s t m 
  | Var (x) -> run_c5 c (List.nth vs (Env.offset x xs)) s t m
  | Op (e0, op, e1) ->
    f5 e0 xs vs (COp0 (e1, xs, op, c)) (VEnv (vs) :: s) t m
  | Fun (x, e) ->
    run_c5 c
      (VFun (fun v c' s' t' m' -> f5 e (x :: xs) (v :: vs) c' s' t' m')) s t m
  | App (e0, e1) ->
    f5 e0 xs vs (CApp0 (e1, xs, c)) (VEnv (vs) :: s) t m
  | Shift (x, e) -> f5 e (x :: xs) (VContS (c, s, t) :: vs) C0 [] TNil m
  | Control (x, e) -> f5 e (x :: xs) (VContC (c, s, t) :: vs) C0 [] TNil m
  | Shift0 (x, e) ->
    begin match m with
        MCons ((c0, s0, t0), m0) ->
        f5 e (x :: xs) (VContS (c, s, t) :: vs) c0 s0 t0 m0
      | _ -> failwith "shift0 is used without enclosing reset"
    end
  | Control0 (x, e) ->
    begin match m with
        MCons ((c0, s0, t0), m0) ->
        f5 e (x :: xs) (VContC (c, s, t) :: vs) c0 s0 t0 m0
      | _ -> failwith "control0 is used without enclosing reset"
    end
  | Reset (e) -> f5 e xs vs C0 [] TNil (MCons ((c, s, t), m))

(* f : e -> v *)
let f expr = f5 expr [] [] C0 [] TNil MNil

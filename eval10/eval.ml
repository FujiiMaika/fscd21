open Syntax
open Value

(* Interpreter with linearized instructions : eval10 *)

(* cons : h -> t -> t *)
let cons h t = match t with
    TNil -> Trail (h)
  | Trail (h') -> Trail (Append (h, h'))

(* apnd : t -> t -> t *)
let apnd t0 t1 = match t0 with
    TNil -> t1
  | Trail (h) -> cons h t1

(* run_h10 : h -> v -> t -> m -> v *)
let rec run_h10 h v t m = match h with
    Hold (c, s) -> run_c10 c (v :: s) t m
  | Append (h, h') -> run_h10 h v (cons h' t) m

(* run_c10 : c -> s -> t -> m -> v *)
and run_c10 c s t m = match (c, s) with
    ([], v :: []) ->
    begin match t with
        TNil ->
        begin match m with
            [] -> v
          | (c, s, t) :: m -> run_c10 c (v :: s) t m
        end
      | Trail (h) -> run_h10 h v TNil m
    end
  | (INum (n) :: c, VEnv (vs) :: s) -> run_c10 c (VNum (n) :: s) t m
  | (IAccess (n) :: c, VEnv (vs) :: s) -> run_c10 c ((List.nth vs n) :: s) t m
  | (IPush_closure (c') :: c, VEnv (vs) :: s) ->
    run_c10 c (VFun (c', vs) :: s) t m
  | (IReturn :: _, v :: VK (c) :: s) -> run_c10 c (v :: s) t m
  | (IPush_env :: c, VEnv (vs) :: s) ->
    run_c10 c (VEnv (vs) :: VEnv (vs) :: s) t m
  | (IPop_env :: c, v :: VEnv (vs) :: s) -> run_c10 c (VEnv (vs) :: v :: s) t m
  | (IOp (op) :: c, v1 :: v0 :: s) ->
    begin match (v0, v1) with
        (VNum (n0), VNum (n1)) ->
        begin match op with
            Plus -> run_c10 c (VNum (n0 + n1) :: s) t m
          | Minus -> run_c10 c (VNum (n0 - n1) :: s) t m
          | Times -> run_c10 c (VNum (n0 * n1) :: s) t m
          | Divide ->
            if n1 = 0 then failwith "Division by zero"
            else run_c10 c (VNum (n0 / n1) :: s) t m
        end
      | _ -> failwith (to_string v0 ^ " or " ^ to_string v1
                       ^ " are not numbers")
    end
  | (ICall :: c, v1 :: v0 :: s) ->
    begin match v0 with
        VFun (c', vs) -> run_c10 c' (VEnv (v1 :: vs) :: VK (c) :: s) t m
      | VContS (c', s', t') -> run_c10 c' (v1 :: s') t' ((c, s, t) :: m)
      | VContC (c', s', t') ->
        run_c10 c' (v1 :: s') (apnd t' (cons (Hold (c, s)) t)) m
      | _ -> failwith (to_string v0
                       ^ " is not a function; it can not be applied.")
    end
  | (IShift (c') :: c, VEnv (vs) :: s) ->
    run_c10 c' (VEnv (VContS (c, s, t) :: vs) :: []) TNil m
  | (IControl (c') :: c, VEnv (vs) :: s) ->
    run_c10 c' (VEnv (VContC (c, s, t) :: vs) :: []) TNil m
  | (IShift0 (c') :: c, VEnv (vs) :: s) ->
    begin match m with
        (c0, s0, t0) :: m0 ->
        run_c10 (c' @ c0) (VEnv (VContS (c, s, t) :: vs) :: s0) t0 m0
      | _ -> failwith "shift0 is used without enclosing reset"
    end
  | (IControl0 (c') :: c, VEnv (vs) :: s) ->
    begin match m with
        (c0, s0, t0) :: m0 ->
        run_c10 (c' @ c0) (VEnv (VContC (c, s, t) :: vs) :: s0) t0 m0
      | _ -> failwith "control0 is used without enclosing reset"
    end
  | (IReset (c') :: c, VEnv (vs) :: s) ->
    run_c10 c' (VEnv (vs) :: []) TNil ((c, s, t) :: m)
  | _ -> failwith "cont or stack error"

(* f10 : e -> string list -> c *)
let rec f10 e xs = match e with
    Num (n) -> [INum (n)] 
  | Var (x) -> [IAccess (Env.offset x xs)]
  | Op (e0, op, e1) ->
    [IPush_env] @ (f10 e0 xs) @ [IPop_env] @ (f10 e1 xs) @ [IOp (op)]
  | Fun (x, e) -> [IPush_closure ((f10 e (x :: xs)) @ [IReturn])]
  | App (e0, e1) ->
    [IPush_env] @ (f10 e0 xs) @ [IPop_env] @ (f10 e1 xs) @ [ICall]
  | Shift (x, e) -> [IShift (f10 e (x :: xs))]
  | Control (x, e) -> [IControl (f10 e (x :: xs))]
  | Shift0 (x, e) -> [IShift0 (f10 e (x :: xs))]
  | Control0 (x, e) -> [IControl0 (f10 e (x :: xs))]
  | Reset (e) -> [IReset (f10 e xs)]
           
(* f : e -> v *)
let f expr = run_c10 (f10 expr []) (VEnv ([]) :: []) TNil []

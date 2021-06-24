open Syntax
open Value

(* Interpreter with functional instructions : eval8 *)

(* initial continuation : s -> t -> m -> v *)
let idc s t m = match s with
    v :: [] ->
    begin match t with
        TNil ->
        begin match m with
            MNil -> v
          | MCons ((c, s, t), m) -> c (v :: s) t m
        end
      | Trail (h) -> h v TNil m
    end
  | _ -> failwith "stack error"

(* cons : (v -> t -> m -> v) -> t -> t *)
let rec cons h t = match t with
    TNil -> Trail (h)
  | Trail (h') -> Trail (fun v t' m -> h v (cons h' t') m)

(* apnd : t -> t -> t *)
let apnd t0 t1 = match t0 with
    TNil -> t1
  | Trail (h) -> cons h t1

(* (>>) : i -> i -> i *)
let (>>) i0 i1 = fun c s t m -> i0 (fun s' t' m' -> i1 c s' t' m') s t m

(* num : int -> i *)
let num n = fun c s t m -> match s with
    VEnv (vs) :: s -> c (VNum (n) :: s) t m
  | _ -> failwith "stack error"

(* access : int -> i *)
let access n = fun c s t m -> match s with
    VEnv (vs) :: s -> c ((List.nth vs n) :: s) t m
  | _ -> failwith "stack error"

(* push_closure : i -> i *)
let push_closure i = fun c s t m -> match s with
    VEnv (vs) :: s ->
    let vfun = VFun (fun c' s' t' m' ->
        begin match s' with
            v :: s' -> i c' (VEnv (v :: vs) :: s') t' m'
          | _ -> failwith "stack error"
        end) in
    c (vfun :: s) t m
  | _ -> failwith "stack error"

(* return : i *)
let return = fun _ s t m -> match s with
    v :: VK (c) :: s -> c (v :: s) t m
  | _ -> failwith "stack error"

(* push_env : i *)
let push_env = fun c s t m -> match s with
    VEnv (vs) :: s -> c (VEnv (vs) :: VEnv (vs) :: s) t m
  | _ -> failwith "stack error"

(* pop_env : i *)
let pop_env = fun c s t m -> match s with
    v :: VEnv (vs) :: s -> c (VEnv (vs) :: v :: s) t m
  | _ -> failwith "stack error"

(* operations : op -> i *)
let operations op = fun c s t m -> match s with
    v1 :: v0 :: s ->
    begin match (v0, v1) with
        (VNum (n0), VNum (n1)) ->
        begin match op with
            Plus -> c (VNum (n0 + n1) :: s) t m
          | Minus -> c (VNum (n0 - n1) :: s) t m
          | Times -> c (VNum (n0 * n1) :: s) t m
          | Divide ->
            if n1 = 0 then failwith "Division by zero"
            else c (VNum (n0 / n1) :: s) t m
        end
      | _ -> failwith (to_string v0 ^ " or " ^ to_string v1
                       ^ " are not numbers")
    end
  | _ -> failwith "stack error"

(* call : i *)
let call = fun c s t m -> match s with
    v1 :: v0 :: s ->
    begin match v0 with
        VFun (f) -> f idc (v1 :: VK (c) :: s) t m
      | VContS (c', s', t') -> c' (v1 :: s') t' (MCons ((c, s, t), m))
      | VContC (c', s', t') ->
        c' (v1 :: s') (apnd t' (cons (fun v t m -> c (v :: s) t m) t)) m
      | _ -> failwith (to_string v0
                       ^ " is not a function; it can not be applied.")
    end
  | _ -> failwith "stack error"

(* shift : i -> i *)
let shift i = fun c s t m -> match s with
    VEnv (vs) :: s -> i idc (VEnv (VContS (c, s, t) :: vs) :: []) TNil m
  | _ -> failwith "stack error"

(* control : i -> i *)
let control i = fun c s t m -> match s with
    VEnv (vs) :: s -> i idc (VEnv (VContC (c, s, t) :: vs) :: []) TNil m
  | _ -> failwith "stack error"

(* shift0 : i -> i *)
let shift0 i = fun c s t m -> match s with
    VEnv (vs) :: s ->
    begin match m with
        MCons ((c0, s0, t0), m0) ->
        i c0 (VEnv (VContS (c, s, t) :: vs) :: s0) t0 m0
      | _ -> failwith "shift0 is used without enclosing reset"
    end
  | _ -> failwith "stack error"

(* control0 : i -> i *)
let control0 i = fun c s t m -> match s with
    VEnv (vs) :: s ->
    begin match m with
        MCons ((c0, s0, t0), m0) ->
        i c0 (VEnv (VContC (c, s, t) :: vs) :: s0) t0 m0
      | _ -> failwith "control0 is used without enclosing reset"
    end
  | _ -> failwith "stack error"

(* reset : i -> i *)
let reset i = fun c s t m -> match s with
    VEnv (vs) :: s -> i idc (VEnv (vs) :: []) TNil (MCons ((c, s, t), m))
  | _ -> failwith "stack error"

(* f8 : e -> string list -> i *)
let rec f8 e xs = match e with
    Num (n) -> num n
  | Var (x) -> access (Env.offset x xs)
  | Op (e0, op, e1) ->
    push_env >> (f8 e0 xs) >> pop_env >> (f8 e1 xs) >> operations (op)
  | Fun (x, e) -> push_closure ((f8 e (x :: xs)) >> return)
  | App (e0, e1) -> push_env >> (f8 e0 xs) >> pop_env >> (f8 e1 xs) >> call   
  | Shift (x, e) -> shift (f8 e (x :: xs))
  | Control (x, e) -> control (f8 e (x :: xs))
  | Shift0 (x, e) -> shift0 (f8 e (x :: xs))
  | Control0 (x, e) -> control0 (f8 e (x :: xs))
  | Reset (e) -> reset (f8 e xs) 
           
(* f : e -> v *)
let f expr = f8 expr [] idc (VEnv ([]) :: []) TNil MNil

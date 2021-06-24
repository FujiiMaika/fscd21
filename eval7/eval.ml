open Syntax
open Value

(* Interpreter with combined arguments : eval7 *)

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

(* f7 : e -> string list -> c -> s -> t -> m -> v *)
let rec f7 e xs c s t m = match s with
    VEnv (vs) :: s ->
    begin match e with
        Num (n) -> c (VNum (n) :: s) t m 
      | Var (x) -> c ((List.nth vs (Env.offset x xs)) :: s) t m
      | Op (e0, op, e1) ->
        f7 e0 xs (fun s0 t0 m0 ->
            begin match s0 with
                v0 :: VEnv (vs) :: s0 ->
                f7 e1 xs (fun s1 t1 m1 ->
                    begin match s1 with
                        v1 :: v0 :: s1 ->
                        begin match (v0, v1) with
                            (VNum (n0), VNum (n1)) ->
                            begin match op with
                                Plus -> c (VNum (n0 + n1) :: s1) t1 m1
                              | Minus -> c (VNum (n0 - n1) :: s1) t1 m1
                              | Times -> c (VNum (n0 * n1) :: s1) t1 m1
                              | Divide ->
                                if n1 = 0 then failwith "Division by zero"
                                else c (VNum (n0 / n1) :: s1) t1 m1
                            end
                          | _ -> failwith (to_string v0 ^ " or " ^ to_string v1
                                           ^ " are not numbers")
                        end
                      | _ -> failwith "stack error"
                    end) (VEnv (vs) :: v0 :: s0) t0 m0
              | _ -> failwith "stack error"
            end) (VEnv (vs) :: VEnv (vs) :: s) t m
      | Fun (x, e) ->
        let vfun = VFun (fun c' s' t' m' ->
            begin match s' with
                v :: s' -> f7 e (x :: xs) c' (VEnv (v :: vs) :: s') t' m'
              | _ -> failwith "stack error"
            end) in
        c (vfun :: s) t m
      | App (e0, e1) ->
        f7 e0 xs (fun s0 t0 m0 ->
            begin match s0 with
                v0 :: VEnv (vs) :: s0 ->
                f7 e1 xs (fun s1 t1 m1 ->
                    begin match s1 with
                        v1 :: v0 :: s1 ->
                        begin match v0 with
                            VFun (f) -> f c (v1 :: s1) t1 m1
                          | VContS (c', s', t') ->
                            c' (v1 :: s') t' (MCons ((c, s1, t1), m1))
                          | VContC (c', s', t') ->
                            c' (v1 :: s')
                              (apnd t' (cons (fun v t m -> c (v :: s1) t m) t1))
                              m1
                          | _ -> failwith (to_string v0
                               ^ " is not a function; it can not be applied.")
                        end
                      | _ -> failwith "stack error"
                    end) (VEnv (vs) :: v0 :: s0) t0 m0
              | _ -> failwith "stack error"
            end) (VEnv (vs) :: VEnv (vs) :: s) t m
      | Shift (x, e) ->
        f7 e (x :: xs) idc (VEnv (VContS (c, s, t) :: vs) :: []) TNil m
      | Control (x, e) ->
        f7 e (x :: xs) idc (VEnv (VContC (c, s, t) :: vs) :: []) TNil m
      | Shift0 (x, e) ->
        begin match m with
            MCons ((c0, s0, t0), m0) ->
            f7 e (x :: xs) c0 (VEnv (VContS (c, s, t) :: vs) :: s0) t0 m0
          | _ -> failwith "shift0 is used without enclosing reset"
        end
      | Control0 (x, e) ->
        begin match m with
            MCons ((c0, s0, t0), m0) ->
            f7 e (x :: xs) c0 (VEnv (VContC (c, s, t) :: vs) :: s0) t0 m0
          | _ -> failwith "control0 is used without enclosing reset"
        end
      | Reset (e) -> f7 e xs idc (VEnv (vs) :: []) TNil (MCons ((c, s, t), m))
    end
  | _ -> failwith "stack error"
           
(* f : e -> v *)
let f expr = f7 expr [] idc (VEnv ([]) :: []) TNil MNil

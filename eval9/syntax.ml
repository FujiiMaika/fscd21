(* op : binary operator *)
type op = Plus | Minus | Times | Divide

(* op_to_string : op -> string *)
let op_to_string op = match op with
    Plus -> " + "
  | Minus -> " - "
  | Times -> " * "
  | Divide -> " / "

(* e : syntax *)
type e = Num of int
       | Var of string
       | Op of e * op * e
       | Fun of string * e
       | App of e * e
       | Shift of string * e
       | Control of string * e
       | Shift0 of string * e
       | Control0 of string * e
       | Reset of e

(* to_string : e -> string *)
let rec to_string exp = match exp with
    Num (n) -> string_of_int n
  | Var (x) -> x
  | Op (e0, op, e1) ->
    "(" ^ to_string e0 ^ op_to_string op ^ to_string e1 ^ ")"
  | Fun (x, e) ->
    "(fun " ^ x ^ " -> " ^ to_string e ^ ")"
  | App (e0, e1) ->
    "(" ^ to_string e0 ^ " " ^ to_string e1 ^ ")"
  | Shift (x, e) ->
    "(shift " ^ x ^ " -> " ^ to_string e ^ ")"
  | Control (x, e) ->
    "(control " ^ x ^ " -> " ^ to_string e ^ ")"
  | Shift0 (x, e) ->
    "(shift0 " ^ x ^ " -> " ^ to_string e ^ ")"
  | Control0 (x, e) ->
    "(control0 " ^ x ^ " -> " ^ to_string e ^ ")"
  | Reset (e) ->
    "reset (" ^ to_string e ^ ")"

(* print : e -> unit *)
let print exp =
  let str = to_string exp in
  print_string str

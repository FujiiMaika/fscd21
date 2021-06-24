open Syntax

(* Interpreter with linearized instructions : eval10 *)

(* Value *)
type v = VNum of int
       | VFun of c * v list
       | VContS of c * s * t
       | VContC of c * s * t
       | VEnv of v list
       | VK of c

and i = INum of int | IAccess of int
      | IPush_closure of c | IReturn
      | IPush_env | IPop_env | IOp of op | ICall
      | IShift of c | IControl of c
      | IShift0 of c | IControl0 of c
      | IReset of c

and c = i list

and s = v list

and h = Hold of c * s
      | Append of h * h

and t = TNil | Trail of h

type m = (c * s * t) list


(* to_string : v -> string *)
let rec to_string value = match value with
    VNum (n) -> string_of_int n
  | VFun (_) -> "<VFun>"
  | VContS (_) -> "<VContS>"
  | VContC (_) -> "<VContC>"
  | VEnv (_) -> "<VEnv>"
  | VK (_) -> "<VK>"

(* Value.print : v -> unit *)
let print exp =
  let str = to_string exp in
  print_string str

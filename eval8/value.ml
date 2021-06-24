(* Interpreter with functional instructions : eval8 *)

(* Value *)
type v = VNum of int
       | VFun of (c -> s -> t -> m -> v)
       | VContS of c * s * t
       | VContC of c * s * t
       | VEnv of v list
       | VK of c

and c = s -> t -> m -> v

and s = v list

and t = TNil | Trail of (v -> t -> m -> v)

and m = MNil
      | MCons of (c * s * t) * m

type i = c -> s -> t -> m -> v


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

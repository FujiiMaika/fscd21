{
open Parser
}

(* Omission of regular expression *)
let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper

rule token = parse
| space+ { token lexbuf }       (* Skip spaces *)
| "(*" [^ '\n']* "\n"           (* comment from '( *'  *)
         { token lexbuf }
| "("    { LPAREN }
| ")"    { RPAREN }
| "+"    { PLUS }
| "-"    { MINUS }
| "*"    { TIMES }
| "/"    { DIVIDE }
| "fun"  { FUN }
| "->"   { ARROW }
| "shift" { SHIFT }
| "control" { CONTROL }
| "shift0" { SHIFT0 }
| "control0" { CONTROL0 }
| "reset" { RESET }
| digit +
         { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
| lower (alpha | digit) *
         { VAR (Lexing.lexeme lexbuf) }
| eof    { EOF }                (* End of file *)
| _      { failwith ("unknown token: " ^ Lexing.lexeme lexbuf) }

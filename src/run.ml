open Ast;;
open Eval;; 



let fname = Sys.argv.(1) in

let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
    Eval.eval p
  with e->
    Printf.printf "\tFatal Error : %s\n" (Printexc.to_string e);
    exit 0

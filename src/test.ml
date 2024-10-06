open Ast ;;


(* let var_x :Ast.pterm = Var ("x");;
let abs_ident :Ast.pterm = Abs ("x", var_x);;
let abs_nimp :Ast.pterm = Abs ("x", Var("y"));;
let app_identite_x :Ast.pterm = App (abs_ident, var_x);;
let var_y_renommer : Ast.pterm  = alpha_conv (Var("Y")) [] ;; 
let abs_k : Ast.pterm = 
  Abs(
    "X",
    Abs(
      "Y", 
      Var("X")
    )
  )
;;

let abs_k_renamed_x : Ast.pterm = alpha_conv (abs_k) [];; 

let abs_func : Ast.pterm = 
  Abs(
    "X",
    Abs(
      "Y", 
      App(Var("X"),Var("Y"))
    )
  )
;;
let abs_func_renamed : Ast.pterm =  alpha_conv (abs_func) [];; *)

let someFuncToReplaceVariable = Abs(
  "X",
  Abs(
    "Y",
    App(Var("X"),Var("Y"))
  )
);;
let varToReplace = "X";;
let ntermToReplace = Abs("N",Abs("Z",App (Var("N"),Var("Z"))));;
let substitued = Ast.substitution varToReplace ntermToReplace someFuncToReplaceVariable;;

let main () = 
  (* print_pterm var_x ;
  print_pterm abs_ident ;
  print_pterm abs_nimp ;
  print_pterm app_identite_x ;
  print_pterm var_y_renommer;
  Printf.printf"Before Renaming :  \n";
  print_pterm abs_k;
  Printf.printf"After Renaming :  \n";
  print_pterm abs_k_renamed_x;
  Printf.printf"Before Renaming :  \n";
  print_pterm abs_func;
  Printf.printf"After Renaming :  \n";
  print_pterm abs_func_renamed;*)
  Printf.printf"Before Function  subs :  \n"; 
  print_pterm someFuncToReplaceVariable;
  Printf.printf"After Function  subs :  \n";
  print_pterm substitued;
;;

let _ = main ();;
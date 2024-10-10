open Ast ;;


let var_x :Ast.pterm = Var ("x");;
let identity :Ast.pterm = Abs ("x", var_x);;
let var_y_renamed : Ast.pterm  = alpha_conv (Var("Y")) [] ;; 
let k : Ast.pterm = Abs("X",Abs("Y", Var("X")));;

let k_renamed : Ast.pterm = alpha_conv (k) [];; 

let abs_func : Ast.pterm = Abs("X",
    Abs("Y", App(Var("X"),Var("Y")))
  )
;;

let varToReplace = "X";;
let ntermToReplace = Abs("N",Abs("Z",App (Var("N"),Var("Z"))));;
let abs_func_substitued_by_substitued_v2 = Ast.substitution_v2 varToReplace ntermToReplace abs_func;;

let t1 = Abs ("x", App (Var "x", Var "y"));;
let free_vars_t1 = free_vars t1;;
let t2 = Var "x";;
let sub_t2_in_t1_result_v2 = substitution_v2 "y" t2 t1;;

let example_II = App (Abs ("x", Var "x"), Abs ("y", Var "y"));;
(* Définition des combinatoires K et S *)
let combinator_k = Abs ("x", Abs ("y", Var "x"))
let combinator_s = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))

let example_skk = App (App (combinator_s, combinator_k), combinator_k);; 
let main () = 
  Printf.printf"Variable Y  renammed:  ";
  print_pterm var_y_renamed;
  Printf.printf" K :  ";
  print_pterm k;
  Printf.printf" \t -> ";
  print_pterm k_renamed;
  Printf.printf"Subs : "; 
  print_pterm abs_func;
  Printf.printf"\t  -> ";
  print_pterm abs_func_substitued_by_substitued_v2;
  (* print_pterm t1;
  Printf.printf "Free variable list for t1 : "; 
  print_string_list free_vars_t1; *)
  Printf.printf "\n--- Substition v2 ---\n";
  print_pterm sub_t2_in_t1_result_v2;
  Printf.printf "\n--- Réduction de l'exemple II ---\n";
  print_reduction_steps example_II;
   (* Exemple SKK *)
  Printf.printf "\n--- Réduction de l'exemple SKK ---\n";
  print_reduction_steps example_skk;
   (* Normalisation avec timeout *)
  Printf.printf "\n--- Normalisation avec timeout ---\n";
  (match ltr_cbv_norm_timeout example_skk 10 with
  | Some nf -> 
      Printf.printf "Forme normale (avec timeout): ";
      print_pterm nf
  | None -> 
      Printf.printf "Divergence détectée (limite de réduction atteinte).\n"
  )
;;

let _ = main ();;
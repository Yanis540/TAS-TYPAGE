open Ast;; 
open Type;; 

(* ! génération des équations *)

(* Exemple : terme Var "x" *)

(* let env_ex = [("x", VarType "int")];;
let test_term = App(Abs("x", Var "x"), Var "x");;
let eqs = generate_equa test_term (VarType "T2") env_ex;;
(* ! Occur Check  *)

let t1 = Arrow (VarType "T1", VarType "T2");;  (* T1 -> T2 *)
let t2 = Arrow (VarType "T1", Arrow (VarType "T1", VarType "T3"));;  (* T1 -> (T1 -> T3) *)
let t3 = Nat;;
let occur_check_examples = [
  (true,"T1",t1);
  (false,"T3",t1);
  (true,"T1",t2);
  (false,"T1",t3)
]
let occur_check_test (e) = 
  let (expected_res,v ,t) = e in 
  let res = occur_check v t in 
  Printf.printf "Occur check %s in (%s): %b expected %b \n" (v) (ptype_to_string t) (res) (expected_res)
;;
(* ! Substituion  *)

let t = Arrow (VarType "T1", Arrow (VarType "T2", Nat));;
let t_substitue = substitute_type "T1" Nat t;;
let system_subs_equas :equas= [(VarType "T1", Nat); (Arrow (VarType "T1", VarType "T2"), VarType "T3")];;
let system_substitued_equas = substitute_in_system "T1" Nat system_subs_equas;;
let system_substitued_equas_2 = substitute_in_system "T2" (VarType "int") system_substitued_equas;; *)

(* ! Unification  *)
let system_unification = [
  (VarType "T1", Nat);
  (Arrow (VarType "T2", VarType "T1"), Arrow (Nat, Nat));
  (VarType "T3", Arrow (Nat, VarType "T2"))
];;
let (system_unification_equas,resolution_env) = unify_step system_unification [];;
let term = Abs ("x", Var "x");;

(* ! Résolution du système  *)
let _ =
  (* Printf.printf "\n--- Génération d'équations ---\n";
  Printf.printf "Terme :  " ;
  Ast.print_pterm test_term; 
  Printf.printf "Equations générées: %s\n" (equas_to_string eqs); 
  Printf.printf "\n--- Occur Check ---\n";
  List.iter occur_check_test occur_check_examples; 
  Printf.printf "\n--- Substitution de type  ---\n";
  print_ptype t_substitue; 
  Printf.printf "\n--- Substitution de Système  ---\n";
  Printf.printf "Système avant substitution : %s\n" (equas_to_string system_subs_equas);
  Printf.printf "Système après substitution : %s\n" (equas_to_string system_substitued_equas);
  Printf.printf "Système après 2 substitution : %s\n" (equas_to_string system_substitued_equas_2);  *)
  Printf.printf "\n--- Unificatiton de Système  ---\n";
  Printf.printf "Système avant unification : %s\n" (equas_to_string system_unification); 
  Printf.printf "Système après unification : %s\n" (equas_to_string system_unification_equas); 
  print_env resolution_env;
  Printf.printf "\n--- Unificatiton de Système  ---\n";
  (* Inférence du type avec un timeout de 2 secondes *)
  Printf.printf "Le type inféré est : %s\n" (ptype_to_string (infer_type_ term))
;; 
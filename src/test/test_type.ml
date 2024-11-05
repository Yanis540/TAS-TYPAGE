open Ast;; 
open Type;; 
open Eval;; 


(* ! génération des équations *)

(* Exemple : terme Var "x" *)

 let env_ex = [("x", VarType "int")];;
let test_term = App(Abs("x", Var "x"), Var "x");;
let eqs,_ = generate_equa test_term (VarType "T2") env_ex;;
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
let system_substitued_equas_2 = substitute_in_system "T2" (VarType "int") system_substitued_equas;; 

(* ! Unification  *)
let system_unification = [
  (VarType "T1", Nat);
  (Arrow (VarType "T2", VarType "T1"), Arrow (Nat, Nat));
  (VarType "T3", Arrow (Nat, VarType "T2"))
];;
let (system_unification_equas,resolution_env) = unify_step system_unification [];;


let test_unify_step (name, eqs) =
  Printf.printf "\n--- Test d'Unification : %s ---\n" name;
  (* Appel de unify_step avec des substitutions initiales vides *)
  try
    let (eqs_result, substitutions) = unify_step eqs [] in
    Printf.printf "Système unifié : %s\n" (equas_to_string eqs_result);
    Printf.printf "Substitutions :\n";
    List.iter (fun (v, t) -> Printf.printf "  %s = %s\n" v (ptype_to_string t)) substitutions
  with Failure msg ->
    Printf.printf "Erreur d'unification : %s\n" msg
;;

let unify_tests = [
  ("Identité", [(VarType "T1", Arrow (VarType "T2", VarType "T2"))]);
  ("Simple Flèche", [(Arrow (VarType "T1", VarType "T2"), Arrow (VarType "T2", VarType "T3"))]);
  ("Variable à Gauche", [(VarType "X", Arrow (Nat, Nat))]);
  ("Variable à Droite", [(Arrow (Nat, Nat), VarType "X")]);
  ("Deux flèches", [(Arrow (VarType "T1", VarType "T2"), Arrow (VarType "T3", VarType "T4"))]);
];;

(* ! Inference  *)

let term = Abs ("x", Var "x");;


let test_infer_type (name, term, env) =
  Printf.printf "\n--- Test d'Inférence de Type : %s ---\n" name;
  (* Tenter d'inférer le type avec un timeout de 2 secondes *)
  match infer_type term env 2.0 with
  | Some (ty,_) -> Printf.printf "Le type inféré est : %s\n" (ptype_to_string ty)
  | None -> Printf.printf "Erreur : le terme n'est pas typable ou timeout atteint.\n"
;;

(* Quelques termes pour tester l'inférence de type *)
let x_term = Var "x";;
let id_term = Abs ("x", Var "x");;
let app_term = App (Abs ("x", Var "x"), Var "y");;

(* Quelques environnements *)
let env1 = [("x", VarType "T1")];;
let env2 = [("x", Arrow (Nat, Nat)); ("y", Nat)];;

let infer_tests = [
  ("Variable seule", x_term, env1);
  ("Identité", id_term, []);
  ("Application", app_term, env2);
];;

(* ! Résolution du système  *)
let _ =
  Printf.printf "\n--- Génération d'équations ---\n";
  Printf.printf "Terme :  " ;
  print_pterm test_term; 
  Printf.printf "Equations générées: %s\n" (equas_to_string eqs); 
  Printf.printf "\n--- Occur Check ---\n";
  List.iter occur_check_test occur_check_examples; 
  Printf.printf "\n--- Substitution de type  ---\n";
  print_ptype t_substitue; 
  Printf.printf "\n--- Substitution de Système  ---\n";
  Printf.printf "Système avant substitution : %s\n" (equas_to_string system_subs_equas);
  Printf.printf "Système après substitution : %s\n" (equas_to_string system_substitued_equas);
  Printf.printf "Système après 2 substitution : %s\n" (equas_to_string system_substitued_equas_2); 
  Printf.printf "\n--- Unificatiton de Système  ---\n";
  Printf.printf "Système avant unification : %s\n" (equas_to_string system_unification); 
  Printf.printf "Système après unification : %s\n" (equas_to_string system_unification_equas); 
  print_env resolution_env;
  Printf.printf "\n--- Unificatiton de Système  ---\n";
  (* Inférence du type avec un timeout de 2 secondes *)
  Printf.printf "Le type inféré est : %s\n" (ptype_to_string (infer_type_ term));
  Printf.printf "\n--- Unificatiton examples de Système  ---\n";
  List.iter test_unify_step unify_tests;
  Printf.printf "\n--- Inférence examples de Système  ---\n";
  List.iter test_infer_type infer_tests;
;; 


(* !  *)
let t = Forall(["X"; "Y"], Arrow(VarType "X", VarType "Y"));;

let _ = 
  print_ptype t;;
open Ast ;;


let var_x :Ast.pterm = Var ("x");;
let identity :Ast.pterm = Abs ("x", var_x);;
let var_y_renamed : Ast.pterm  = alpha_conv (Var("Y")) [] ;; 
let abs_func : Ast.pterm = Abs("X",Abs("Y", App(Var("X"),Var("Y"))))
;;
let varToReplace = "X";;
let ntermToReplace = Abs("N",Abs("Z",App (Var("N"),Var("Z"))));;
let abs_func_substitued_by_substitued_v2 = Ast.substitution varToReplace ntermToReplace abs_func;;

let t1 = Abs ("x", App (Var "x", Var "y"));;
(* let free_vars_t1 = free_vars t1;; *)
let t2 = Var "x";;
let sub_t2_in_t1_result_v2 = substitution "y" t2 t1;;

let example_II = App (Abs ("x", Var "x"), Abs ("y", Var "y"));;
(* Définition des combinatoires K et S *)
let combinator_k = Abs ("x", Abs ("y", Var "x"))
let combinator_s = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
(* Combinateurs combinés *)
let combinator_i = Abs ("x", Var "x") ;;
let combinator_k = Abs ("x", Abs ("y", Var "x")) ;;
let combinator_s = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z"))))) ;;
let combinator_delta = Abs ("x", App (Var "x", Var "x")) ;;
let combinator_omega = App (combinator_delta, combinator_delta) ;;  (* Ω = (λx.x x) (λx.x x) *)
let combinator_skK = App (App (combinator_s, combinator_k), combinator_k) ;;
let combinator_sI = App (App (combinator_s, combinator_i), combinator_i) ;;
let example_skk = App (App (combinator_s, combinator_k), combinator_k);; 

(* Nombres de Church *)
let church_zero = Abs ("f", Abs ("x", Var "x")) ;;
let church_one = Abs ("f", Abs ("x", App (Var "f", Var "x"))) ;;
let church_two = Abs ("f", Abs ("x", App (Var "f", App (Var "f", Var "x")))) ;;
let church_three = Abs ("f", Abs ("x", App (Var "f", App (Var "f", App (Var "f", Var "x"))))) ;;

(* Opérations Arithmétiques *)
let succ = Abs ("n", Abs ("f", Abs ("x", App (Var "f", App (App (Var "n", Var "f"), Var "x"))))) ;;
let plus_ = Abs ("m", Abs ("n", Abs ("f", Abs ("x", App (App (Var "m", Var "f"), App (App (Var "n", Var "f"), Var "x")))))) ;;
let mult = Abs ("m", Abs ("n", Abs ("f", App (Var "m", App (Var "n", Var "f"))))) ;;
(* Fonction qui convertit un entier en notation Church en un entier OCaml *)
let church_to_int (church_num: pterm) : int =
  match church_num with
  | Abs (_, Abs (_, body)) -> (
      let rec eval n = match n with
        | Var "x" -> 0
        | App (Var "f", t) -> 1 + eval t
        | _ -> failwith "Invalid Church numeral"
      in eval body
    )
  | _ -> failwith "Not a valid Church numeral"
;;

let examples = [
  ("I", combinator_i);
  ("K", combinator_k);
  ("S", combinator_s);
  (* ("delta", combinator_delta); *)
  (* ("Omega", combinator_omega); *)
  ("S K K", combinator_skK);
  ("S I I", combinator_sI);
  ("Church 0", church_zero);
  ("Church 1", church_one);
  ("Church 2", church_two);
  ("Church 3", church_three);
  ("Successeur", succ);
  ("Addition", plus_);
  ("Multiplication", mult);
];; 

let test_normalization (name, term) =
  Printf.printf "\n--- Test: %s ---\n" name;
  (* Tenter de normaliser avec un timeout de 10 étapes *)
  match ltr_cbv_norm_timeout term 1.0 with
  | Some nf -> 
      Printf.printf "Forme normale (avec timeout): %s\n" (pterm_to_string nf)
  | None -> 
      Printf.printf "Divergence détectée (limite de réduction atteinte).\n"
;;
(* Fonction pour tester et afficher le résultat des opérations arithmétiques *)
let test_arithmetic (name, term) =
  Printf.printf "\n--- Test Arithmétique: %s ---\n" name;
  (* Tenter de normaliser avec un timeout de 1 seconde *)
  match ltr_cbv_norm_timeout term 1.0 with
  | Some nf -> (
      (* Printf.printf "Forme normale (avec timeout): %s\n" (pterm_to_string nf); *)
      try
        let i = church_to_int nf in
        Printf.printf "Résultat en entier: %d\n" i
      with Failure msg ->
        Printf.printf "Erreur : %s\n" msg
    )
  | None -> 
      Printf.printf "Divergence détectée (timeout de 1 seconde atteinte).\n"
;;
(* Encodage de l'addition de 1 et 2: plus 1 2 *)
let plus_one_two = App (App (plus_, church_one), church_two) ;;

(* Encodage de la multiplication de 2 et 3: mult 2 3 *)
let mult_two_three = App (App (mult, church_two), church_three) ;;

(* Encodage de successeur de 2: succ 2 *)
let succ_two = App (succ, church_two) ;;

(* Encodage de 3 + 2: plus 3 2 *)
let plus_three_two = App (App (plus_, church_three), church_two) ;;

(* Encodage de 2 * (plus 1 3): mult 2 (plus 1 3) *)
let mult_two_plus_one_three = App (App (mult, church_two), App (App (plus_, church_one), church_three)) ;;

(* Liste des tests arithmétiques *)
let arithmetic_tests = [
  ("plus 1 2", plus_one_two);
  ("mult 2 3", mult_two_three);
  ("succ 2", succ_two);
  ("plus 3 2", plus_three_two);
  ("mult 2 (plus 1 3)", mult_two_plus_one_three);
] ;;

let main () = 

  Printf.printf"Subs : "; 
  print_pterm abs_func;
  Printf.printf"\t  -> ";
  print_pterm abs_func_substitued_by_substitued_v2;
  (* print_pterm t1;
  Printf.printf "Free variable list for t1 : "; 
  print_string_list free_vars_t1; *)
  Printf.printf "\n--- Substition  ---\n";
  print_pterm sub_t2_in_t1_result_v2;
  Printf.printf "\n--- Réduction de l'exemple II ---\n";
  print_reduction_steps example_II;
   (* Exemple SKK *)
  Printf.printf "\n--- Réduction de l'exemple SKK ---\n";
  print_reduction_steps example_skk;
   (* Normalisation avec timeout *)
  Printf.printf "\n--- Normalisation avec timeout ---\n";
  (match ltr_cbv_norm_timeout example_skk 1.0 with
  | Some nf -> 
    Printf.printf "Forme normale (avec timeout): ";
    print_pterm nf
    | None -> 
      Printf.printf "Divergence détectée (limite de réduction atteinte).\n"
  );
  Printf.printf "\n--- Normalisation avec timeout ---\n";
  (* Exécuter les tests pour chaque exemple *)
  List.iter test_normalization examples;
  (* Exécuter les tests pour chaque exemple *)
  List.iter test_arithmetic arithmetic_tests
;;

let _ = main ();;
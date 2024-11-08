# Rendu 
Projet de Typage présenté par Yanis Tabellout pour le Master 2 STL sorbonne université

# Structure de fichier 
```
└───src
    ├───archive
    ├───examples
    ├───exec
    ├───ast.ml : Ast du lambda calcul 
    ├───eval.ml : code de l'évaluateur
    ├───type.ml : code du typeur
    ├───run.ml : code annexe pour lancer l'évaluteur avec le lexer/parser
    └───test
```

# Parties 
## 5 premières parties
toutes les parties du projet ont été implémentées notamment la partie du polymorphisme faible. J'ai implémenté un pour le lambda calcul aussi comme extension.
## Extension 
J'ai ajouté comme extension : 
- **lexer/parser** du lambda calcul  
- **Types Sommes**  

# Éxecution 
##  Tests
dans un terminal lancez : 
```shell 
wsl # si vous êtes sur windows
cd src 
mkdir exec
make test 
./exec/<Nom_du_test>
```

##  Evaluation du lexer/parser

dans un terminal lancez : 
```shell 
./run-parser.sh
```
Vous aurez les résultats de de touts les exemples

# Problème rencontré  
Malgré que j'ai implémenté le polymorphisme faible (Weak) mais il y'a un petit soucis que je n'arrive pas à régler c'est l'exemple suivant : 
```Ocaml
let l := [] in let _ = (l:=[]) in (hd (l)) + 2
```
et aussi 
```Ocaml
let l := [] in let _ = (l:=[]) in (hd (l)) + 2
```

qui passe dans le typeur mais qui ne sont pas sensé passé, j'ai passé la semaine à essayer de le corriger ça ne sert à rien de le débeuger encore plus. 
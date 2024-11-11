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
- **Weak** : Le weak marche bien (j'ai passé énormément de temps )

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

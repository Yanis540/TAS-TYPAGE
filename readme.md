# Rendu 
Projet de Typage présenté par Yanis Tabellout pour le Master 2 STL sorbonne université. 
Disclaimer pour le prof, certaines parties du typeur a été implémenté grâce à chatgpt.

# Structure de fichier 
```
└───src
    ├───archive
    ├───examples
    ├───exec
    ├───test
    ├───ast.ml : Ast du lambda calcul 
    ├───eval.ml : code de l'évaluateur
    ├───type.ml : code du typeur
    ├───run.ml : code annexe pour lancer l'évaluteur avec le lexer/parser
    └───run-parser.sh : script shell pour afficher les résultats des exemples
```


# Parties 
## 5 premières parties
toutes les parties du projet ont été implémentées notamment la partie du polymorphisme faible. J'ai implémenté un pour le lambda calcul aussi comme extension.
### Évaluteur
-   L'évaluateur implémente la stratégie : **Left To Right Call By Value (ltr-Cbv)**. 
-   Pour la gestion de mémoire, j'ai fait le choix de représenter la mémoire par une liste de couple (nom, valeur). 
-   Comme en APS, la mémoire peut être modifier que par l'opérateur **Assign** qui représente une règle dans notre évaluteur, 
mais par exemple une beta réduction ne modifie pas, par contre le côté gauche ou à droite d'une application peut le modifier e.g : 
```Ocaml 
let x = ref [] in let _ = x:=[1] in hd(!x)  (*s'évalue à 1*)
```

### Typeur 
-   les types du lambda calcul sont représentés par le type **ptype**. 
-   Les équations sont réprésentées par une liste de couple **(ptype,ptype)**. 
-   J'ai ajouté un environnement de typage qui est une liste de couple **(nom, ptype)**

## Extension 
J'ai ajouté comme extension : 
- **lexer/parser** du lambda calcul  
- **Types Sommes**  
- **Weak** : Le weak marche bien (j'ai passé énormément de temps et c'est normal x) )
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
Vous aurez les résultats de de touts les exemples sur le terminal, vous pouvez rediriger la sortie standard vers un fichier pour mieux visualiser le résultat

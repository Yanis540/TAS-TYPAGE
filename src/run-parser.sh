make run 

for file in "./examples"/*; do 
    echo -e "Traitement du fichier $file  ..." 
    res_eval=$(./exec/run "$file" 2>&1)
    echo "$res_eval"
done 
make run 

for file in "./examples"/*; do 
    name=$(echo "$file" | sed -E 's|.*/([^_]+)_([0-9]+)\.lambda$|\1|' | sed 's/^./\U&/')
    number=$(echo "$file" | sed -E 's|.*/([^_]+)_([0-9]+)\.lambda$|\2|')
    echo -e "Traitement :  $name $number  \n" 
    res_eval=$(./exec/run "$file" 2>&1)
    echo -e "$res_eval \n"
done 
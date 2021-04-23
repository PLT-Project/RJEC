#!/bin/bash

#latex script by TA Justin from PLT Fall 2018
#sourced from Shoo
TARGET='./latex_compiler.txt'

echo "" > $TARGET

declare -a FILES
FILES=("rjec.ml" "scanner.mll" "ast.ml" "rjecparse.mly" "sast.ml" "semant.ml" "codegen.ml" "concurrency.c" "Makefile" "buildlibmill.sh" "rjec.sh" "testall.sh")


for f in ${FILES[@]}
do
   echo $f
   echo '\subsection{'"$f"'}' >> $TARGET
   echo '\begin{mdframed}[hidealllines=true,backgroundcolor=blue!20]' >> $TARGET
   echo '\begin{lstlisting}'  >> $TARGET  
   cat $f >> $TARGET
   echo '\end{lstlisting}' >> $TARGET
   echo '\end{mdframed}' >> $TARGET
done



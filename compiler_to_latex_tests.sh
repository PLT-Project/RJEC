#!/bin/bash

#justin (PLT Fall 2018 TA)'s latex script.
#Modified by Claire Adams of Shoo for tests
#Modified by Justin Chen for RJEC
#cd demo/
cd tests/
TARGET='../latex_compiler.txt'

echo "" > $TARGET

declare -a FILES
FILES=("*")

for f in ${FILES[@]}
do
   echo $f
   echo '\subsubsection{'"$f"'}' >> $TARGET
   if [ ${f: -5} == ".shoo" ]
   then
    echo '\begin{mdframed}[hidealllines=true,backgroundcolor=blue!10]' >> $TARGET
    echo '\begin{lstlisting}'  >> $TARGET  
    cat $f >> $TARGET
   else
    echo '\begin{mdframed}[hidealllines=true,backgroundcolor=green!10]' >> $TARGET
    echo '\begin{lstlisting}'  >> $TARGET  
    cat $f >> $TARGET
   fi
   
   echo '\end{lstlisting}' >> $TARGET
   echo '\end{mdframed}' >> $TARGET
done



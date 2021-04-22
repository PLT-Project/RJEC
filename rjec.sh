#!/bin/sh

# adapted from testall.sh

# Path to the LLVM interpreter
LLI="lli"

# Path to the LLVM compiler
LLC="llc"

# Path to the C compiler
CC="cc"

# Path to the rjec compiler.  Usually "./rjec.native"
# Try "_build/rjec.native" if ocamlbuild was unable to create a symbolic link.
RJEC="./rjec.native"

if [ -z "$1" ]
  then
    echo "Usage: ./rjec.sh <filename.rjec>"
    exit 1
fi

keep=0

if [ -z "$LD_LIBRARY_PATH" ]
then
    LD_LIBRARY_PATH=$(pwd)/libmill
    export LD_LIBRARY_PATH
    echo "export LD_LIBRARY_PATH=$(pwd)/libmill"
else
    echo "check that LD_LIBRARY_PATH is exported!"
fi

basename=`echo $1 | sed 's/.*\\///
                             s/.rjec//'`

generatedfiles=""

generatedfiles="$generatedfiles ${basename}.ll ${basename}.s ${basename}.exe ${basename}.out" &&
eval "$RJEC" "$1" ">" "${basename}.ll" &&
eval "$LLC" "-relocation-model=pic" "${basename}.ll" ">" "${basename}.s" &&
eval "$CC" "-o" "${basename}.exe" "${basename}.s" "printbool.o" "concurrency.o" "-L" "libmill/" "-lmill" &&
eval "./${basename}.exe"

if [ $keep -eq 0 ] ; then
    rm -f $generatedfiles
fi

LDFLAGS = -L libmill/
LDLIBS = -lmill

# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

# "make all" builds the executable as well as the "printbig" library designed
# to test linking external code

.PHONY : all
all : rjec.native printbool.o libmill/libmill.a concurrency.o

# builds the libmill library used for concurrency in RJEC
libmill/libmill.a :
	./buildlibmill.sh

# "make rjec.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

rjec.native :
	opam config exec -- \
	ocamlbuild -X libmill/ -use-ocamlfind rjec.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff
	rm *.o *.ll *.s *.exe

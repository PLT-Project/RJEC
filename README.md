# RJEC
Really Just Elementary Concurrency

By Riya Chakraborty, Justin Chen, Yuanyuting Wang, Caroline Hoang  
COMS 4115 Programming Languages & Translators  
Columbia University

RJEC is a Go-like language with CSP-style concurrency.

## Build instructions
We currently only support installation via Docker.

To install, download or clone the repo and run the following command:
```
docker run --rm -it -v `pwd`:/home/microc -w=/home/microc columbiasedwards/plt
```

You may also achieve similar results by manual installation under Ubuntu 18.04, but this is not supported.

To build, run `make all`.

## Running tests
Tests can be run by running `make`, which will run `./testall.sh`.

## Compiling RJEC programs
You can compile a run a RJEC program using `./rjec.sh`. For intermediate compilation steps, please reference this file.
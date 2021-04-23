#!/bin/sh

# builds the libmill library used for concurrency in RJEC
# written by Justin Chen

cd libmill
cmake --configure .
cmake --build .
cd ..
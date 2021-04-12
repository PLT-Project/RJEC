#!/bin/sh

# builds the libmill library used for concurrency in RJEC

cd libmill
cmake --configure .
cmake --build .
cd ..
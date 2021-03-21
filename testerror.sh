#!/bin/sh

# get error output of single program
eval $1
retval=$?
if [ $retval -ne 0 ]; then
    echo "Return code was not zero but $retval"
fi
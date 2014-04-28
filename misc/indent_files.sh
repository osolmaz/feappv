#!/bin/bash

# Script to auto indent all the source files
# Was useful when porting from Fortran 77 to 90

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

for i in $(find "$DIR"/../ -name '*.f90'); do
  echo Indenting file $i
  emacs -batch $i -l "$DIR"/indent.el -f emacs-format-function
done

#!/bin/bash -e

PATH=.cabal-sandbox/bin:$PATH
for i in {100,300} ; do
  random-sym --total-vars=80 --vars-in-sym=25 --sym-size=${i}000 \
    | sort -ru > ${i}k.sym
  cat ${i}k.sym \
    | sym-to-cnf --total-vars=80 --method=tseitin +RTS -K20M -s \
    > ${i}k.cnf
done

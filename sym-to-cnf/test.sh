#!/bin/bash -e

cabal sandbox init
cabal install

PATH=.cabal-sandbox/bin:$PATH

for i in {1..4} ; do
  random-sym --total-vars=64 --vars-in-sym=25 --sym-size=50000 \
    | sort -ru > test${i}.sym
done

cat test{1..4}.sym \
  | sym-to-cnf --total-vars=64 --method=tseitin +RTS -K20M \
  > test.cnf

minisat test.cnf test.sat || echo

check-res test.sat test{1..4}.sym



# for i in {100,300} ; do
#   random-sym --total-vars=80 --vars-in-sym=25 --sym-size=${i}000 \
#     | sort -ru > ${i}k.sym
#   cat ${i}k.sym \
#     | sym-to-cnf --total-vars=80 --method=tseitin +RTS -K20M -s \
#     > ${i}k.cnf
# done

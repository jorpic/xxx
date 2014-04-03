#!/bin/bash -e

cabal sandbox init
cabal install

PATH=.cabal-sandbox/bin:$PATH

rm -rf .test
mkdir .test

for i in {1..4} ; do
  random-sym --total-vars=64 --vars-in-sym=25 --sym-size=50000 \
    | sort -ru > .test/test${i}.sym
  wc -l .test/test${i}.sym
done

time sym-to-cnf --max-var=64 --method=bdd \
  .test/test*.sym +RTS -K20M > .test/test.cnf
wc -l .test/test.cnf

minisat .test/test.cnf .test/test.sat || echo
check-res .test/test.sat .test/test*.sym



# for i in {100,300} ; do
#   random-sym --total-vars=80 --vars-in-sym=25 --sym-size=${i}000 \
#     | sort -ru > ${i}k.sym
#   cat ${i}k.sym \
#     | sym-to-cnf --max-var=80 --method=tseitin +RTS -K20M -s \
#     > ${i}k.cnf
# done

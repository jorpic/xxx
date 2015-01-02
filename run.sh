#!/bin/bash -e


if [[ $# -eq 0 ]] ; then
  echo 'Usage: ./run.sh dag inp.template out.template inp.test out.test'
  exit 0
fi


DAG=$1
INP_TEMPLATE=$2
OUT_TEMPLATE=$3
INP_TEST=$4
OUT_TEST=$5


PATH=".cabal-sandbox/bin:$PATH"

# rm -rf .test
# mkdir .test

echo == Eval DAG and record trace
time dag-tools eval-check $INP_TEST $OUT_TEST < $DAG > .test/trace.vals

echo == Fix inputs from template
time dag-tools fix-inputs $INP_TEMPLATE < $DAG > .test/fixed.dag
wc -l $DAG
wc -l .test/fixed.dag

echo == Check fixed DAG
time dag-tools eval-check $INP_TEST $OUT_TEST < .test/fixed.dag > /dev/null

echo == Convert to poly system
time dag-tools to-system $OUT_TEMPLATE < .test/fixed.dag > .test/system0.eqs
wc -l .test/system0.eqs

echo == Check system
time system-tools check .test/trace.vals < .test/system0.eqs

echo == Delinearize system
time system-tools delinearize < .test/system0.eqs > .test/system1.eqs
wc -l .test/system1.eqs
time system-tools delinearize < .test/system1.eqs > .test/system2.eqs
wc -l .test/system2.eqs
time system-tools delinearize < .test/system2.eqs > .test/system3.eqs
wc -l .test/system3.eqs
time system-tools delinearize < .test/system3.eqs > .test/system4.eqs
wc -l .test/system4.eqs
time system-tools delinearize < .test/system4.eqs > .test/system5.eqs
wc -l .test/system5.eqs
time system-tools delinearize < .test/system5.eqs > .test/system6.eqs
wc -l .test/system6.eqs

echo == Check transformed system
time system-tools check .test/trace.vals < .test/system6.eqs

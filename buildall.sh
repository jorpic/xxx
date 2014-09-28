#!/bin/bash -e

function install {
  echo $1
  cd $1
  cabal sandbox init --sandbox=../.cabal-sandbox
  cabal install
  cd -
}

install dag-tools
install system-tools

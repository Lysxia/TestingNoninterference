#!/bin/sh
MAIN=./dist/build/stack/stack
INSTRS=Cally
PROP=LLNI
EQUIV=Full
START=QuasiInitial
STRATEGY=ByExec
TIMEOUT=60
set -x
DATE=`date +%y%m%d-%H%M%S`
uname -n
$MAIN \
  --gen-instrs=Instrs$INSTRS \
  --ifc-semantics="*" \
  --prop-test=Prop$PROP \
  --equiv=Equiv$EQUIV \
  --starting-as=Start$START \
  --gen-strategy=Gen$STRATEGY \
  --shrink-nothing \
  --timeout=$TIMEOUT \
  --latex-output \
  $@


#!/bin/sh
MAIN=./dist/build/stack/stack
INSTRS=Cally
PROP=LLNI
EQUIV=Full
START=QuasiInit
TIMEOUT=300
set -x
DATE=`date +%y%m%d-%H%M%S`
uname -n
$MAIN \
  --gen-instrs=Instrs$INSTRS \
  --ifc-semantics="*" \
  --prop-test=Prop$PROP \
  --equiv=Equiv$EQUIV \
  --starting-as=Start$START \
  --timeout=$TIMEOUT \
  --latex-output \
  --gen-lucky \
  $@

#!/bin/sh
# To be run from project root.
MAIN=./dist/build/stack/stack
INSTRS=Cally
PROP=LLNI
EQUIV=Low
START=QuasiInitial
STRATEGY=ByExec
set -x
DATE=`date +%y%m%d-%H%M%S`
LOGFILE=./stack/scripts/logs/$DATE-`uname -n`
echo $@ >> $LOGFILE
$MAIN \
  --gen-instrs=Instrs$INSTRS \
  --ifc-semantics="*" \
  --prop-test=Prop$PROP \
  --equiv=Equiv$EQUIV \
  --starting-as=Start$START \
  --gen-strategy=Gen$STRATEGY \
  --timeout=1 \
  --latex-output \
  --gen-lucky \
  $@ | tee $LOGFILE

# The following options are used for the paper:
# --timeout=300 --gen-lucky
# --timeout=60 --gen-lucky=False


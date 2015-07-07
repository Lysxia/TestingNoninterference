#!/bin/sh
MAIN=./dist/build/stack/stack
INSTRS=Cally
PROP=LLNI
EQUIV=Low
START=QuasiInitial
TIMEOUT=300
set -x
DATE=`date +%y%m%d-%H%M%S`
LOGFILE=./stack/scripts/lucky/qinit/low/run.log-$DATE-`uname -n`
echo $@ >> $LOGFILE
$MAIN \
  --gen-instrs=Instrs$INSTRS \
  --ifc-semantics="*" \
  --prop-test=Prop$PROP \
  --equiv=Equiv$EQUIV \
  --starting-as=Start$START \
  --timeout=$TIMEOUT \
  --latex-output \
  --gen-lucky \
  $@ | tee $LOGFILE


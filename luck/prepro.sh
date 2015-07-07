#!/bin/sh
set +x
GENDIR=core/
mkdir -p $GENDIR
BASENAME=PicoGenExec
for START in "" "-DSTARTANY" ; do
  for EQUIV in "" "-DEQUIVFULL" ; do
    for BUG in "" "-DBUGARITH" "-DBUGPUSH" "-DBUGPOP" "-DBUGLOAD" \
      "-DBUGSTOREVALUE" "-DBUGSTOREPOINTER" "-DBUGSTOREPC" \
      "-DBUGJUMPNORAISE" "-DBUGJUMPLOWER" \
      "-DBUGCALL" "-DBUGRETURN" ; do
      m4 $START $EQUIV $BUG $BASENAME.m4 > $GENDIR$BASENAME$START$EQUIV$BUG.core
    done
  done
done


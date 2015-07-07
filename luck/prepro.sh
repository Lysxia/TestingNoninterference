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
      gcc -E -P -w $BASENAME.cpp $START $EQUIV $BUG -o $GENDIR$BASENAME$START$EQUIV$BUG.core
    done
  done
done


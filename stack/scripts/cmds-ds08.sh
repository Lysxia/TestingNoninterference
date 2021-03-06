#!/bin/bash
source ~/.bash_profile
export LD_LIBRARY_PATH=$HOME/lib
echo '% Automatically generated -- do not modify by hand' > ../picopaper/expsettings.tex
runhaskell GenerateTablesMakefile.hs --config=GenMakefileRel > Makefile.picotables
runhaskell GenerateTablesMakefile.hs --config=GenTablesRel > ../picopaper/picotable-names.tex
export FILENAME="../picopaper/exp-Cally-EENI-Low-Initial-ByExec2-True.tex" ; \
	echo '\\gdef\\bugtableinfo{'$FILENAME'}%' > $FILENAME.tmp ; \
	./TMUDriver \
	  --which-tmm-routine=NoTMMRoutine \
	  --gen-instrs=InstrsCally \
	  --ifc-semantics="[IfcBugArithNoTaint,IfcBugPushNoTaint,IfcBugLoadNoTaint,IfcBugStoreNoPointerTaint,IfcBugAllowWriteDownThroughHighPtr,IfcBugStoreNoValueTaint,IfcBugJumpNoRaisePc,IfcBugJumpLowerPc,IfcBugStoreNoPcTaint,IfcBugAllowWriteDownWithHighPc,IfcBugCallNoRaisePc,IfcBugReturnNoTaint,IfcBugValueOrVoidOnReturn,IfcBugPopPopsReturns]" \
	  --tmu-prop-test=PropEENI \
	  --equiv=EquivLow \
	  --starting-as=StartInitial \
	  --gen-strategy=GenByExec2 \
	  --smart-ints=True \
	  --shrink-nothing=True \
	  --show-counterexamples=False \
	  --latex-output \
	  --tmu-timeout=1000 \
	| tee -a $FILENAME.tmp; \
	mv $FILENAME.tmp $FILENAME; \
	scp $FILENAME antals@eniac.seas.upenn.edu:~/html/picotables/
mail -s 'Finished running commands on ds08 <EOM>' antals@seas.upenn.edu < /dev/null

### Build

    # Generate luck source (requires m4)
    # Destination is luck/luck
    cd luck/
    sh prepro.sh
    cd ..
    # luck must be installed.
    stack build tni
    # or cabal...

### Run

    # There's a script with basic parameters
    # It uses "stack exec". Replace with "cabal exec" if necessary
    sh exec-tni

### Output

    % Format = bug & #tests/sec & discard ratio & mean time to failure
	\row{\IfcDefault}{20}{0\%}{---}{0}{---}{---} %0/20 0.0000
	\means{}{---}{---}{---}
	\extrabuginfo{InstrsCally}{\PropLLNI}{EquivLow}{StartQuasiInitial}{\GenByExec}{True}{1sec}{1010}{Sat Mar 12 19:04:33 CET 2016}
	\averagespeed{20}
	\averagediscrate{0\%}

20 tests/sec, found 0 bug (IfcDefault is the correct generator+checker)
 over a total of 20 tests run in 1 second...

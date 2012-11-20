UHC-OPT=
MAIN=src/Examples/Shapes

default: all

all:
	${UHC} ${UHC-OPT} --import-path="src" ${MAIN}

ghci:
	ghci ${MAIN} -isrc

clean:
	find . -type f \( -name "*.grin" -o -name "*.c" -o -name "*.o" -o -name "*.hs-cpp" -o -name "*-cpp.hs" -o -name "*.hi" -o -name "*.core" -o -name "*.mjs" -o -name "*.js" -o -name "*.html" \) | xargs --no-run-if-empty rm

.PHONY: clean, tests
  

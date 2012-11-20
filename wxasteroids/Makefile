CPPHS=cpphs
UHC-OPT=--pgmP=${CPPHS} --optP=--noline --optP=--hashes
TARGET=js
MAIN=Asteroids.hs
OOLight=../../lightoo/src
JSPRELUDE=../../uhc-js/uhc-js/src

default: all

all:
	cd src && ${UHC} ${UHC-OPT} -t${TARGET} --import-path="../" --import-path="../contravariant-0.1.2" --import-path="${OOLight}" --import-path="${JSPRELUDE}" ${MAIN}
	cd ../

ghci:
	cd src && ghci -I${OOLight} -i${OOLight} -isrc ${MAIN}
	cd ../

clean:
	find . -type f \( -name "*.grin" -o -name "*.c" -o -name "*.o" -o -name "*.hs-cpp" -o -name "*.hi" -o -name "*.core" -o -name "*.mjs" -o -name "*.html" \) | xargs --no-run-if-empty rm

.PHONY: clean, tests

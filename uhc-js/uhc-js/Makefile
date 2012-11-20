default: package.conf.inplace

dist/setup-config:
	cabal configure --uhc

package.conf.inplace: dist/setup-config
	cabal build --uhc-option="-tjs"

clean:
	cabal clean

.PHONY: clean

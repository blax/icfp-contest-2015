.PHONY: all sandbox configure build submission clean full-clean

all: build

sources := $(wildcard src/*.hs)
package := icfp-contest-mmxv.cabal
sandbox-dir := .cabal-sandbox
sandbox-file := cabal.sandbox.config
config-file := dist/setup-config
executable := dist/build/play_icfp2015
submission := dist/icfp-contest-mmxv-1.0.tar.gz

sandbox: $(sandbox-file)
$(sandbox-file): $(package)
	cabal sandbox init
	cabal install --dependencies-only

configure: $(config-file)
$(config-file): $(package) $(sandbox-file)
	cabal configure

build: $(executable)
$(executable): $(package) $(sandbox-file) $(config-file) $(sources)
	cabal build || ( touch -r src $(@D) ; exit 1 )

submission: $(submission)
$(submission): $(executable)
	cabal sdist

clean:
	rm -rf dist

full-clean:
	rm -rf $(sandbox-dir) $(sandbox-file) dist

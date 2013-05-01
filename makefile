all: Subspace.html valor

ELM_VERSION="0.8"

elm-runtime.js:
	cp ~/.cabal/share/Elm-${ELM_VERSION}/elm-runtime.js .

Subspace.html: elm-runtime.js *.elm
	elm --make -r elm-runtime.js Subspace.elm

test.html: elm-runtime.js *.elm
	elm --make -r elm-runtime.js test.elm

valor: *.go
	go build

clean:
	rm -rf *.html *.js valor


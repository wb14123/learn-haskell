
all: json

json: json.hs
	ghc -prof -fprof-auto -rtsopts json.hs

test:
	./json +RTS -p < a.json

clean:
	rm *.hi *.o


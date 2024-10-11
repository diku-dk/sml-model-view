MLCOMP?=mlton

ALL: example

run: example
	./example

lib: sml.pkg
	smlpkg sync

example: example.sml example.mlb lib
	$(MLCOMP) -output example example.mlb

clean:
	rm -fr example lib MLB

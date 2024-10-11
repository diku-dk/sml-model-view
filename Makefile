MLCOMP?=mlton

ALL: example

run: example
	./example

example: example.sml example.mlb
	$(MLCOMP) -output example example.mlb


PACKAGE=imandra-reason-parser

all:
	@dune build @install -p $(PACKAGE)

clean:
	@dune clean

.PHONY: all clean

all: memory-architecture.svg

%.svg: %.scm
	guile -L . graph-lang.scm < $^ > $@
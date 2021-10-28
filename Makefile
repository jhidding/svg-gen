all: distributed-memory.svg shared-memory.svg

%.svg: %.scm
	guile -L . graph-lang.scm < $^ > $@
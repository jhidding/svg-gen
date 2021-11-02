all: memory-architecture.svg

%.svg: %.scm
	guile -L . xml-gen.scm < $^ > $@
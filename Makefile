all: $(patsubst %.scm,%.svg,$(wildcard examples/*.scm))

examples/%.svg: examples/%.scm examples/style.css
	guile xml-gen.scm < $< > $@

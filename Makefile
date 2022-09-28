.PHONY: site clean watch watch-pandoc watch-browser-sync

theme := default
pandoc_input := README.md
pandoc_output := docs/index.html

theme_dir := .entangled/templates/$(theme)
pandoc_args += -s -t html5 -f markdown+fenced_code_attributes+fenced_divs --toc --toc-depth 2
pandoc_args += --template $(theme_dir)/template.html
pandoc_args += --css theme.css
pandoc_args += --mathjax
pandoc_args += --filter pandoc-eqnos
# pandoc_args += --syntax-definition .entangled/syntax/dhall.xml
# pandoc_args += --highlight-style $(theme_dir)/syntax.theme
pandoc_args += --section-divs
pandoc_args += --lua-filter .entangled/scripts/hide.lua
pandoc_args += --lua-filter .entangled/scripts/annotate.lua
pandoc_args += --lua-filter .entangled/scripts/make.lua
static_files := $(theme_dir)/theme.css $(theme_dir)/static
static_targets := $(static_files:$(theme_dir)/%=docs/%)
functional_deps := Makefile $(wildcard .entangled/scripts/*.lua) $(theme_dir)/template.html $(theme_dir)/syntax.theme

# figure_src := $(wildcard fig/*)
# figure_targets := $(figure_src:%=docs/%)

all: $(patsubst %.scm,%.svg,$(wildcard examples/*.scm))

site: $(pandoc_output) $(static_targets) $(figure_targets)

clean:
	rm -rf docs

# $(figure_targets): docs/fig/%: fig/%
# 	@mkdir -p $(@D)
# 	cp $< $@

$(static_targets): docs/%: $(theme_dir)/%
	@mkdir -p $(@D)
	rm -rf $@
	cp -r $< $@

docs/index.html: README.md $(functional_deps)
	@mkdir -p $(@D)
	pandoc $(pandoc_args) -o $@ $<

# Starts a tmux with Entangled, Browser-sync and an Inotify loop for running
# Pandoc.
watch:
	@tmux new-session make --no-print-directory watch-pandoc \; \
		split-window -v make --no-print-directory watch-browser-sync \; \
		split-window -v entangled daemon \; \
		select-layout even-vertical \;

watch-pandoc:
	@while true; do \
		inotifywait -e close_write -r .entangled Makefile README.md; \
		make site; \
		make all; \
	done

watch-browser-sync:
	browser-sync start -w -s docs

examples/%.svg: examples/%.scm $(wildcard examples/*.css)
	guile xml-gen.scm < $< > $@

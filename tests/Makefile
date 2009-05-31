scheme := petite

.PHONY: all
all: render

.src: gen.ss
	$(scheme) $<

include .src

%.tga: %.scene run.ss
	echo '(load "$<")' | $(scheme) run.ss

.PHONY: render
render: $(src:.scene=.tga) run.ss

.PHONY: clean
clean:
	rm -f .src $(src) $(src:.scene=.tga)

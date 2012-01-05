scheme := petite

.PHONY: all
all: render

.src: gen.ss
	echo '(load "$<")' | $(scheme) -q run.ss

include .src

%.eps: %.asy
	asy -noView $<

%.png: %.eps
	convert -flatten $< $@

.PHONY: render
render: $(src:.asy=.png) run.ss

.PHONY: clean
clean:
	rm -f .src $(src) $(src:.asy=.eps) $(src:.asy=.png)

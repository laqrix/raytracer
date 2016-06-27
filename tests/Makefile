scheme := scheme

.PHONY: all
all: render

.src: gen.ss
	$(scheme) -q $<

include .src

%.tga: %.scene run.ss
	echo '(load "$<")' | $(scheme) -q run.ss

.PHONY: render
render: $(src:.scene=.tga) run.ss

.PHONY: clean
clean:
	rm -f .src $(src) $(src:.scene=.tga)

DAYS := $(shell ls | grep -E ^day-[[:digit:]]+\.el$)
SLIM := $(patsubst %.el, %-slim.el, $(DAYS))

.PHONY: all
all:
	./aoc-slim $(DAYS)

.PHONY: push
push:
	./push

.PHONY: slim
slim:
	$(RM) $(SLIM) day-*.elc

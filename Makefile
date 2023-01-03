DAYS := $(shell ls | grep -E ^day-[[:digit:]]+\.el$)
CLEAN := $(patsubst %.el, %-clean.el, $(DAYS))

.PHONY: all
all:
	./aoc-clean $(DAYS)

.PHONY: push
push:
	./push

.PHONY: clean
clean:
	$(RM) $(CLEAN) day-*.elc

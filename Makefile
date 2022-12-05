DAYS := $(shell ls | grep -E day-[[:digit:]]+\.el)
CLEAN := $(patsubst %.el, %-clean.el, $(DAYS))

.PHONY: all
all: $(CLEAN)

$(CLEAN): %-clean.el: %.el
	./aoc-clean $^

.PHONY: push
push:
	./push

.PHONY: clean
clean:
	$(RM) $(CLEAN)

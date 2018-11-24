compile: clean
	emacs --batch --quick --load test/make-compile.el

clean:
	rm -f *.elc

.PHONY: compile clean

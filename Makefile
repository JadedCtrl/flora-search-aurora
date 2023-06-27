LISP ?= ecl

maps:
	$(LISP) \
		--load "res/maps/tmx→lisp.lisp"

build:
	$(LISP) \
		--eval '(ql:quickload :flora-search-aurora)' \
		--eval '(asdf:make :flora-search-aurora)' \
		--eval '(quit)'

LISP ?= ecl

fonts:
	$(LISP) \
		--load "res/fonts/flf→lisp.lisp"
maps:
	$(LISP) \
		--load "res/maps/tmx→lisp.lisp"

build:
	$(LISP) \
		--eval '(ql:quickload :flora-search-aurora)' \
		--eval '(asdf:make :flora-search-aurora)' \
		--eval '(quit)'

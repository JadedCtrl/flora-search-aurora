LISP ?= ecl
SWANK ?= ${HOME}/.config/emacs/elpa/slime-20221206.26/swank-loader.lisp
USE_SWANK ?= no

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

run:
ifeq ($(USE_SWANK),yes)
	$(LISP) \
		--eval '(ql:quickload :flora-search-aurora)' \
		--eval '(load "$(SWANK)")' \
		--eval '(swank-loader:init)' \
		--eval "(swank:create-server :dont-close 't)" \
		--eval '(flora-search-aurora:main)'
else
	$(LISP) \
		--eval '(ql:quickload :flora-search-aurora)' \
		--eval '(flora-search-aurora:main)'
endif

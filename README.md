# Flora Search Aurora

## Installation

You can run the game through a pre-built binary, available under Releases of this repository.

If you’d rather (or have to) run the game from source, you can set up the game like so:

1. Install a Common Lisp implementation. I’d recommend [Steel Bank Common Lisp](http://www.sbcl.org/), which is robust and available on many *nixes under the package-name `sbcl`.
2. Set up [Quicklisp](https://quicklisp.org/), which is a “package-manager” for Common Lisp libraries.
3. Clone this repo into your Quicklisp projects path.
4. Run or build the game with `make run` or `make build`.

To do so, you can run the following commands, replacing `guix` with your package-manager of choice:

```
$ guix install sbcl
$ wget https://beta.quicklisp.org/quicklisp.lisp
$ sbcl --load quicklisp.lisp \
	--eval '(quicklisp-quickstart:install :path "~/.local/share/quicklisp/)' \
	--eval '(ql:add-to-init-file)'
$ rm quicklisp.lisp
$ cd ~/.local/share/quicklisp/local-projects/
$ git clone https://notabug.org/jadedctrl/flora-search-aurora
$ cd flora-search-aurora/
$ make build
$ ./flora-search-aurora
```

For development, so that you can connect to the game’s REPL with (i.e.) [SLIME](https://slime.common-lisp.dev/), you might want to run the game like so:

`$ USE_SWANK=yes SWANK=… make run`

… where SWANK is the path to SLIME’s `swank-loader.lisp` file, probably at `~/.config/emacs/elpa/slime-*/swank-loader.lisp`.

The game has been tested with both [Embeddable Common Lisp](https://ecl.common-lisp.dev/) and SBCL. It runs perfectly on both, but building a binary isn’t working on ECL at the moment.

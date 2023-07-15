# Flora Search Aurora
_Flora Search Aurora_ is a game about destiny-making, peace-making, and (most importantly) bouquet-making.


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
$ sbcl --load quicklisp.lisp
```

And in SBCL run…

```
* (quicklisp-quickstart:install :path "~/.local/lib/quicklisp/")
* (ql:add-to-init-file)
```

Back in the shell…

```
$ rm quicklisp.lisp
$ cd ~/.local/lib/quicklisp/local-projects/
$ git clone https://notabug.org/jadedctrl/flora-search-aurora
$ cd flora-search-aurora/
$ make build
$ ./flora-search-aurora
```

The game has been tested with both [Embeddable Common Lisp](https://ecl.common-lisp.dev/) and SBCL. It runs perfectly on both, but building a binary isn’t working on ECL at the moment.


## LibreJam
This game was made for the [2023-06 edition](https://jamgaroo.xyz/jams/2) of [LibreJam](https://bytecrab.org/librejam/).

The theme of this LibreJam was “ASCII”, and several restrictions were imposted.
Submitted games must:

* Run in a terminal
* Use only ASCII characters
* Print no more than 72 columns and 20 rows
* Display no colours or text formatting
* Have source-code smaller than 1MiB

Now, I’m proud to say that I meet these requirements! As for size, you can test like so:

```
$ du *.lisp res/maps/*.lisp \
| awk '{ printf("%s +", $1) } END { printf("\n") }' \
| sed 's/+$//' \
| bc \
| sed 's/$/KiB/'

764KiB
```


## Misc. information
Author: Jaidyn Ann <jadedctrl@posteo.at>
License: GNU GPLv3

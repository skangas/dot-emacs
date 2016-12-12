# GNU Make rules for fetching and byte-compiling various elisp files.
# http://edward.oconnor.cx/config/elisp/Makefile

EMACS=emacs -q --no-site-file

usage:
	@echo "usage: make [foo.el | foo.elc | autoloads | usage | all]"
	@echo "       make foo.el  will fetch foo.el from the Internet"
	@echo "       make foo.elc will byte-compile foo.el."
	@echo "                    (fetching foo.el first if necessary)."
	@echo "       make all will turn buil .elc for all .el in this dir."
	@echo "       make autoloads will update the autoloads file."
	@echo "       make usage displays this message."
	@echo

autoloads: install.el
	emacs -q --no-site-file -batch -L . -l install.el \
		-eval '(install-update-autoloads "autoloads")'


ELS = $(shell ls -1 *.el)
ELCS = $(ELS:.el=.elc)
all: $(ELCS)
	make $(ELCS)

.el.elc:
	$(EMACS) -batch -L . \
		-eval "(setq max-lisp-eval-depth 1500 max-specpdl-size 3000)" \
		-eval "(mapc (lambda (dir) (add-to-list 'load-path dir)) (parse-colon-path (getenv \"LOAD_PATH\")))" \
		-f batch-byte-compile $*.el

EMACSWIKI_DOWNLOAD=http://www.emacswiki.org/cgi-bin/wiki/download

# Where I most recently fetched various random elisp files written by
# other people.

cperl-mode.el:
	curl -O http://github.com/jrockway/cperl-mode/raw/master/cperl-mode.el
ffap-perl-module.el:
	curl -O ftp://download.tuxfamily.org/user42/ffap-perl-module.el
filladapt.el:
	curl -O http://www.wonderworks.com/download/filladapt.el
haskell-ghci.el:
	curl -O http://home.arachsys.com/~chris/haskell-ghci.el
rcirc-late-fix.el:
	curl -O $(EMACSWIKI_DOWNLOAD)/rcirc-late-fix.el
rcirc-notify.el:
	curl -O $(EMACSWIKI_DOWNLOAD)/rcirc-notify.el
zenburn-theme.el:
	curl -O https://github.com/djcb/elisp/raw/master/themes/zenburn-theme.el
window-numbering.el:
	curl -O http://nschum.de/src/emacs/window-numbering-mode/window-numbering.el

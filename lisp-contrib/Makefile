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

pjb-xresources.el:
	curl -O https://raw.githubusercontent.com/informatimago/emacs/master/pjb-xresources.el
sv-kalender.el:
	curl -O http://bigwalter.net/daniel/elisp/sv-kalender.el
sql-upcase.el:
	curl -O https://www.emacswiki.org/emacs/download/sql-upcase.el

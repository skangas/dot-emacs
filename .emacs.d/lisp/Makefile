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

autopair.el:
	curl -O http://autopair.googlecode.com/svn/trunk/autopair.el
boxquote.el:
	curl -O http://www.davep.org/emacs/boxquote.el
centered-cursor-mode.el:
	curl -O http://www.emacswiki.org/emacs/download/centered-cursor-mode.el
cursor-chg.el:
	curl -O $(EMACSWIKI_DOWNLOAD)/cursor-chg.el # $
cperl-mode.el:
	curl -O http://github.com/jrockway/cperl-mode/raw/master/cperl-mode.el
ffap-perl-module.el:
	curl -O ftp://download.tuxfamily.org/user42/ffap-perl-module.el
filladapt.el:
	curl -O http://www.wonderworks.com/download/filladapt.el
haskell-ghci.el:
	curl -O http://home.arachsys.com/~chris/haskell-ghci.el
iimage.el:
	curl -O http://www.netlaputa.ne.jp/~kose/Emacs/lisp/iimage.el
openwith.el:
	curl -O http://stud4.tuwien.ac.at/~e0225855/misc/openwith.el
paredit.el:
	curl -O http://mumble.net/~campbell/emacs/paredit.el
rcirc-late-fix.el:
	curl -O $(EMACSWIKI_DOWNLOAD)/rcirc-late-fix.el
rcirc-notify.el:
	curl -O $(EMACSWIKI_DOWNLOAD)/rcirc-notify.el
sr-speedbar.el:
	curl -O $(EMACSWIKI_DOWNLOAD)/sr-speedbar.el # $
sunrise-commander.el:
	curl -O $(EMACSWIKI_DOWNLOAD)/sunrise-commander.el # $
window-numbering.el:
	curl -O http://nschum.de/src/emacs/window-numbering-mode/window-numbering.el

# commented out stuff that I don't use
# applescript-mode.el:
# 	curl -O http://jaist.dl.sourceforge.jp/macemacsjp/9401/applescript-mode.el
# boxquote.el:
# 	curl -O http://www.davep.org/emacs/boxquote.el
# bzr-mode.el:
# 	curl -O http://fresh.homeunix.net/~luke/misc/repo/bzr-mode/bzr-mode.el
# css-mode.el:
# 	curl -O http://www.garshol.priv.no/download/software/css-mode/css-mode.el
# csv.el:
# 	curl -o csv.el http://de.geocities.com/ulf_jasper/lisp/csv.el.txt
# darcs.el:
# 	curl -O http://fresh.homeunix.net/~luke/repos/darcs-mode/darcs.el
# darcsum.el:
# 	curl -O http://www.newartisans.com/johnw/Emacs/darcsum.el
# dom.el:
# 	curl -O $(EMACSWIKI_DOWNLOAD)/dom.el # $
# ecmascript-mode.el:
# 	curl -O $(EMACSWIKI_DOWNLOAD)/ecmascript-mode.el # $
# erc-robot.el:
# 	curl -O $(EMACSWIKI_DOWNLOAD)/erc-robot.el # $
# eudcb-mab.el:
# 	curl -O http://www.newartisans.com/johnw/Emacs/eudcb-mab.el
# fillcode.el:
# 	curl -O http://snarfed.org/space/fillcode/fillcode.el
# hexrgb.el:
# 	curl -O http://www.emacswiki.org/emacs/download/hexrgb.el
# SAVANNAH_VIEWVC=http://cvs.savannah.nongnu.org/viewvc/*checkout*
# http-cookies.el http-get.el http-post.el:
# 	curl -o $*.el $(SAVANNAH_VIEWVC)/http-emacs/$*.el?root=http-emacs # $
# http-twiddle.el:
# 	curl -O http://fresh.homeunix.net/~luke/misc/emacs/http-twiddle.el
# install.el:
# 	curl -O http://www.iro.umontreal.ca/~monnier/elisp/install.el
# javascript.el: javascript.el.zip
# 	unzip -fo javascript.el.zip
# javascript.el.zip:
# 	curl -O http://web.comhem.se/~u34308910/emacs/javascript.el.zip
# lisppaste.el:
# 	curl -O http://www.ph.ed.ac.uk/~s0198183/lisppaste.el
# markdown-mode.el:
# 	curl -O http://jrblevin.freeshell.org/software/markdown-mode/markdown-mode.el
# mb-depth.el:
# 	curl -O $(EMACSWIKI_DOWNLOAD)/mb-depth.el # $
# multi-region.el:
# 	curl -O http://purl.org/NET/wence/multi-region.el
# mwe-log-commands.el:
# 	curl -O http://www.foldr.org/~michaelw/emacs/mwe-log-commands.el
# oddmuse.el:
# 	curl -O $(EMACSWIKI_DOWNLOAD)/oddmuse.el # $
# oneonone.el:
# 	curl -O $(EMACSWIKI_DOWNLOAD)/oneonone.el
# paredit.el:
# 	curl -O http://mumble.net/~campbell/emacs/paredit.el
# parenface.el:
# 	curl -O http://www.davep.org/emacs/parenface.el
# pcmpl-darcs.el:
# 	curl -O http://sacha.free.net.ph/notebook/emacs/pcmpl-darcs.el
# php-mode.el:
# 	curl -O http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Easymacs/repository/lib/php-mode.el
# pop3.el:
# 	curl -o pop3.el "http://cvs.m17n.org/viewcvs/*checkout*/root/gnus/lisp/pop3.el?rev=1.6.30.21.4.25"
# psvn.el:
# 	curl -O http://www.xsteve.at/prg/emacs/psvn.el
# python.el: syntax.el sym-comp.el
# 	curl -o python.el http://www.loveshack.ukfsn.org/emacs/python-21.el
# rpm-spec-mode.el:
# 	curl -O http://www.tihlde.org/~stigb/rpm-spec-mode.el
# rails.el:
# 	curl -O https://opensvn.csie.org/mvision/emacs/.emacs.d/rails.el
# ruby: misc.tar.gz
# 	tar xzvf misc.tar.gz
# 	mv misc ruby
# RUBY_CVSWEB="http://www.ruby-lang.org/cgi-bin/cvsweb.cgi/ruby"
# misc.tar.gz:
# 	curl -o misc.tar.gz "$(RUBY_CVSWEB)/misc/misc.tar.gz?tarball=1" # $
# setnu.el:
# 	curl -O $(EMACSWIKI_DOWNLOAD)/setnu.el # $
# sym-comp.el:
# 	curl -O http://www.loveshack.ukfsn.org/emacs/sym-comp.el
# tidy.el:
# 	curl -O $(EMACSWIKI_DOWNLOAD)/tidy.el # $
# tracking.el:
# 	curl -O $(EMACSWIKI_DOWNLOAD)/tracking.el # $
# twit.el:
# 	curl -O http://www.emacswiki.org/cgi-bin/emacs/download/twit.el
# WIKIPEDIA_ROOT="http://cvs.gna.org/viewcvs/*checkout*/wikipedia-el"
# WIKIPEDIA_PATH="wikipedia-el/wikipedia.el?rev=HEAD&content-type=text/plain"
# wikipedia.el:
# 	curl -o wikipedia.el $(WIKIPEDIA_ROOT)/$(WIKIPEDIA_PATH)
# wikipedia-mode.el:
# 	curl -O $(EMACSWIKI_DOWNLOAD)/wikipedia-mode.el # $
# vc-darcs.el:
# 	curl -O $(EMACSWIKI_DOWNLOAD)/vc-darcs.el # $
# vc-svn.el:
# 	curl -O http://www.tug.org/ftp/dist/vc-svn.el
# xml-rpc.el:
# 	curl -o xml-rpc.el "$(SAVANNAH_VIEWVC)/emacsweblogs/weblogger/lisp/xml-rpc.el?revision=1.7" # $
#         # patch -p0 xml-rpc.el xml-rpc.el.patch
# xml-rpc.el.patch:
# 	curl -O http://www.ph.ed.ac.uk/~s0198183/xml-rpc.el.patch
# xterm-frobs.el:
# 	curl -O ftp://ftp.splode.com/pub/users/friedman/emacs-lisp/xterm-frobs.el

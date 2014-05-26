VERSION = $(shell git describe --tags --match 'v[0-9]*' --abbrev=0 | sed 's/^v//;s/\.0*/./g')
GIT_VERSION = $(shell git describe --tags --match 'v[0-9]*' --long --dirty | sed 's/^v//')

INSTALL_INFO = install-info
EMACS = emacs
EFLAGS =
BATCH = $(EMACS) $(EFLAGS) --batch -Q -L .
SUBST_ATAT = sed -e 's/@@GIT_VERSION@@/$(GIT_VERSION)/g;s/@GIT_VERSION@/$(GIT_VERSION)/g;s/@@VERSION@@/$(VERSION)/g;s/@VERSION@/$(VERSION)/g'

ELFILES = \
	purescript-align-imports.el \
	purescript-collapse.el \
	purescript-font-lock.el \
	purescript-indent.el \
	purescript-indentation.el \
	purescript-mode.el \
	purescript-move-nested.el \
	purescript-navigate-imports.el \
	purescript-package.el \
	purescript-simple-indent.el \
	purescript-sort-imports.el \
	purescript-str.el \
	purescript-string.el \
	purescript-unicode-input-method.el \
	purescript-utils.el \
	purescript-yas.el

ELCFILES = $(ELFILES:.el=.elc)
AUTOLOADS = purescript-mode-autoloads.el

PKG_DIST_FILES = $(ELFILES) logo.svg NEWS purescript-mode.info dir
PKG_TAR = purescript-mode-$(VERSION).tar
ELCHECKS=$(addprefix check-, $(ELFILES:.el=))

%.elc: %.el
	@$(BATCH) \
	   --eval "(byte-compile-disable-warning 'cl-functions)" \
	   -f batch-byte-compile $<

.PHONY: all compile info clean check $(ELCHECKS) elpa package

all: compile $(AUTOLOADS) info

compile: $(ELCFILES)

$(ELCHECKS): check-%: %.el
	@$(BATCH) --eval '(when (check-declare-file "$*.el") (error "check-declare failed"))'
	@$(BATCH) \
		 --eval "(setq byte-compile-error-on-warn t)" \
	 	 --eval "(byte-compile-disable-warning 'cl-functions)" \
		 -f batch-byte-compile $*.el
	@$(RM) $*.elc
	@if [ -f "$(<:%.el=tests/%-tests.el)" ]; then \
	if $(BATCH) --eval "(require 'ert)" 2> /dev/null; then \
		echo; \
		$(BATCH) -l "$(<:%.el=tests/%-tests.el)" -f ert-run-tests-batch-and-exit; \
	else \
		echo "ERT not available, skipping unit tests"; \
	fi; \
	fi
	@echo "--"

check: clean $(ELCHECKS)
	@echo "checks passed!"

clean:
	$(RM) $(ELCFILES) $(AUTOLOADS) $(AUTOLOADS:.el=.elc) $(PKG_TAR) purescript-mode.tmp.texi purescript-mode.info dir

info: purescript-mode.info dir

dir: purescript-mode.info
	$(INSTALL_INFO) --dir=$@ $<

purescript-mode.tmp.texi: purescript-mode.texi
	$(SUBST_ATAT) < purescript-mode.texi > purescript-mode.tmp.texi

purescript-mode.info: purescript-mode.tmp.texi
	$(MAKEINFO) $(MAKEINFO_FLAGS) -o $@ $<

purescript-mode.html: purescript-mode.tmp.texi
	$(MAKEINFO) $(MAKEINFO_FLAGS) --html --no-split -o $@ $<

# Generate ELPA-compatible package
package: $(PKG_TAR)
elpa: $(PKG_TAR)

$(PKG_TAR): $(PKG_DIST_FILES) purescript-mode-pkg.el.in
	rm -rf purescript-mode-$(VERSION)
	mkdir purescript-mode-$(VERSION)
	cp $(PKG_DIST_FILES) purescript-mode-$(VERSION)/
	$(SUBST_ATAT) < purescript-mode-pkg.el.in > purescript-mode-$(VERSION)/purescript-mode-pkg.el
	$(SUBST_ATAT) < purescript-mode.el > purescript-mode-$(VERSION)/purescript-mode.el
	(sed -n -e '/^;;; Commentary/,/^;;;/p' | egrep '^;;( |$$)' | cut -c4-) < purescript-mode.el > purescript-mode-$(VERSION)/README
	tar cvf $@ purescript-mode-$(VERSION)
	rm -rf purescript-mode-$(VERSION)
	@echo
	@echo "Created ELPA compatible distribution package '$@' from $(GIT_VERSION)"

$(AUTOLOADS): $(ELFILES) purescript-mode.elc
	$(BATCH) \
		--eval '(setq make-backup-files nil)' \
		--eval '(setq generated-autoload-file "$(CURDIR)/$@")' \
		-f batch-update-autoloads "."

# HACK: embed version number into .elc file
purescript-mode.elc: purescript-mode.el
	$(SUBST_ATAT) < purescript-mode.el > purescript-mode.tmp.el
	@$(BATCH) --eval "(byte-compile-disable-warning 'cl-functions)" -f batch-byte-compile purescript-mode.tmp.el
	mv purescript-mode.tmp.elc purescript-mode.elc
	$(RM) purescript-mode.tmp.el

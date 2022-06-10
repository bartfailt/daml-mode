.POSIX:
EMACS = emacs

NEEDED_PACKAGES = lsp-mode haskell-mode package-lint

INIT_PACKAGES="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (dolist (pkg '(${NEEDED_PACKAGES})) \
    (unless (package-installed-p pkg) \
      (unless (assoc pkg package-archive-contents) \
        (package-refresh-contents)) \
      (package-install pkg))) \
  )"

all: compile package-lint clean-elc

package-lint:
	${EMACS} -Q --eval ${INIT_PACKAGES} --eval '(setq package-lint-main-file "lisp/daml-mode.el")' -batch -f package-lint-batch-and-exit lisp/daml-mode.el

compile: clean-elc
	${EMACS} -Q -eval ${INIT_PACKAGES} -L . -batch -f batch-byte-compile lisp/daml-mode.el

clean-elc:
	rm -f lisp/daml-mode.elc

.PHONY: all compile clean-elc package-lint

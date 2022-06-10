;;; daml-mode.el --- Emacs mode for daml

;; Copyright (C) 2016-2022 Bártfai Tamás

;; Author: Joseph Collard
;; Package-Requires: ((haskell-mode "16.1") (lsp-mode "7.0") (emacs "25.1"))
;; URL: https://github.com/bartfaitamas/daml-mode
;; Package-Version: 0-snapshot

;;; Commentary:

;; Provides a major mode for editing daml source code, and working
;; with the daml SDK provided lsp server common core and third-party
;; Elm tools including the compiler, repl, elm-format and more.

;;; Code:
(require 'haskell-mode)
(require 'haskell-indent)
(require 'cl-lib)

(defcustom daml-font-lock-keywords
  '("template" "ensure" "daml" "observer" "signatory" "agreement" "controller" "can" "nonconsuming" "return" "with" "mwith")
  "Identifiers treated as reserved keywords in daml and are not
keywords for Haskell."
  :group 'daml
  :type '(repeat string))

(set
 'haskell-font-lock-keywords
 (cl-remove-duplicates (append haskell-font-lock-keywords daml-font-lock-keywords)))

(defun daml-font-lock-keywords ()
  "Generate font lock keywords for daml. Mostly haskell, with some
daml specific syntax."
  (append (haskell-font-lock-keywords)
          `(("\\<\\(submit\\|submitMustFail\\|fetch\\|exercise\\|create\\|test\\)\\>"
             0 font-lock-function-name-face)
            ("\\<\\(m?with\\)\\>"
             0 font-lock-builtin-face))))

(define-derived-mode daml-mode haskell-mode "daml" "Major mode for daml."
  (setq-local font-lock-defaults
              '((daml-font-lock-keywords)
                nil nil nil nil
                (font-lock-syntactic-face-function
                 . haskell-syntactic-face-function)
                ;; Get help from font-lock-syntactic-keywords.
                (parse-sexp-lookup-properties . t)
                (font-lock-extra-managed-props . (composition haskell-type))))
  (haskell-indentation-mode -1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.daml\\'" . daml-mode))

(provide 'daml-mode)
;;; daml-mode.el ends here

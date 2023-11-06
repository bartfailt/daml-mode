;;; daml-mode.el --- Major mode for daml             -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2022 Bártfai Tamás
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Bártfai Tamás

;; Package-Requires: ((emacs "27.1") (haskell-mode "16.1"))
;; URL: https://github.com/bartfaitamas/daml-mode
;; Package-Version: 1.0

;;; Commentary:

;; Provides a major mode for editing daml source code; for working
;; with the Daml SDK provided lsp facilities also install daml-lsp.

;;; Code:
(require 'haskell-mode)
(require 'haskell-indent)
(require 'cl-lib)

(defcustom daml-font-lock-keywords
  '("template" "ensure" "daml" "observer" "signatory" "agreement"
    "controller" "can" "nonconsuming" "with" "mwith"
    "choice" "interface" "key" "maintainer" "exception" "for"
    "viewtype" "magreement" "preconsuming" "postconsuming" "message")
  "Identifiers treated as reserved keywords in daml.
They are not keywords for Haskell."
  :group 'daml
  :type '(repeat string))

(set
 'haskell-font-lock-keywords
 (cl-remove-duplicates (append haskell-font-lock-keywords daml-font-lock-keywords)))

(defun daml-font-lock-keywords ()
  "Generate font lock keywords for daml.
Mostly haskell, with some daml specific syntax."
  (append (haskell-font-lock-keywords)
          `(("\\<\\(submit\\|submitMustFail\\|fetch\\|exercise\\|create\\|test\\)\\>"
             0 font-lock-function-name-face)
            ("\\<\\(m?with\\|choice\\|template\\|interface\\|key\\|maintainer\\|daml\\|exception\\|for\\|viewtype\\|m?agreement\\|controller\\|can\\|ensure\\|signatory\\|nonconsuming\\|observer\\|preconsuming\\|postconsuming\\|message     )\\>"
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

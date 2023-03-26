(require 'package)
(push '("melpa" . "https://melpa.org/packages/") package-archives)
(package-initialize)
(dolist (pkg '(lsp-mode haskell-mode package-lint)) 
  (unless (package-installed-p pkg) 
    (unless (assoc pkg package-archive-contents) 
      (package-refresh-contents)) 
    (package-install pkg)))

(setq custom-file 
      (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory))

(add-to-list 'load-path (expand-file-name "~/daml-mode/lisp"))

(require 'daml-mode)
(require 'daml-lsp)

(add-hook 'daml-mode-hook #'lsp)

;;; daml-lsp.el --- LSP client definition for daml

;; Copyright (C) 2016-2022 Bártfai Tamás

;;; Commentary:

;; Client definition for lsp-mode. 

;;; Code:

(require 'lsp-mode)

;;;###autoload
(defcustom daml-lsp-extra-arguments nil
  "Specify the path to damlc."
  :type '(list string)
  :group 'daml-lsp)

(defgroup daml-lsp nil
  "Customization group for daml-lsp."
  :group 'tools)

(defun daml-ls--suggest-project-root ()
  (and (memq major-mode '(daml-mode))
       (when-let (dir (locate-dominating-file default-directory "daml.yaml"))
         (expand-file-name dir))))

(add-to-list 'lsp-language-id-configuration '(daml-mode . "daml"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (append '("daml" "ide") daml-lsp-extra-arguments))
                  :major-modes '(daml-mode)
                  :priority -1
                  :multi-root nil
                  :server-id 'daml-ls
                  :notification-handlers
                  (lsp-ht
                   ("daml/keepAlive" 'ignore)
                   ("daml/workspace/validations" 'ignore)
                   ("window/progress/start" 'ignore)
                   ("window/progress/report" 'ignore)
                   ("window/progress/done" 'ignore)
                   ("$/progress" 'ignore))))

(provide 'daml-lsp)
;;; daml-lsp.el ends here

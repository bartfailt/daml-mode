;;; daml-lsp.el --- LSP client definition for daml

;; Copyright (C) 2016-2022 Bártfai Tamás

;;; Commentary:

;; Client definition for lsp-mode.

;;; Code:

(require 'lsp-mode)
(require 'shr)
(require 'dash)
(require 'url-util)

;;;###autoload
(defcustom daml-lsp-extra-arguments nil
  "Specify the path to damlc."
  :type '(list string)
  :group 'daml-lsp)

(defgroup daml-lsp nil
  "Customization group for daml-lsp."
  :group 'tools)

(add-to-list 'lsp-language-id-configuration '(daml-mode . "daml"))
(add-hook 'kill-buffer-hook #'lsp-daml--virtualResource-kill-hook)

(defun lsp-daml--virtualResource-kill-hook ()
  (when (string-prefix-p "*daml-script " (buffer-name))

    (message "lsp-daml--virtualResource-kill-hook")
    (let ((script-uri (substring (buffer-name) 13)))
      (lsp-notify
       "textDocument/didClose"
       (list :textDocument
             (list :uri script-uri
                   :languageId ""
                   :version 0
                   :text ""))))))

(defun lsp-daml--script-result-buffer-name (script-uri)
  (format "*daml-script %s" (url-unhex-string script-uri)))

(lsp-defun lsp-daml--show-resource ((&Command :arguments?))
  "Execute the show daml script action"
  (interactive)
  (let* ((script-uri (elt arguments? 1))
         (result-buf-name (lsp-daml--script-result-buffer-name script-uri)))
     (lsp-notify
      "textDocument/didOpen"
      (list :textDocument
            (list :uri script-uri
                  :languageId ""
                  :version 0
                  :text "")))

     (if-let ((result-buf (get-buffer result-buf-name)))
         (with-help-window result-buf
           (print "Try modifying the script to show the results"))
       (with-output-to-temp-buffer result-buf-name
         (print (format "Executing script %s"
                        (url-unhex-string script-uri)))))))


(lsp-interface (DAMLVirtualResourceChange (:uri :contents) nil)
               (DAMLVirtualResourceNote (:uri :note) nil))

(defun lsp-daml--virtualResource-note (workspace params)
  (-let [(&DAMLVirtualResourceNote :uri :note) params]
    (lsp-daml--display-virtualResource uri note)))

(defun lsp-daml--virtualResource-change (workspace params)
  (-let [(&DAMLVirtualResourceChange :uri :contents) params]
    (lsp-daml--display-virtualResource uri contents)))

(defun lsp-daml--display-virtualResource (uri contents)
  (let* ((result-buffer-name (lsp-daml--script-result-buffer-name uri))
         (dom (with-temp-buffer
                (insert contents)
                (libxml-parse-html-region (point-min) (point-max)))))
    (when (get-buffer result-buffer-name)
      (with-current-buffer (get-buffer-create result-buffer-name)
        (with-help-window result-buffer-name
          (erase-buffer)
          (shr-insert-document dom))))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (append '("daml" "ide") daml-lsp-extra-arguments))
                  :major-modes '(daml-mode)
                  :priority -1
                  :multi-root nil
                  :server-id 'daml-ls
                  :action-handlers (lsp-ht ("daml.showResource" #'lsp-daml--show-resource))
                  :notification-handlers
                  (lsp-ht
                   ("daml/virtualResource/didChange" #'lsp-daml--virtualResource-change)
                   ("daml/virtualResource/note" #'lsp-daml--virtualResource-note))))

(provide 'daml-lsp)
;;; daml-lsp.el ends here

;;; daml-lsp.el --- LSP client definition for daml             -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2022 Bártfai Tamás
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Client definition for lsp-mode.

;;; Code:

(require 'lsp-mode)
(require 'dash)

;;;###autoload
(defcustom daml-lsp-extra-arguments nil
  "Specify the path to damlc."
  :type '(list string)
  :group 'daml-lsp)

(defgroup daml-lsp nil
  "Customization group for daml-lsp."
  :group 'tools)

(add-to-list 'lsp-language-id-configuration '(daml-mode . "daml"))
(add-hook 'kill-buffer-hook #'daml-lsp--virtualResource-kill-hook)

(defun daml-lsp--virtualResource-kill-hook ()
  "Run when buffer closed to free up LSP resources."
  (when (string-prefix-p "*daml-script " (buffer-name))

    (message "daml-lsp--virtualResource-kill-hook")
    (let ((script-uri (substring (buffer-name) 13)))
      (lsp-notify
       "textDocument/didClose"
       (list :textDocument
             (list :uri script-uri
                   :languageId ""
                   :version 0
                   :text ""))))))

(defun daml-lsp--script-result-buffer-name (script-uri)
  "Generate a name for result buffer from running daml script SCRIPT-URI."
  (format "*daml-script %s" (url-unhex-string script-uri)))

(lsp-defun daml-lsp--show-resource ((&Command :arguments?))
  "Execute the show daml script action."
  (interactive)
  (let* ((script-uri (elt arguments? 1))
         (result-buf-name (daml-lsp--script-result-buffer-name script-uri)))
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

(eval-when-compile
  (lsp-interface (DAMLVirtualResourceChange (:uri :contents) nil)
                 (DAMLVirtualResourceNote (:uri :note) nil)))

(defun daml-lsp--virtualResource-note (_workspace params)
  "Display a virtual resource note with PARAMS.  WORKSPACE is ignored."

  (-let [(&DAMLVirtualResourceNote :uri :note) params]
    (daml-lsp--display-virtualResource uri note)))

(defun daml-lsp--virtualResource-change (_workspace params)
  "Display a virtual resource change with PARAMS.  WORKSPACE is ignored."
  (-let [(&DAMLVirtualResourceChange :uri :contents) params]
    (daml-lsp--display-virtualResource uri contents)))

(defun daml-lsp--display-virtualResource (uri contents)
  "Display virtual resource info for URI and CONTENTS."
  (let* ((result-buffer-name (daml-lsp--script-result-buffer-name uri))
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
                  :action-handlers (lsp-ht ("daml.showResource" #'daml-lsp--show-resource))
                  :notification-handlers
                  (lsp-ht
                   ("daml/virtualResource/didChange" #'daml-lsp--virtualResource-change)
                   ("daml/virtualResource/note" #'daml-lsp--virtualResource-note))))

(provide 'daml-lsp)
;;; daml-lsp.el ends here

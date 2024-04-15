;;; flycheck-biomejs.el --- Flycheck: Biome JavaScript/TypeScript linter support -*- lexical-binding: t; -*-

;; Copyright (C) 2024  craneduck

;; Author: craneduck <28823828+craneduck@users.noreply.github.com>
;; Keywords: tools, convenience, javascript, typescript
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (flycheck "32"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This Flycheck extension is an EXPERIMENTAL PACKAGE intended to provide
;; JavaScript/TypeScript syntax checking by biome.
;; (see URL `https://biomejs.dev/')

;;; Code:

(require 'flycheck)

(defgroup flycheck-biomejs ()
  "."
  :group 'tools
  :prefix "flycheck-biomejs-"
  :link '(url-link "https://github.com/craneduck/flycheck-biomejs"))

(defvar flycheck-biomejs/connection nil)
(defvar flycheck-biomejs/buffer-list nil)
(defvar-local flycheck-biomejs/vcs-root-path nil)
(defvar-local flycheck-biomejs/config-path nil)
(defvar-local flycheck-biomejs/config-object nil)
;; (defvar-local flycheck-biomejs/erros nil)

;; Referenced eglot. `https://github.com/joaotavora/eglot/blob/master/eglot.el'
(defconst flycheck-biomejs/{} (make-hash-table :size 1) "The empty JSON object.")
(defconst flycheck-biomejs/jsonrpc-client-name "flycheck-biomejs")
(defconst flycheck-biomejs/jsonrpc-client-version "0.0.1")

(defun flycheck-biomejs/get-version (command)
  "Return biome version string.

COMMAND: Full path or command name of the biome command"
  ;; Version: 1.6.0 => "1.6.0"
  (let ((line (shell-command-to-string
               (mapconcat #'shell-quote-argument
                          `(,command "--version") " ")))
        (re (rx "Version:"
                (zero-or-more space)
                (group (one-or-more digit)
                       "."
                       (one-or-more digit)
                       "."
                       (one-or-more digit)))))
    (when (string-match-p re line)
      (string-match re line)
      (match-string 1 line))))

(defun flycheck-biomejs/check-version (installed supported)
  "Compare INSTALLED and SUPPORTED versions and return a boolean value."
  (let* ((installed (split-string installed "\\."))
         (supported (split-string supported "\\."))
         (installed-major (string-to-number (nth 0 installed)))
         (installed-minor (string-to-number (nth 1 installed)))
         (installed-patch (string-to-number (nth 2 installed)))
         (supported-major (string-to-number (nth 0 supported)))
         (supported-minor (string-to-number (nth 1 supported)))
         (supported-patch (string-to-number (nth 2 supported))))
    (cond ((< installed-major supported-major) nil)
          ((> installed-major supported-major) t)
          (t (cond ((< installed-minor supported-minor) nil)
                   ((> installed-minor supported-minor) t)
                   (t (>= installed-patch supported-patch)))))))

(defun flycheck-biomejs/convert-jsonc-to-json ()
  "Convert JSONC to JSON.

After changing buffer to `c-mode', remove comments according to
JavaScript syntax.  Then use a regular expression to remove trailing commas"
  (c-mode)
  ;; Remove comments
  (goto-char (point-min))
  (let ((re (rx (or "\"" "//" "/*"))))
    (while (re-search-forward re nil t)
      (cond ((char-equal (char-before) ?\")
             ;; if "\"", go back 1 character and
             ;; move to the back of the closing quote
             (goto-char (1- (point)))
             (goto-char (scan-sexps (point) 1)))
            ((or (char-equal (char-before) ?/)
                 (char-equal (char-before) ?*))
             ;; If "//" or "/*", go back 2 characters and
             ;; delete the string for the forward-comment move
             (goto-char (- (point) 2))
             (let ((begin (point)))
               (forward-comment (point-max))
               (delete-region begin (point))))
            )))
  ;; Remove trailing commas
  (goto-char (point-min))
  (let ((re (rx (>= 1 (zero-or-more (or space "\r" "\n"))
                    ","
                    (zero-or-more (or space "\r" "\n")))
                (group (or "]" "}")))))
    (replace-regexp-in-region re "\\1")))

(defun flycheck-biomejs/get-vcs-root-path ()
  "Return VCS root path."
  (when-let ((path (locate-dominating-file buffer-file-name ".git")))
    (expand-file-name "./" path)))

;; (defun flycheck-biomejs/get-manifest-path ()
;;   "Experimental function."
;;   (when-let ((path (locate-dominating-file buffer-file-name "package.json")))
;;     (expand-file-name "package.json" path)))

(defun flycheck-biomejs/get-config-path ()
  "Return biome.json path."
  (when-let ((path (locate-dominating-file buffer-file-name "biome.json")))
    (expand-file-name "biome.json" path)))

(defun flycheck-biomejs/get-file-content (path)
  "Return the file content of PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun flycheck-biomejs/get-config-object (path)
  "Parse and return a json file of the PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (flycheck-biomejs/convert-jsonc-to-json)
    (goto-char (point-min))
    (json-parse-buffer :null-object nil
                       :false-object :json-false)))

;; Running biome in a synchronous process causes Emacs to become inoperable
;; 同期プロセスでbiomeを実行すると、Emacsが操作不能になる
;; (defun flycheck-biomejs/start-server-and-get-socket-path ()
;;   "Experimental function."
;;   (string-trim (shell-command-to-string "biome __print_socket")))

(defun flycheck-biomejs/process-sentinel (process event)
  "Close the buffer with the result of `biome __print_socket'.

PROCESS and EVENT: make-processのsentinelを参照"
  (when (string= "finished\n" event)
    (kill-buffer (process-buffer process))))

(defun flycheck-biomejs/start-server-and-get-socket-path ()
  "Execute `biome __print_socket' and return the result.

`biome __print_socket' command automatically starts the biome server
and prints the socket path."
  ;; 同期プロセスでbiomeを実行すると、Emacsが操作不能になる
  (let* ((process (make-process :name "biome-process"
                                :buffer "*biome process*"
                                :sentinel #'flycheck-biomejs/process-sentinel
                                :command '("biome" "__print_socket")))
         (start-time (current-time))
         (path (with-current-buffer (process-buffer process)
                 (while (and (seq-empty-p (buffer-string))
                             (<= (float-time (time-subtract (current-time) start-time)) 3))
                   (sleep-for 0.001))
                 (unless (seq-empty-p (buffer-string))
                   (goto-char (point-min))
                   (buffer-substring-no-properties (point-min) (search-forward "\n"))))))
    (when path (string-trim path))))

(defun flycheck-biomejs/jsonrpc-connect-and-initialize ()
  "Connect to the started biome server with unix domain socket and initialize."
  (let* ((socket-path (flycheck-biomejs/start-server-and-get-socket-path))
         (conn (make-instance 'jsonrpc-process-connection
                              :name "flycheck-biomejs-jsonrpc"
                              :events-buffer-config '(:size 2000000 :format full)
                              :process (make-network-process
                                        :name "*flycheck biomejs jsonrpc*"
                                        :nowait nil
                                        :noquery t
                                        ;; :host 'local
                                        :coding 'utf-8-unix
                                        :family 'local
                                        :service socket-path)))
         ;; (vcs-root-path (flycheck-biomejs/get-vcs-root-path))
         ;; (manifest-path (flycheck-biomejs/get-manifest-path))
         ;; (manifest-content (if config-path (flycheck-biomejs/get-file-content config-path) nil))
         )
    (jsonrpc-request conn "initialize" `(:capabilities
                                         ,flycheck-biomejs/{}
                                         :client_info (:name
                                                       ,flycheck-biomejs/jsonrpc-client-name
                                                       :version ,flycheck-biomejs/jsonrpc-client-version)))
    (message "[flycheck-biomejs] connect to: %s" socket-path)
    (add-hook 'kill-emacs-hook #'flycheck-biomejs/jsonrpc-shutdown)
    ;; (message "[flycheck-biomejs] vcs-root-path: %s" vcs-root-path)
    ;; (message "[flycheck-biomejs] manifest-path: %s" manifest-path)
    ;; (message "[flycheck-biomejs] config-path: %s" config-path)
    ;; (jsonrpc-request conn "biome/open_project" `(:content ,manifest-content :path (:path ,manifest-path) :version 1))
    ;; (jsonrpc-request conn "biome/update_current_project" `(:path (:path ,manifest-path)))
    conn))

(defun flycheck-biomejs/jsonrpc-open-file (conn)
  "Send file open information to biome server.

CONN is a json-rpc connection."
  (let* ((src-path (buffer-file-name))
         (src-content (buffer-substring-no-properties (point-min) (point-max))))
    (jsonrpc-request conn "biome/open_file" `(:content
                                              ,src-content
                                              :path (:path ,src-path)
                                              :version 0))
    (message "[flycheck-biomejs] open file: %s" src-path)
    (add-to-list 'flycheck-biomejs/buffer-list src-path)
    (add-hook 'kill-buffer-hook #'flycheck-biomejs/jsonrpc-close-file nil t)))

(defun flycheck-biomejs/jsonrpc-request-diagnostics (conn)
  "Retrieve and return diagnostic information from the biome server.

CONN is a json-rpc connection."
  (let* ((src-path (buffer-file-name))
         (src-content (buffer-substring-no-properties (point-min) (point-max))))
    (unless flycheck-biomejs/vcs-root-path
      (setq flycheck-biomejs/vcs-root-path (flycheck-biomejs/get-vcs-root-path)))
    (unless flycheck-biomejs/config-path
      (setq flycheck-biomejs/config-path (flycheck-biomejs/get-config-path)))
    (unless flycheck-biomejs/config-object
      (setq flycheck-biomejs/config-object (flycheck-biomejs/get-config-object flycheck-biomejs/config-path)))
    (jsonrpc-request conn "biome/update_settings" `(:configuration
                                                    ,flycheck-biomejs/config-object
                                                    :gitignore_matches [] ;; TODO: set gitignore_matches
                                                    :vcs_base_path ,flycheck-biomejs/vcs-root-path
                                                    :working_directory ,flycheck-biomejs/vcs-root-path))
    (if (seq-contains-p flycheck-biomejs/buffer-list src-path)
        (jsonrpc-request conn "biome/change_file" `(:content
                                                    ,src-content
                                                    :path (:path ,src-path)
                                                    :version 0))
      (flycheck-biomejs/jsonrpc-open-file conn))
    (jsonrpc-request conn "biome/pull_diagnostics" `(:categories
                                                     ["Syntax" "Lint"]
                                                     :max_diagnostics 100
                                                     :path (:path ,src-path)))))

(defun flycheck-biomejs/jsonrpc-close-file ()
  "Send file close information to biome server."
  (let ((src-path (buffer-file-name))
        (conn flycheck-biomejs/connection))
    (flycheck-mode -1) ;; close fileのあとにstart functionが実行されないように
    (when (jsonrpc-connection-p conn)
      (jsonrpc-request conn "biome/close_file" `(:path (:path ,src-path))))
    (message "[flycheck-biomejs] close file: %s" src-path)
    (setq flycheck-biomejs/buffer-list (delete src-path flycheck-biomejs/buffer-list))
    (when (length= flycheck-biomejs/buffer-list 0)
      (flycheck-biomejs/jsonrpc-shutdown))))

(defun flycheck-biomejs/jsonrpc-shutdown ()
  "Shutdown the biome server."
  (when flycheck-biomejs/connection
    (jsonrpc-shutdown flycheck-biomejs/connection)
    (kill-buffer (jsonrpc-events-buffer flycheck-biomejs/connection))
    (message "[flycheck-biomejs] shutdown connection")
    (setq flycheck-biomejs/connection nil)))

(defun flycheck-biomejs/parse-errors (diagnostics checker)
  "Convert biome diagnostic information to flycheck errors.

DIAGNOSTICS: biome/pull_diagnosticsの結果.
CHECKER: flycheck-define-generic-checkerの実装を参照."
  (when (plist-member diagnostics :diagnostics)
    (let ((diags (plist-get diagnostics :diagnostics))
          (_errors (plist-get diagnostics :errors)))
      (mapcar (lambda (diag)
                (let* ((location (plist-get diag :location))
                       ;; :path => :file
                       (_path (nth 1 (plist-get location :path)))
                       ;; :span [start-pos, end-pos]
                       (start-pos (aref (plist-get location :span) 0))
                       (end-pos (aref (plist-get location :span) 1)))
                  (flycheck-error-new-at-pos (1+ start-pos)
                                             (pcase (plist-get diag :severity)
                                               ((or "error" "fatal") 'error)
                                               ((or "information" "hint") 'info)
                                               ((or "warning" _) 'warning))
                                             (format "%s(%s)" (plist-get diag :description) (plist-get diag :category))
                                             :end-pos (1+ end-pos)
                                             :checker checker)))
              diags))))

(defun flycheck-biomejs/verify (_checker)
  "Verify flycheck-biomejs."
  (let* ((command (executable-find "biome"))
         (version (when command (flycheck-biomejs/get-version command)))
         (version-p (when version (flycheck-biomejs/check-version version "1.6.0")))
         (config-path (and buffer-file-name (flycheck-biomejs/get-config-path))))
    (list
     (flycheck-verification-result-new
      :label "biome command"
      :message (if command (format "Found at %s" command) "Not found")
      :face (if command 'success '(bold error)))
     (flycheck-verification-result-new
      :label "biome config file"
      :message (if config-path (format "Found at %s" config-path) "Not found")
      :face (if config-path 'success '(bold error)))
     (flycheck-verification-result-new
      :label "biome version"
      :message (if version-p (format "%s" version) (format "%S" version))
      :face (if version-p 'success '(bold error))))))

(defun flycheck-biomejs/start (checker callback)
  "Experimental function.

CHECKER and CALLBACK: flycheck-define-generic-checkerの実装を参照."
  (condition-case err
      (let* ((conn (if flycheck-biomejs/connection flycheck-biomejs/connection (flycheck-biomejs/jsonrpc-connect-and-initialize)))
             (output (flycheck-biomejs/jsonrpc-request-diagnostics conn))
             (errors (flycheck-biomejs/parse-errors output checker)))
        (unless flycheck-biomejs/connection
          (setq flycheck-biomejs/connection conn))
        (funcall callback 'finished errors))
    (error (funcall callback 'errored (error-message-string err)))))

(flycheck-define-generic-checker 'javascript-biome
  "A code linter for JavaScript/TypeScript."
  :start #'flycheck-biomejs/start
  :verify #'flycheck-biomejs/verify
  :modes '(js-mode js2-mode js3-mode typescript-mode typescript-ts-mode typescript-tsx-mode)
  :predicate (lambda () (and buffer-file-name (flycheck-biomejs/get-config-path))))

(add-to-list 'flycheck-checkers 'javascript-biome)

(provide 'flycheck-biomejs)
;;; flycheck-biomejs.el ends here

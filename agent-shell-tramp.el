;;; agent-shell-tramp.el --- running agent-shell on remote machine using tramp -*- lexical-binding: t -*-

;; Copyright (C) 2026 Junyi Hou

;; Author: Junyi Hou <junyi.yi.hou@gmail.com>
;; URL: https://github.com/junyi-hou/agent-shell-tramp
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.50.1") (acp "0.11.1"))

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; This package provides TRAMP support for `agent-shell`.
;; It allows `agent-shell` to run processes on remote machines
;; using Emacs TRAMP functionality.
;;
;; To use it, enable `agent-shell-tramp-mode`. This will
;; automatically add advice to `agent-shell` and `acp` to
;; handle remote file paths and process execution.
;;
;; Usage:
;;   (require 'agent-shell-tramp)
;;   (agent-shell-tramp-mode 1)
;;
;; Then, when you are in a buffer visiting a remote file via TRAMP,
;; running `agent-shell' will work as expected on the remote host.

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'acp)
(require 'tramp)
(require 'agent-shell)

(defgroup agent-shell-tramp nil
  "TRAMP support for agent-shell."
  :group 'agent-shell
  :prefix "agent-shell-tramp-")

(defvar tramp-use-ssh-controlmaster-options)
(defvar tramp-ssh-controlmaster-options)

(declare-function agent-shell-cwd "agent-shell")
(declare-function tramp-tramp-file-p "tramp")
(declare-function tramp-dissect-file-name "tramp")
(declare-function tramp-file-name-localname "tramp")
(declare-function tramp-make-tramp-file-name "tramp")

(defvar agent-shell-path-resolver-function)

;; handling environment variables
(defun agent-shell-tramp--prepare-env-var (var)
  "Format environment variables VAR for shell export.
VAR should be a list of \"KEY=VALUE\" strings.
Returns a list of \"export KEY='VALUE'\" strings with properly quoted values."
  (mapcar
   (lambda (env)
     (let* ((idx (string-search "=" env))
            (key (substring env 0 idx))
            (value (substring env (1+ idx))))
       (format "export %s=%s" key (shell-quote-argument value))))
   var))

(defun agent-shell-tramp--make-acp-client (orig-fn &rest args)
  "Advice for `agent-shell--make-acp-client' to support TRAMP."
  (let* ((buffer (plist-get args :context-buffer))
         (cwd
          (with-current-buffer (or buffer (current-buffer))
            (agent-shell-cwd))))
    (if (and cwd (tramp-tramp-file-p cwd))
        (let* ((vec (tramp-dissect-file-name cwd))
               (host (tramp-file-name-host vec))
               (user (tramp-file-name-user vec))
               (port (tramp-file-name-port vec))
               (method (tramp-file-name-method vec))
               (command (plist-get args :command))
               (command-params (plist-get args :command-params))
               (env-vars
                (agent-shell-tramp--prepare-env-var
                 (plist-get args :environment-variables))))
          (unless (member method '("ssh" "scp" "rpc" nil))
            (error "TRAMP method '%s' not supported; only SSH/RPC is supported" method))
          (when (tramp-file-name-hop vec)
            (error "Multi-hop TRAMP paths not supported"))
          (let* ((ssh-dest
                  (if user
                      (format "%s@%s" user host)
                    host))
                 (command-list
                  (seq-filter
                   #'identity
                   `("ssh" ,(when port
                        (list "-p" port))
                     ,ssh-dest
                     ,(format "bash -lc \"%s; %s\""
                              (string-join env-vars " ")
                              (string-join (append (list command) command-params)
                                           " ")))))
                 (args (plist-put args :command (car command-list)))
                 (args (plist-put args :command-params (cdr command-list))))
            (apply orig-fn args)))
      (apply orig-fn args))))

(defun agent-shell-tramp-resolve-path (path)
  "Resolve PATH for TRAMP compatibility.
If in a TRAMP context:
- If PATH is already a TRAMP path, return its local part.
- If PATH is a local path on the remote, return it as a full TRAMP path.
If not in a TRAMP context, return PATH unchanged."
  (let* ((cwd (agent-shell-cwd))
         (tramp-vec (and (tramp-tramp-file-p cwd) (tramp-dissect-file-name cwd))))
    (cond
     ;; Path is already a TRAMP path - strip the prefix for the agent
     ((tramp-tramp-file-p path)
      (tramp-file-name-localname (tramp-dissect-file-name path)))
     ;; Path is a remote-local path - add TRAMP prefix for Emacs
     (tramp-vec
      (tramp-make-tramp-file-name tramp-vec path))
     ;; Not in a TRAMP context
     (t
      path))))

(defun agent-shell-tramp--transcript-dir (cwd)
  "Return the local transcript directory corresponding to remote CWD.
Returns nil if CWD is not a TRAMP path.
Ensures the directory exists before returning."
  (when (and (fboundp 'tramp-tramp-file-p) (tramp-tramp-file-p cwd))
    (let* ((vec (tramp-dissect-file-name cwd))
           (host (tramp-file-name-host vec))
           (localname (tramp-file-name-localname vec))
           (safe-path
            (replace-regexp-in-string "/" "_" (string-trim localname "/" "/"))))
      ;; make sure that the directory exists
      (let ((transcript-dir
             (expand-file-name (format ".agent-shell/transcripts/%s/%s" host safe-path)
                               (expand-file-name "~"))))
        (make-directory transcript-dir t)
        transcript-dir))))

(defun agent-shell-tramp-transcript-dir ()
  "Generate a local file path for storing the session transcript.
If the current context is remote, it uses a host-specific local directory.
Otherwise, it uses the standard .agent-shell/transcripts directory relative
to the current working directory."
  (if-let* ((cwd (agent-shell-cwd))
            (dir (agent-shell-tramp--transcript-dir cwd)))
    (expand-file-name (format-time-string "%F-%H-%M-%S.md") dir)
    (agent-shell--default-transcript-file-path)))

(defvar agent-shell-tramp--orig-path-resolver-function nil)
(defvar agent-shell-tramp--orig-transcript-file-path-function nil)

;;;###autoload
(define-minor-mode agent-shell-tramp-mode
  "Minor mode to enable agent-shell TRAMP remote support."
  :global t
  :group
  'agent-shell-tramp
  (if agent-shell-tramp-mode
      (progn
        (advice-add
         #'agent-shell--make-acp-client
         :around #'agent-shell-tramp--make-acp-client)

        (setq
         agent-shell-tramp--orig-path-resolver-function
         agent-shell-path-resolver-function
         agent-shell-tramp--orig-transcript-file-path-function agent-shell-transcript-file-path-function

         ;; update values
         agent-shell-path-resolver-function #'agent-shell-tramp-resolve-path
         agent-shell-transcript-file-path-function #'agent-shell-tramp-transcript-dir))
    ;; restore values
    (setq
     agent-shell-path-resolver-function agent-shell-tramp--orig-path-resolver-function
     agent-shell-transcript-file-path-function agent-shell-tramp--orig-transcript-file-path-function

     agent-shell-tramp--orig-transcript-file-path-function nil
     agent-shell-tramp--orig-path-resolver-function nil)

    (advice-remove
     #'agent-shell--make-acp-client #'agent-shell-tramp--make-acp-client)))

(provide 'agent-shell-tramp)
;;; agent-shell-tramp.el ends here

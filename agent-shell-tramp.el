;;; agent-shell-tramp.el --- TRAMP support for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junyi Hou
;; Copyright (C) 2026 Clay Sheaff

;; Author: Junyi Hou <junyi.yi.hou@gmail.com>
;; URL: https://github.com/junyi-hou/agent-shell-tramp
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.51.1") (acp "0.12.1"))

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
;; This package connects `agent-shell' to Emacs TRAMP paths.
;;
;; Process startup depends on ACP honoring Emacs file handlers.  Use a recent
;; acp.el build that passes `:file-handler' when starting ACP clients.

;;; Code:

(require 'acp)
(require 'agent-shell)
(require 'subr-x)
(require 'tramp)

(defgroup agent-shell-tramp nil
  "TRAMP support for agent-shell."
  :group 'agent-shell
  :prefix "agent-shell-tramp-")

(defcustom agent-shell-tramp-transcript-directory
  (expand-file-name "~/.agent-shell/transcripts/")
  "Local root directory for transcripts from remote agent-shell sessions."
  :type 'directory
  :group 'agent-shell-tramp)

(declare-function agent-shell-cwd "agent-shell")
(declare-function agent-shell--current-shell "agent-shell")
(declare-function tramp-dissect-file-name "tramp")
(declare-function tramp-file-name-host "tramp")
(declare-function tramp-file-name-localname "tramp")
(declare-function tramp-file-name-method "tramp")
(declare-function tramp-file-name-user "tramp")
(declare-function tramp-make-tramp-file-name "tramp")

(defvar agent-shell-tramp--enabled nil)
(defvar agent-shell-tramp--orig-path-resolver-function nil)
(defvar agent-shell-tramp--orig-transcript-file-path-function nil)

(defun agent-shell-tramp--check-acp-support ()
  "Signal a helpful error if acp.el lacks required file-handler support."
  (when (version< acp-package-version "0.12.1")
    (user-error "agent-shell-tramp requires acp.el 0.12.1 or newer")))

(defun agent-shell-tramp--dissect (filename)
  "Return TRAMP vector for FILENAME, or nil when FILENAME is not remote."
  (when (and (stringp filename)
             (ignore-errors
               (file-remote-p filename)))
    (tramp-dissect-file-name filename)))

(defun agent-shell-tramp--current-vec ()
  "Return TRAMP vector for the current `agent-shell' working directory."
  (when-let ((cwd
              (ignore-errors
                (agent-shell-cwd))))
    (agent-shell-tramp--dissect cwd)))

(defun agent-shell-tramp-resolve-path (path)
  "Resolve PATH between TRAMP and remote-local formats.

When `agent-shell-cwd' is remote:
- TRAMP paths become remote-local paths for the agent.
- Remote-local paths become TRAMP paths for Emacs file handlers.

When `agent-shell-cwd' is local, pass PATH through the original
`agent-shell-path-resolver-function' so symlink resolution and other
custom resolvers are preserved."
  (if-let ((vec (agent-shell-tramp--current-vec)))
    (cond
     ((agent-shell-tramp--dissect path)
      (tramp-file-name-localname (tramp-dissect-file-name path)))
     ((stringp path)
      (tramp-make-tramp-file-name vec path))
     (t
      path))
    (if agent-shell-tramp--orig-path-resolver-function
        (funcall agent-shell-tramp--orig-path-resolver-function path)
      path)))

(defun agent-shell-tramp--safe-component (string &optional fallback)
  "Return STRING sanitized for use as a single path component.
Use FALLBACK when STRING is nil or empty."
  (let ((value
         (if (and string (not (string-empty-p string)))
             string
           (or fallback "unknown"))))
    (replace-regexp-in-string "[^[:alnum:].@_-]" "_" value)))

(defun agent-shell-tramp--shorten (string max-length)
  "Return STRING truncated to MAX-LENGTH characters."
  (if (> (length string) max-length)
      (substring string 0 max-length)
    string))

(defun agent-shell-tramp--transcript-dir ()
  "Return local transcript directory for the current remote session."
  (when-let* ((cwd
               (ignore-errors
                 (agent-shell-cwd)))
              (vec (agent-shell-tramp--dissect cwd)))
    (let* ((method
            (agent-shell-tramp--safe-component (tramp-file-name-method vec) "tramp"))
           (user-host
            (agent-shell-tramp--safe-component
             (format "%s@%s"
                     (or (tramp-file-name-user vec) "default")
                     (or (tramp-file-name-host vec) "unknown"))))
           (localname (or (tramp-file-name-localname vec) "/"))
           (raw-slug (string-trim localname "/" "/"))
           (slug
            (agent-shell-tramp--shorten
             (agent-shell-tramp--safe-component raw-slug "root") 80))
           (hash (substring (secure-hash 'sha1 cwd) 0 12)))
      (file-name-concat agent-shell-tramp-transcript-directory
                        method
                        user-host
                        (format "%s-%s" slug hash)))))

(defun agent-shell-tramp-transcript-file-path ()
  "Return local transcript file path for current remote session, or nil."
  (if-let* ((dir (agent-shell-tramp--transcript-dir)))
    (progn
      (make-directory dir t)
      (expand-file-name (format-time-string "%F-%H-%M-%S.md") dir))
    (agent-shell--default-transcript-file-path)))

(defun agent-shell-tramp--markdown-fence (text)
  "Return a Markdown fence longer than any backtick run in TEXT."
  (let ((start 0)
        (longest 0))
    (while (string-match "`+" text start)
      (setq longest (max longest (- (match-end 0) (match-beginning 0))))
      (setq start (match-end 0)))
    (make-string (max 3 (1+ longest)) ?`)))

(defun agent-shell-tramp--insert-shell-command-output (command &rest args)
  "Call COMMAND with ARGS, handling remote agent directories synchronously."
  (if-let* ((shell-buffer (agent-shell--current-shell))
            (directory (buffer-local-value 'default-directory shell-buffer))
            ((file-remote-p directory)))
      (let* ((shell-command (read-string "insert command output: "))
             (process-command
              (with-current-buffer shell-buffer
                (agent-shell--build-command-for-execution
                 (list shell-file-name
                       shell-command-switch
                       (format "%s 2>&1" shell-command)))))
             (output
              (let ((default-directory directory))
                (with-temp-buffer
                  (insert "$ " shell-command "\n\n")
                  (apply #'process-file
                         (car process-command) nil t nil (cdr process-command))
                  (buffer-string))))
             (fence (agent-shell-tramp--markdown-fence output))
             (code-block (format "%sshell
%s
%s" fence output fence)))
        (if (with-current-buffer shell-buffer (shell-maker-busy))
            (with-current-buffer shell-buffer
              (agent-shell-prompt-queue
               (agent-shell--prompt-queue-read
                :initial (concat code-block "\n\n"))))
          (agent-shell-insert :text (concat code-block "\n\n")
                              :no-focus t
                              :shell-buffer shell-buffer)))
    (apply command args)))

(defun agent-shell-tramp--enable ()
  "Enable agent-shell TRAMP integration."
  (agent-shell-tramp--check-acp-support)
  (unless agent-shell-tramp--enabled
    (setq
     agent-shell-tramp--orig-path-resolver-function agent-shell-path-resolver-function
     agent-shell-tramp--orig-transcript-file-path-function agent-shell-transcript-file-path-function
     agent-shell-path-resolver-function #'agent-shell-tramp-resolve-path
     agent-shell-transcript-file-path-function #'agent-shell-tramp-transcript-file-path)
    (advice-add 'agent-shell-insert-shell-command-output
                :around #'agent-shell-tramp--insert-shell-command-output)
    (setq agent-shell-tramp--enabled t)))

(defun agent-shell-tramp--disable ()
  "Disable agent-shell TRAMP integration."
  (when agent-shell-tramp--enabled
    (setq
     agent-shell-path-resolver-function agent-shell-tramp--orig-path-resolver-function
     agent-shell-transcript-file-path-function agent-shell-tramp--orig-transcript-file-path-function
     agent-shell-tramp--orig-path-resolver-function nil
     agent-shell-tramp--orig-transcript-file-path-function nil)
    (advice-remove 'agent-shell-insert-shell-command-output
                   #'agent-shell-tramp--insert-shell-command-output)
    (setq agent-shell-tramp--enabled nil)))

;;;###autoload
(define-minor-mode agent-shell-tramp-mode
  "Toggle TRAMP support for agent-shell.

When enabled, `agent-shell' resolves remote file paths through TRAMP and
stores transcripts for remote sessions locally."
  :global t
  :group
  'agent-shell-tramp
  (if agent-shell-tramp-mode
      (agent-shell-tramp--enable)
    (agent-shell-tramp--disable)))

(provide 'agent-shell-tramp)
;;; agent-shell-tramp.el ends here

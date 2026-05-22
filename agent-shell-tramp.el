;;; agent-shell-tramp.el --- TRAMP support for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junyi Hou
;; Copyright (C) 2026 Clay Sheaff

;; Author: Junyi Hou <junyi.yi.hou@gmail.com>
;; URL: https://github.com/junyi-hou/agent-shell-tramp
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.51.1") (acp "0.11.3"))

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
(require 'cl-lib)
(require 'subr-x)
(require 'tramp)

(defgroup agent-shell-tramp nil
  "TRAMP support for agent-shell."
  :group 'agent-shell
  :prefix "agent-shell-tramp-")

(defcustom agent-shell-tramp-require-acp-file-handler-support t
  "When non-nil, refuse to enable without acp.el file-handler support."
  :type 'boolean
  :group 'agent-shell-tramp)

(defcustom agent-shell-tramp-transcript-directory
  (expand-file-name "~/.agent-shell/transcripts/")
  "Local root directory for transcripts from remote agent-shell sessions."
  :type 'directory
  :group 'agent-shell-tramp)

(defcustom agent-shell-tramp-remote-shell nil
  "Obsolete.  Process startup is now delegated to TRAMP file handlers."
  :type '(choice (const :tag "Unused" nil) string)
  :group 'agent-shell-tramp)

(make-obsolete-variable
 'agent-shell-tramp-remote-shell
 "process startup is delegated to TRAMP file handlers instead"
 "0.2.0")

(defvar acp-file-handler-process-support)

(declare-function agent-shell-cwd "agent-shell")
(declare-function tramp-dissect-file-name "tramp")
(declare-function tramp-file-name-host "tramp")
(declare-function tramp-file-name-localname "tramp")
(declare-function tramp-file-name-method "tramp")
(declare-function tramp-file-name-user "tramp")
(declare-function tramp-make-tramp-file-name "tramp")

(defvar agent-shell-tramp--enabled nil)

(defun agent-shell-tramp--tree-member-p (needle tree)
  "Return non-nil if NEEDLE appears in TREE."
  (cond
   ((eq needle tree) t)
   ((consp tree)
    (or (agent-shell-tramp--tree-member-p needle (car tree))
        (agent-shell-tramp--tree-member-p needle (cdr tree))))
   ((byte-code-function-p tree)
    (string-match-p (regexp-quote (format "%S" needle))
                    (prin1-to-string tree)))
   ((vectorp tree)
    (cl-loop for item across tree
             thereis (agent-shell-tramp--tree-member-p needle item)))
   (t nil)))

(defun agent-shell-tramp--acp-source-has-file-handler-p ()
  "Return non-nil if installed acp.el source passes `:file-handler'."
  (when-let ((file (locate-library "acp.el")))
    (with-temp-buffer
      (insert-file-contents file nil nil nil t)
      (goto-char (point-min))
      (when (re-search-forward "^(cl-defun acp--start-client\\_>" nil t)
        (let ((end (save-excursion
                     (or (and (re-search-forward "^(\\(?:cl-\\)?defun " nil t)
                              (match-beginning 0))
                         (point-max)))))
          (re-search-forward ":file-handler" end t))))))

(defun agent-shell-tramp-acp-file-handler-support-p ()
  "Return non-nil when loaded acp.el can start clients via file handlers."
  (or (and (boundp 'acp-file-handler-process-support)
           acp-file-handler-process-support)
      (and (fboundp 'acp--start-client)
           (and (agent-shell-tramp--tree-member-p
                 :file-handler
                 (symbol-function 'acp--start-client))
                t))
      (and (agent-shell-tramp--acp-source-has-file-handler-p) t)))

(defun agent-shell-tramp--check-acp-support ()
  "Signal a helpful error if acp.el lacks required file-handler support."
  (when (and agent-shell-tramp-require-acp-file-handler-support
             (not (agent-shell-tramp-acp-file-handler-support-p)))
    (user-error "agent-shell-tramp requires acp.el file-handler process support")))

(defun agent-shell-tramp--dissect (filename)
  "Return TRAMP vector for FILENAME, or nil when FILENAME is not remote."
  (when (and (stringp filename)
             (ignore-errors (file-remote-p filename)))
    (tramp-dissect-file-name filename)))

(defun agent-shell-tramp--current-vec ()
  "Return TRAMP vector for the current `agent-shell' working directory."
  (when-let ((cwd (ignore-errors (agent-shell-cwd))))
    (agent-shell-tramp--dissect cwd)))

(defun agent-shell-tramp-resolve-path (path)
  "Resolve PATH between TRAMP and remote-local formats.

When `agent-shell-cwd' is remote:
- TRAMP paths become remote-local paths for the agent.
- Absolute remote-local paths become TRAMP paths for Emacs file handlers.

When `agent-shell-cwd' is local, return PATH unchanged."
  (if-let ((vec (agent-shell-tramp--current-vec)))
      (cond
       ((agent-shell-tramp--dissect path)
        (tramp-file-name-localname (tramp-dissect-file-name path)))
       ((and (stringp path)
             (file-name-absolute-p path))
        (tramp-make-tramp-file-name vec path))
       (t path))
    path))

(defun agent-shell-tramp--resolve-path-around (orig-fn path)
  "Around advice for `agent-shell-path-resolver-function'.
ORIG-FN is the previous resolver and PATH is the path to resolve."
  (let ((resolved (agent-shell-tramp-resolve-path path)))
    (if (equal resolved path)
        (if orig-fn (funcall orig-fn path) path)
      resolved)))

(defun agent-shell-tramp--safe-component (string &optional fallback)
  "Return STRING sanitized for use as a single path component.
Use FALLBACK when STRING is nil or empty."
  (let ((value (if (and string (not (string-empty-p string)))
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
  (when-let* ((cwd (ignore-errors (agent-shell-cwd)))
              (vec (agent-shell-tramp--dissect cwd)))
    (let* ((method (agent-shell-tramp--safe-component
                    (tramp-file-name-method vec) "tramp"))
           (user-host (agent-shell-tramp--safe-component
                       (format "%s@%s"
                               (or (tramp-file-name-user vec) "default")
                               (or (tramp-file-name-host vec) "unknown"))))
           (localname (or (tramp-file-name-localname vec) "/"))
           (raw-slug (string-trim localname "/" "/"))
           (slug (agent-shell-tramp--shorten
                  (agent-shell-tramp--safe-component raw-slug "root")
                  80))
           (hash (substring (secure-hash 'sha1 cwd) 0 12)))
      (file-name-concat agent-shell-tramp-transcript-directory
                        method
                        user-host
                        (format "%s-%s" slug hash)))))

(defun agent-shell-tramp-transcript-file-path ()
  "Return local transcript file path for current remote session, or nil."
  (when-let ((dir (agent-shell-tramp--transcript-dir)))
    (make-directory dir t)
    (expand-file-name (format-time-string "%F-%H-%M-%S.md") dir)))

(defun agent-shell-tramp--transcript-file-path-around (orig-fn)
  "Around advice for `agent-shell-transcript-file-path-function'.
ORIG-FN is the previous transcript file path function."
  (or (agent-shell-tramp-transcript-file-path)
      (when orig-fn (funcall orig-fn))))

(defun agent-shell-tramp--enable ()
  "Enable agent-shell TRAMP integration."
  (agent-shell-tramp--check-acp-support)
  (when agent-shell-tramp-remote-shell
    (message "`agent-shell-tramp-remote-shell' is obsolete and no longer used"))
  (unless agent-shell-tramp--enabled
    (add-function :around
                  (default-value 'agent-shell-path-resolver-function)
                  #'agent-shell-tramp--resolve-path-around)
    (add-function :around
                  (default-value 'agent-shell-transcript-file-path-function)
                  #'agent-shell-tramp--transcript-file-path-around)
    (setq agent-shell-tramp--enabled t)))

(defun agent-shell-tramp--disable ()
  "Disable agent-shell TRAMP integration."
  (when agent-shell-tramp--enabled
    (remove-function (default-value 'agent-shell-path-resolver-function)
                     #'agent-shell-tramp--resolve-path-around)
    (remove-function (default-value 'agent-shell-transcript-file-path-function)
                     #'agent-shell-tramp--transcript-file-path-around)
    (setq agent-shell-tramp--enabled nil)))

;;;###autoload
(define-minor-mode agent-shell-tramp-mode
  "Toggle TRAMP support for agent-shell.

When enabled, `agent-shell' resolves remote file paths through TRAMP and
stores transcripts for remote sessions locally."
  :global t
  :group 'agent-shell-tramp
  (if agent-shell-tramp-mode
      (agent-shell-tramp--enable)
    (agent-shell-tramp--disable)))

(provide 'agent-shell-tramp)
;;; agent-shell-tramp.el ends here

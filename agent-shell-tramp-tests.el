;;; agent-shell-tramp-tests.el --- Tests for agent-shell-tramp -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-tramp)

(defmacro agent-shell-tramp-tests--with-cwd (cwd &rest body)
  "Run BODY with `agent-shell-cwd' returning CWD."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-shell-cwd)
              (lambda () ,cwd)))
     ,@body))

(ert-deftest agent-shell-tramp-resolve-tramp-to-remote-local ()
  "TRAMP paths are stripped before being sent to the remote agent."
  (agent-shell-tramp-tests--with-cwd "/ssh:user@host:/home/user/project/"
    (should (equal (agent-shell-tramp-resolve-path
                    "/ssh:user@host:/home/user/project/file.el")
                   "/home/user/project/file.el"))))

(ert-deftest agent-shell-tramp-resolve-remote-local-to-tramp ()
  "Remote-local absolute paths are wrapped for Emacs file handlers."
  (agent-shell-tramp-tests--with-cwd "/ssh:user@host:/home/user/project/"
    (let ((resolved (agent-shell-tramp-resolve-path
                     "/home/user/project/file.el")))
      (should (equal (file-remote-p resolved) "/ssh:user@host:"))
      (should (equal (tramp-file-name-localname
                      (tramp-dissect-file-name resolved))
                     "/home/user/project/file.el")))))

(ert-deftest agent-shell-tramp-resolve-relative-path-is-unchanged ()
  "Relative paths are left untouched in remote sessions."
  (agent-shell-tramp-tests--with-cwd "/ssh:user@host:/home/user/project/"
    (should (equal (agent-shell-tramp-resolve-path "src/file.el")
                   "src/file.el"))))

(ert-deftest agent-shell-tramp-resolve-local-context-is-identity ()
  "Local sessions do not resolve paths."
  (agent-shell-tramp-tests--with-cwd "/tmp/project/"
    (should (equal (agent-shell-tramp-resolve-path "/tmp/project/file.el")
                   "/tmp/project/file.el"))))

(ert-deftest agent-shell-tramp-resolver-around-composes ()
  "Around resolver calls the previous resolver for paths it does not handle."
  (agent-shell-tramp-tests--with-cwd "/tmp/project/"
    (should (equal (agent-shell-tramp--resolve-path-around
                    (lambda (path) (concat "orig:" path))
                    "file.el")
                   "orig:file.el"))))

(ert-deftest agent-shell-tramp-acp-support-detects-upstream-file-handler ()
  "ACP support detection accepts upstream `:file-handler' implementation."
  (let ((acp-file-handler-process-support nil))
    (cl-letf (((symbol-function 'acp--start-client)
               (lambda ()
                 (make-process :name "test"
                               :command '("true")
                               :file-handler t))))
      (should (agent-shell-tramp-acp-file-handler-support-p)))))

(ert-deftest agent-shell-tramp-transcript-path-is-local ()
  "Remote transcripts are stored in a local directory."
  (let ((agent-shell-tramp-transcript-directory
         (expand-file-name "agent-shell-tramp-test/" temporary-file-directory)))
    (agent-shell-tramp-tests--with-cwd "/ssh:user@host:/home/user/project/"
      (let ((path (agent-shell-tramp-transcript-file-path)))
        (should path)
        (should-not (file-remote-p path))
        (should (file-directory-p (file-name-directory path)))
        (should (string-match-p "/ssh/user@host/" path))))))

(ert-deftest agent-shell-tramp-mode-installs-and-removes-wrappers ()
  "Global mode composes with existing resolver and transcript functions."
  (let ((agent-shell-path-resolver-function
         (default-value 'agent-shell-path-resolver-function))
        (agent-shell-transcript-file-path-function
         (default-value 'agent-shell-transcript-file-path-function))
        (agent-shell-tramp-require-acp-file-handler-support nil))
    (unwind-protect
        (progn
          (setq-default agent-shell-path-resolver-function #'identity)
          (setq-default agent-shell-transcript-file-path-function
                        (lambda () "/tmp/local.md"))
          (agent-shell-tramp-mode 1)
          (should agent-shell-tramp--enabled)
          (agent-shell-tramp-mode -1)
          (should-not agent-shell-tramp--enabled)
          (should (eq (default-value 'agent-shell-path-resolver-function)
                      #'identity)))
      (setq-default agent-shell-path-resolver-function
                    agent-shell-path-resolver-function)
      (setq-default agent-shell-transcript-file-path-function
                    agent-shell-transcript-file-path-function)
      (setq agent-shell-tramp--enabled nil))))

(ert-deftest agent-shell-tramp-mode-enable-is-idempotent ()
  "Enabling the mode repeatedly does not stack duplicate wrappers."
  (let ((agent-shell-path-resolver-function
         (default-value 'agent-shell-path-resolver-function))
        (agent-shell-transcript-file-path-function
         (default-value 'agent-shell-transcript-file-path-function))
        (agent-shell-tramp-require-acp-file-handler-support nil))
    (unwind-protect
        (agent-shell-tramp-tests--with-cwd "/tmp/project/"
          (setq-default agent-shell-path-resolver-function
                        (lambda (path) (concat "orig:" path)))
          (setq-default agent-shell-transcript-file-path-function
                        (lambda () "/tmp/local.md"))
          (agent-shell-tramp-mode 1)
          (agent-shell-tramp-mode 1)
          (should (equal (funcall (default-value
                                   'agent-shell-path-resolver-function)
                                  "file.el")
                         "orig:file.el")))
      (agent-shell-tramp-mode -1)
      (setq-default agent-shell-path-resolver-function
                    agent-shell-path-resolver-function)
      (setq-default agent-shell-transcript-file-path-function
                    agent-shell-transcript-file-path-function)
      (setq agent-shell-tramp--enabled nil))))

(provide 'agent-shell-tramp-tests)
;;; agent-shell-tramp-tests.el ends here

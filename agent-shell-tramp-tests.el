;;; agent-shell-tramp-tests.el --- Tests for agent-shell-tramp -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-tramp)

(defmacro agent-shell-tramp-tests--with-cwd (cwd &rest body)
  "Run BODY with `agent-shell-cwd' returning CWD."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-shell-cwd) (lambda () ,cwd)))
     ,@body))

(ert-deftest agent-shell-tramp-resolve-tramp-to-remote-local ()
  "TRAMP paths are stripped before being sent to the remote agent."
  (agent-shell-tramp-tests--with-cwd "/ssh:user@host:/home/user/project/"
    (should
     (equal
      (agent-shell-tramp-resolve-path
       "/ssh:user@host:/home/user/project/file.el")
      "/home/user/project/file.el"))))

(ert-deftest agent-shell-tramp-resolve-remote-local-to-tramp ()
  "Remote-local absolute paths are wrapped for Emacs file handlers."
  (agent-shell-tramp-tests--with-cwd "/ssh:user@host:/home/user/project/"
    (let ((resolved (agent-shell-tramp-resolve-path "/home/user/project/file.el")))
      (should (equal (file-remote-p resolved) "/ssh:user@host:"))
      (should
       (equal
        (tramp-file-name-localname (tramp-dissect-file-name resolved))
        "/home/user/project/file.el")))))

(ert-deftest agent-shell-tramp-resolve-relative-path-to-tramp ()
  "Relative paths are wrapped for Emacs file handlers in remote sessions."
  (agent-shell-tramp-tests--with-cwd "/ssh:user@host:/home/user/project/"
    (let ((resolved (agent-shell-tramp-resolve-path "src/file.el")))
      (should (equal (file-remote-p resolved) "/ssh:user@host:"))
      (should
       (equal
        (tramp-file-name-localname (tramp-dissect-file-name resolved))
        "src/file.el")))))

(ert-deftest agent-shell-tramp-resolve-local-context-is-identity ()
  "Local sessions do not resolve paths when no original resolver is set."
  (agent-shell-tramp-tests--with-cwd "/tmp/project/"
    (should
     (equal
      (agent-shell-tramp-resolve-path "/tmp/project/file.el") "/tmp/project/file.el"))))

(ert-deftest agent-shell-tramp-resolve-local-context-chains-to-original ()
  "Local sessions pass paths through the original resolver."
  (let ((agent-shell-path-resolver-function agent-shell-path-resolver-function))
    (unwind-protect
        (progn
          (setq agent-shell-path-resolver-function (lambda (path) (concat "resolved:" path)))
          (agent-shell-tramp-mode 1)
          (agent-shell-tramp-tests--with-cwd "/tmp/project/"
            (should
             (equal
              (agent-shell-tramp-resolve-path "/tmp/project/file.el")
              "resolved:/tmp/project/file.el"))))
      (agent-shell-tramp-mode -1))))

(ert-deftest agent-shell-tramp-check-acp-support-rejects-old-version ()
  "ACP support check rejects versions before file-handler support."
  (let ((acp-package-version "0.11.3"))
    (should-error (agent-shell-tramp--check-acp-support) :type 'user-error)))

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

(ert-deftest agent-shell-tramp-default-transcript-path-for-local-project ()
  "Call `agent-shell--default-transcript-file-path' for local projects."
  (cl-letf (((symbol-function #'make-directory) #'ignore))
    (agent-shell-tramp-tests--with-cwd "/home/user/project"
      (let ((path (agent-shell-tramp-transcript-file-path)))
        (should
         (string-prefix-p "/home/user/project/.agent-shell/transcripts/" path))))))

(ert-deftest agent-shell-tramp-shell-command-uses-remote-agent-directory ()
  "Shell commands run in the remote agent directory and enter its prompt."
  (let ((shell-buffer (generate-new-buffer " *agent-shell-tramp-command-test*"))
        inserted-text
        process-directory
        process-program
        process-args)
    (unwind-protect
        (progn
          (with-current-buffer shell-buffer
            (setq-local default-directory "/ssh:user@host:/project/"))
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "pwd"))
                    ((symbol-function 'agent-shell--current-shell)
                     (lambda () shell-buffer))
                    ((symbol-function 'agent-shell--build-command-for-execution)
                     #'identity)
                    ((symbol-function 'process-file)
                     (lambda (program _infile _destination _display &rest args)
                       (setq process-directory default-directory)
                       (setq process-program program)
                       (setq process-args args)
                       (insert "/project\n")
                       0))
                    ((symbol-function 'shell-maker-busy) (lambda () nil))
                    ((symbol-function 'agent-shell-insert)
                     (lambda (&rest args)
                       (setq inserted-text (plist-get args :text)))))
            (agent-shell-tramp--insert-shell-command-output #'ignore))
          (should (equal process-directory "/ssh:user@host:/project/"))
          (should (equal process-program shell-file-name))
          (should (equal process-args
                         (list shell-command-switch "pwd 2>&1")))
          (should (string-match-p "\\$ pwd\n\n/project" inserted-text)))
      (kill-buffer shell-buffer))))

(ert-deftest agent-shell-tramp-shell-command-leaves-local-process-unchanged ()
  "Local shell commands retain the original process behavior."
  (let ((shell-buffer (generate-new-buffer " *agent-shell-tramp-command-test*"))
        (invoking-directory temporary-file-directory)
        process-directory)
    (unwind-protect
        (progn
          (with-current-buffer shell-buffer
            (setq-local default-directory "/local/project/"))
          (let ((original-called nil))
            (cl-letf (((symbol-function 'agent-shell--current-shell)
                       (lambda () shell-buffer)))
              (let ((default-directory invoking-directory))
                (agent-shell-tramp--insert-shell-command-output
                 (lambda ()
                   (setq original-called t)
                   (setq process-directory default-directory)))))
            (should original-called))
          (should (equal process-directory invoking-directory)))
      (kill-buffer shell-buffer))))

(ert-deftest agent-shell-tramp-mode-installs-and-removes-wrappers ()
  "Global mode installs and removes path and transcript functions."
  (let ((agent-shell-path-resolver-function agent-shell-path-resolver-function)
        (agent-shell-transcript-file-path-function
         agent-shell-transcript-file-path-function))
    (unwind-protect
        (progn
          (setq agent-shell-path-resolver-function #'identity)
          (setq agent-shell-transcript-file-path-function (lambda () "/tmp/local.md"))
          (agent-shell-tramp-mode 1)
          (should agent-shell-tramp--enabled)
          (should
           (eq agent-shell-path-resolver-function #'agent-shell-tramp-resolve-path))
          (should
           (eq
            agent-shell-transcript-file-path-function
            #'agent-shell-tramp-transcript-file-path))
          (should
           (advice-member-p #'agent-shell-tramp--insert-shell-command-output
                            'agent-shell-insert-shell-command-output))
          (agent-shell-tramp-mode -1)
          (should-not agent-shell-tramp--enabled)
          (should (eq agent-shell-path-resolver-function #'identity))
          (should-not
           (advice-member-p #'agent-shell-tramp--insert-shell-command-output
                            'agent-shell-insert-shell-command-output)))
      (setq agent-shell-path-resolver-function agent-shell-path-resolver-function)
      (setq agent-shell-transcript-file-path-function
            agent-shell-transcript-file-path-function)
      (setq agent-shell-tramp--enabled nil))))

(ert-deftest agent-shell-tramp-mode-enable-is-idempotent ()
  "Enabling the mode repeatedly keeps original functions restorable."
  (let ((agent-shell-path-resolver-function agent-shell-path-resolver-function)
        (agent-shell-transcript-file-path-function
         agent-shell-transcript-file-path-function))
    (unwind-protect
        (let ((original-resolver (lambda (path) (concat "orig:" path)))
              (original-transcript (lambda () "/tmp/local.md")))
          (setq agent-shell-path-resolver-function original-resolver)
          (setq agent-shell-transcript-file-path-function original-transcript)
          (agent-shell-tramp-mode 1)
          (agent-shell-tramp-mode 1)
          (should (eq agent-shell-tramp--orig-path-resolver-function original-resolver))
          (agent-shell-tramp-mode -1)
          (should (eq agent-shell-path-resolver-function original-resolver))
          (should (eq agent-shell-transcript-file-path-function original-transcript)))
      (agent-shell-tramp-mode -1)
      (setq agent-shell-path-resolver-function agent-shell-path-resolver-function)
      (setq agent-shell-transcript-file-path-function
            agent-shell-transcript-file-path-function)
      (setq agent-shell-tramp--enabled nil))))

(provide 'agent-shell-tramp-tests)
;;; agent-shell-tramp-tests.el ends here

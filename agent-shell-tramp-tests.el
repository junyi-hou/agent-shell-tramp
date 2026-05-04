;;; agent-shell-tramp-tests.el --- Tests for agent-shell-tramp -*- lexical-binding: t -*-

(require 'ert)
(require 'agent-shell-tramp)

(ert-deftest agent-shell-tramp--prepare-env-var-test ()
  "Test environment variable formatting."
  (let ((input '("KEY1=VALUE1" "KEY2=VALUE WITH SPACES" "KEY3=foo=bar"
                 "KEY4=it's quoted" "KEY5=$NOT_EXPANDED" "KEY6=")))
    (should
     (equal
      (agent-shell-tramp--prepare-env-var input)
      (list (format "export KEY1=%s" (shell-quote-argument "VALUE1"))
            (format "export KEY2=%s" (shell-quote-argument "VALUE WITH SPACES"))
            (format "export KEY3=%s" (shell-quote-argument "foo=bar"))
            (format "export KEY4=%s" (shell-quote-argument "it's quoted"))
            (format "export KEY5=%s" (shell-quote-argument "$NOT_EXPANDED"))
            (format "export KEY6=%s" (shell-quote-argument "")))))))

(ert-deftest agent-shell-tramp-resolve-path-test ()
  "Test path resolution logic."
  (let ((agent-shell-cwd-mock "/ssh:user@host:/remote/path"))
    (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () agent-shell-cwd-mock)))
      ;; Test remote TRAMP path input
      (should
       (equal
        (agent-shell-tramp-resolve-path "/ssh:user@host:/remote/file") "/remote/file"))
      ;; Test local path on remote input
      (should
       (equal
        (agent-shell-tramp-resolve-path "/remote/path/subdir/file")
        "/ssh:user@host:/remote/path/subdir/file"))))

  (let ((agent-shell-cwd-mock "/local/path"))
    (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () agent-shell-cwd-mock)))
      ;; Test local path when NOT in TRAMP context
      (should (equal (agent-shell-tramp-resolve-path "/local/file") "/local/file")))))

(ert-deftest agent-shell-tramp-transcript-dir-test ()
  "Test transcript directory generation for TRAMP."
  (let ((agent-shell-cwd-mock "/ssh:user@host:/remote/path"))
    (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () agent-shell-cwd-mock))
              ((symbol-function 'make-directory)
               (lambda (_dir &optional _parents) nil)))
      (let ((result (agent-shell-tramp-transcript-dir)))
        (should (string-match-p "/.agent-shell/transcripts/host/remote_path/" result))
        (should (string-match-p "\\.md$" result)))))

  (let ((agent-shell-cwd-mock "/local/path"))
    (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () agent-shell-cwd-mock))
              ((symbol-function 'make-directory)
               (lambda (_dir &optional _parents) nil)))
      (let ((result (agent-shell-tramp-transcript-dir)))
        (should (string-match-p "/local/path/.agent-shell/transcripts/" result))
        (should (string-match-p "\\.md$" result))))))

(ert-deftest agent-shell-tramp--make-acp-client-test ()
  "Test `agent-shell--make-acp-client' advice for TRAMP."
  (let ((agent-shell-cwd-mock "/ssh:user@host:/remote/path")
        (captured-args nil))
    (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () agent-shell-cwd-mock)))
      (let ((mock-fn (lambda (&rest args) (setq captured-args args))))
        (apply #'agent-shell-tramp--make-acp-client
               mock-fn
               '(:command
                 "ls"
                 :command-params ("-la")
                 :environment-variables ("FOO=BAR" "BAZ=QUX WITH SPACES")
                 :context-buffer nil))
        (should (equal (plist-get captured-args :command) "ssh"))
        (let ((params (plist-get captured-args :command-params)))
          (should (member "user@host" params))
          (should
           (string-match-p
            "bash -lc \"export FOO=BAR export BAZ='QUX WITH SPACES'; ls -la\""
            (car (last params)))))))))

(provide 'agent-shell-tramp-tests)
;;; agent-shell-tramp-tests.el ends here

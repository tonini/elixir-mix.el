;; Usage:
;;
;;   emacs -Q -l test/test-runner.el           # interactive mode
;;   emacs -batch -Q -l test/test-runner.el    # batch mode

(let ((current-directory (file-name-directory load-file-name)))
  (setq elixir-mix-test-path (expand-file-name "." current-directory))
  (setq elixir-mix-root-path (expand-file-name ".." current-directory)))

(add-to-list 'load-path elixir-mix-root-path)
(add-to-list 'load-path elixir-mix-test-path)

(require 'elixir-mix)

(dolist (test-file (or argv (directory-files elixir-mix-test-path t "-tests.el$")))
  (load test-file nil t))

(ert-run-tests-batch-and-exit t)

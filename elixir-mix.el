;;; elixir-mix.el --- Emacs integration for Elixir's mix
;;
;; Filename: elixir-mix.el
;; Description: Integration of Elixir's building and deployment tool: mix into Emacs.
;; Author: Samuel Tonini
;; Maintainer: Samuel Tonini
;; Created: So Jun  9 10:01:02 2013 (+0200)
;; Version: 0.1.4
;; URL: http://github.com/tonini/elixir-mix.el
;; Keywords: elixir, mix, elixir-mix

;; The MIT License (MIT)
;;
;; Copyright (c) Samuel Tonini
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;
;;   Manual Installation:
;;
;;    (add-to-list 'load-path "~/path/to/elixir-mix.el/")
;;    (require 'elixir-mix)
;;    (global-elixir-mix-mode)
;;
;;   Interesting variables are:
;;
;;       `elixir-mix-command`
;;
;;            Path to the executable <mix> command
;;
;;       `elixir-mix-buffer-name`
;;
;;            Name for the buffer used for mix shell output.
;;
;;   Major commands are:
;;
;;        M-x elixir-mix-new
;;
;;            Create a new Elixir application.
;;
;;        M-x elixir-mix-test
;;
;;            Run the whole Elixir application test suite.
;;
;;        M-x elixir-mix-test-this-buffer
;;
;;            Run the current buffer through <mix test> command.
;;
;;        M-x elixir-mix-test-file
;;
;;            Run a file through <mix test> command.
;;
;;        M-x elixir-mix-compile
;;
;;            Compile the whole Elixir application.
;;
;;        M-x elixir-mix-run
;;
;;            Runs the given expression in the Elixir application context.
;;
;;        M-x elixir-mix-deps-with-prompt
;;
;;            Prompt for mix deps commands.
;;
;;        M-x elixir-mix-local-with-prompt
;;
;;            Prompt for mix local commands.
;;
;;        M-x elixir-mix-local-install
;;
;;            Prompt for mix local.install <path> or <url>.
;;
;;        M-x elixir-mix-local-install-with-path
;;
;;            Runs local.install and prompt for a <path> as argument.
;;
;;        M-x elixir-mix-local-install-with-url
;;
;;            Runs local.install and prompt for a <url> as argument.
;;
;;        M-x elixir-mix-help
;;
;;            Show help output for a specific mix command.
;;
;;        M-x elixir-mix-execute
;;
;;            Run any command in the context of the application,
;;            except `help` and `new`.
;;            Just run any command as you like, including arguments
;;            for the specific command.  (example: test --quick)
;;

;;; Code:

(defcustom elixir-mix-command "mix"
  "The shell command for mix."
  :type 'string
  :group 'elixir-mix)

(defvar elixir-mix-buffer-name "*MIX*"
  "Name of the mix output buffer.")

(defvar elixir-mix--elixir-project-root-indicator
  "mix.exs"
  "The file which indicate an elixir project root.")

(defvar elixir-mix--deps-commands
  '("deps" "deps.clean" "deps.compile" "deps.get" "deps.unlock" "deps.unlock")
  "List of all deps.* available commands.")

(defvar elixir-mix--local-commands
  '("local" "local.install" "local.rebar" "local.uninstall")
  "List of all local.* available commands.")

(defvar elixir-mix--local-install-option-types
  '("path" "url")
  "List of local.install option types.")

(defun elixir-mix--elixir-project-root (&optional directory)
  "Finds the root directory of the project by walking the
   directory tree until it finds a elixir project root indicator."
  (let* ((directory (file-name-as-directory (or directory (expand-file-name default-directory)))))
    (locate-dominating-file directory elixir-mix--elixir-project-root-indicator)))

(defun elixir-mix--get-buffer (name)
  "Get and kills a buffer if exists and returns a new one."
  (let ((buffer (get-buffer name)))
    (when buffer (kill-buffer buffer))
    (generate-new-buffer name)))

(defun elixir-mix--buffer-setup (buffer)
  "Setup the mix buffer before display."
  (display-buffer buffer)
  (with-current-buffer buffer
    (setq buffer-read-only nil)
    (local-set-key "q" 'quit-window)))

(defun elixir-mix--run-command-async (command)
  (let ((buffer (elixir-mix--get-buffer elixir-mix-buffer-name)))
    (async-shell-command (format "%s %s" elixir-mix-command command) buffer)
    (elixir-mix--buffer-setup buffer)))

(defun elixir-mix--completing-read (prompt command-list)
  (completing-read prompt command-list nil t nil nil (car command-list)))

(defun elixir-mix-flatten (alist)
  (cond ((null alist) nil)
        ((atom alist) (list alist))
        (t (append (elixir-mix-flatten (car alist))
                   (elixir-mix-flatten (cdr alist))))))

(defun elixir-mix-new (name)
  "Create a new elixir project with mix."
  (interactive "Gmix new: ")
  (elixir-mix--run-command-async (format "new %s" name)))

(defun elixir-mix-test ()
  "Run the whole elixir test suite."
  (interactive)
  (elixir-mix-execute "test"))

(defun elixir-mix-test-this-buffer ()
  "Run the current buffer through mix test."
  (interactive)
  (elixir-mix--test-file buffer-file-name))

(defun elixir-mix-test-file (filename)
  "Run <mix test> with the given `filename`"
  (interactive "Fmix test: ")
  (elixir-mix--test-file filename))

(defun elixir-mix--test-file (filename)
  (when (not (file-exists-p filename))
    (error "The given file doesn't exists"))
  (elixir-mix-execute (format "test %s" filename)))

(defun elixir-mix-compile ()
  "Compile the whole elixir project."
  (interactive)
  (elixir-mix-execute "compile"))

(defun elixir-mix-run (code)
  "Runs the given expression in the elixir application context."
  (interactive "Mmix run: ")
  (elixir-mix-execute (format "run -e '%s'" code)))

(defun elixir-mix-deps-with-prompt (command)
  "Prompt for mix deps commands."
  (interactive
   (list (elixir-mix--completing-read "mix deps: " elixir-mix--deps-commands)))
  (elixir-mix-execute command))

(defun elixir-mix-local-with-prompt (command)
  "Prompt for mix local commands."
  (interactive
   (list (elixir-mix--completing-read "mix local: " elixir-mix--local-commands)))
  (if (string= command "local.install")
      (call-interactively 'elixir-mix-local-install)
    (elixir-mix-execute command)))

(defun elixir-mix-local-install (path-or-url)
  "Prompt for mix local.install <path> or <url>."
  (interactive
   (list (completing-read "mix local.install FORMAT: "
                          elixir-mix--local-install-option-types
                          nil t nil nil (car elixir-mix--local-install-option-types))))
  (if (string= path-or-url (car elixir-mix--local-install-option-types))
      (call-interactively 'elixir-mix-local-install-with-path)
    (call-interactively 'elixir-mix-local-install-with-url)))

(defun elixir-mix-local-install-with-path (path)
  "Runs local.install and prompt for a <path> as argument."
  (interactive "fmix local.install PATH: ")
  (elixir-mix-execute (format "local.install %s" path)))

(defun elixir-mix-local-install-with-url (url)
  "Runs local.install and prompt for a <url> as argument."
  (interactive "Mmix local.install URL: ")
  (elixir-mix-execute (format "local.install %s" url)))

(defun elixir-mix-help (command)
  "Show help output for a specific mix command."
  (interactive "Mmix help: ")
  (elixir-mix--run-command-async (format "help %s" command)))

(defun elixir-mix-execute (command)
  "Run a mix command."
  (interactive "Mmix: ")
  (cond ((string= command "") (error "There is no such command"))
        ((string-match "^new" command)
         (error "Please use the `elixir-mix-new (name)` function to create a new elixir project"))
        ((string-match "^help" command)
         (error "Please use the `elixir-mix-help (command)` function to get a mix command specific help")))
  (let ((project-root (elixir-mix--elixir-project-root)))
    (when (not project-root) (error "Couldn't find any elixir project root"))
    (setq default-directory (elixir-mix--elixir-project-root))
    (elixir-mix--run-command-async command)))

;;;###autoload
(define-minor-mode global-elixir-mix-mode
  "Toggle global-elixir-mix-mode to use elixir's mix build tool within emacs."
  :global t)

(provide 'elixir-mix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elixir-mix.el ends here

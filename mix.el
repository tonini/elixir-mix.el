;;; mix.el --- Emacs integration for elixir-lang's mix
;;
;; Filename: mix.el
;; Description:
;; Author: Samuel Tonini
;; Maintainer: Samuel Tonini
;; Created: So Jun  9 10:01:02 2013 (+0200)
;; Version: 0.0.1
;; URL: http://github.com/tonini/mix.el
;; Keywords: elixir, mix
;; Compatibility:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;   Basic steps to setup:
;;
;;    (add-to-list 'load-path
;;                 "~/path/to/mix.el/")
;;    (require 'mix)
;;    (global-mix-mode)

;;; Code:

(defcustom mix-command "mix"
  "The shell command for mix"
  :type 'string
  :group 'mix)

(defvar mix-buffer-name "*MIX*"
  "Name of the mix output buffer.")

(defvar mix--elixir-project-root-indicators
  '("mix.exs" "mix.lock" ".git")
  "list of files and directories which indicate a elixir project root.")

(defun mix--elixir-project-root-directory-p (a-directory)
  "Returns t if a-directory is the elixir project root"
  (equal a-directory (file-name-directory (directory-file-name a-directory))))

(defun mix--elixir-project-root (&optional directory)
  "Finds the root directory of the project by walking the
   directory tree until it finds a elixir project root indicator."
  (let* ((directory (file-name-as-directory (or directory (expand-file-name default-directory))))
         (present-files (directory-files directory)))
    (cond ((mix--elixir-project-root-directory-p directory) nil)
          ((> (length (intersection present-files mix--elixir-project-root-indicators :test 'string=)) 0) directory)
          (t (mix--elixir-project-root (file-name-directory (directory-file-name directory)))))))

(defun mix--get-buffer (name)
  "Get and kills a buffer if exists and returns a new one."
  (let ((buffer (get-buffer name)))
    (when buffer (kill-buffer buffer))
    (generate-new-buffer name)))

(defun mix--buffer-setup (buffer)
  "setup the mix buffer before display."
  (display-buffer buffer)
  (with-current-buffer buffer
    (setq buffer-read-only t)
    (local-set-key "q" 'quit-window)))

(defun mix--run-command-async (command)
  (let ((buffer (mix--get-buffer mix-buffer-name)))
    (async-shell-command command buffer)
    (mix--buffer-setup buffer)))

(defun mix-new (name)
  "create a new elixir project with mix."
  (interactive "Gmix new: ")
  (mix--run-command-async (format "%s new %s" mix-command name)))

(defun mix-test ()
  "run the whole elixir test suite."
  (interactive)
  (mix-execute "test"))

(defun mix-test-this-buffer ()
  "Run the current buffer through mix test."
  (interactive)
  (mix-execute (format "test %s" buffer-file-name)))

(defun mix-compile ()
  "compile the whole elixir project."
  (interactive)
  (mix-execute "compile"))

(defun mix-run (code)
  "runs the given expression in the elixir project context."
  (interactive "Mmix run: ")
  (mix-execute (format "run '%s'" code)))

(defun mix-help (command)
  "show a help for a specific mix command."
  (interactive "Mmix help: ")
  (mix--run-command-async (format "%s help %s" mix-command command)))

(defun mix-execute (command)
  "Run a mix command."
  (interactive "MMix: ")
  (cond ((string= command "") (error "There is no such command."))
        ((string-match "^new" command)
         (error "Please use the `mix-new (name)` function to create a new elixir project."))
        ((string-match "^help" command)
         (error "Please use the `mix-help (command)` function to get a mix command specific help.")))
  (let ((project-root (mix--elixir-project-root)))
    (when (not project-root) (error "Couldn't find any elixir project root."))
    (setq default-directory (mix--elixir-project-root))
    (mix--run-command-async (format "%s %s" mix-command command))))

;;;###autoload
(define-minor-mode global-mix-mode
  "toggle global-mix-mode for use elixir mix build tool within emacs."
  :global t)

(provide 'mix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mix.el ends here

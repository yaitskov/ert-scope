;;; ert-scope.el --- Isolate buffers and files created within a scope of ert test  -*- lexical-binding: t -*-

;; Author: Daniil Iaitskov <dyaitskov@gmail.com>
;; Maintainer: Daniil Iaitskov <dyaitskov@gmail.com>
;; URL: https://github.com/yaitskov/ert-scope
;; Version: 0.0.1
;; Keywords: lisp, tools
;; Package-Requires: ((emacs "30.0") (ert-async "20200105.1031"))

;; The software is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with request.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `ert-deftest' and async counterpart macro leak buffers

;;; Code:

(require 'ert)
(require 'ert-async)

(defmacro ert-scope-with-temp-dir (&rest body)
  "Create a temporary directory bound to TDIR and execute BODY."
  `(letrec ((tdir (make-temp-file "ert-tmpdir" t))
            (default-directory tdir))
     (unwind-protect
         (progn ,@body)
       (delete-directory tdir t))))

(defun ert-scope-kill-buffers-of-list (buffer-white-list)
  "Kill all buffers which are not on the BUFFER-WHITE-LIST."
  (mapc (lambda (b)
          (when (not (cl-find b buffer-white-list))
            (message "Kill leaking buffer [%s]" b)
            (kill-buffer b)))
        (buffer-list)))

(defmacro ert-scope-buffers (&rest body)
  "Kill all buffers created within BODY on leaving the scope.

Example:
  (ert-deftest foo ()
    (ert-scope-buffers
      (ert-scope-with-temp-directory
        (find-file \"foo\")
        (with-curret-buffer \"foo\" (insert \"foo\")))))
  (ert-deftest bar ()
    (ert-scope-buffers
      (ert-scope-with-temp-directory
        (find-file \"foo\")
        (with-curret-buffer \"foo\" (insert \"foo\")))))"

  `(let ((existed-buffers (buffer-list)))
     (unwind-protect (progn ,@body)
       (ert-scope-kill-buffers-of-list existed-buffers))))

(defvar ert-scope-async-timeout-ahead 3
  "Number of seconds timeout occurs before `ert-async-timeout'.")

(defmacro ert-scope-unwind-protect (end bodyform &rest unwindforms)
  "Async version of `unwind-protect'.

END is provided by `ert-deftest-async'.
BODYFORM is a protected block.
UNWINDFORMS are executed at the END or on timeout."
  `(let ((origin-end ,end))
     (letrec
         (
          (timeout-timer-h
           (run-at-time (max 0 (- ert-async-timeout ert-scope-async-timeout-ahead)) nil
                        (lambda ()
                          (unwind-protect
                              (progn ,@unwindforms)
                            (funcall origin-end "ert-scope timeout")))))
          (,end (lambda (&optional error-message)
                  (cancel-timer timeout-timer-h)
                  (unwind-protect
                      (progn ,@unwindforms)
                    (funcall origin-end error-message)))))
       ,bodyform)))

(defmacro ert-scope-with-temp-dir-async (end &rest body)
  "Create a temporary directory bound to `default-directory' and eval BODY.

the temporary directory is deleted when END is called or a timeout happens."
  `(letrec ((tdir (make-temp-file "ert-tmpdir" t))
            (default-directory tdir))
     (ert-scope-unwind-protect
      ,end
      (progn ,@body)
      (delete-directory tdir t))))

(defmacro ert-scope-buffers-async (end &rest body)
  "Kill all buffers created within BODY when END is called or timeout occurs.

It serves similar purpose as `ert-scope-buffers', but for `ert-deftest-async'.

Example:
  (ert-deftest-async foo (end)
    (ert-scope-buffers-async foo
       (letrec
        ((on-hook
           (lambda ()
              (remove-hook \='python-mode-hook on-hook)
              (if buffer-read-only
                  (end \"buffer read only\")
                (end)))))
        (add-hook \='python-mode-hook on-hook)
        (find-file \"foo.py\"))))"
  `(let
       ((existed-buffers (buffer-list)))
     (ert-scope-unwind-protect
      ,end
      (progn ,@body)
      (ert-scope-kill-buffers-of-list existed-buffers))))

(provide 'ert-scope)
;;; ert-scope.el ends here

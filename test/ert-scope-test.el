;;; ert-scope-test.el --- test ert-scope package  -*- lexical-binding: t -*-
(require 'ert-scope)
(require 'ert)
(require 'ert-async)

;;; Code:

(ert-deftest ert-scope-with-temp-dir-success-test ()
  (cl-flet
      ((unscoped-test ()
         (find-file "foo.txt")
         (should (= (point-max) 1))
         (should (equal (buffer-name) "foo.txt"))
         (insert "hello world")
         (save-buffer)
         (kill-this-buffer)))
    (cl-flet
        ((scoped-test () (ert-scope-with-temp-dir tdir (unscoped-test))))
      (cl-loop for i from 0 to 5 do (scoped-test)))))

(ert-deftest ert-scope-with-temp-dir-error-test ()
  (cl-flet
      ((unscoped-test ()
         (find-file "foo.txt")
         (should (= (point-max) 1))
         (should (equal (buffer-name) "foo.txt"))
         (insert "hello world")
         (save-buffer)
         (kill-this-buffer)))
    (should-error
     (ert-scope-with-temp-dir tdir (unscoped-test) (error "Oops")))
    (ert-scope-with-temp-dir tdir (unscoped-test))))

(ert-deftest ert-scope-buffers-test ()
  (cl-flet
      ((unscoped-test ()
         (switch-to-buffer (get-buffer-create "foo.txt"))
         (should (= (point-max) 1))
         (insert "hello world")
         (should (> (point-max) 1))))
    (cl-flet
        ((scoped-test () (ert-scope-buffers (unscoped-test))))
      (cl-loop for i from 0 to 5 do (scoped-test)))))


;; (ert-deftest-async skip-empty-file-test (end)
;;   (letrec ((log-file "no-existing.log")
;;            (check
;;             (lambda ()
;;               (remove-hook 'color-log-mode-evaled-hook check)
;;               (when buffer-read-only
;;                 (funcall end (concat "Buffer for [" log-file "] is immutable")))
;;               (funcall end))))
;;     (when (file-exists-p log-file)
;;       (delete-file log-file))
;;     (add-hook 'color-log-mode-evaled-hook check)
;;     (without-buffers)
;;     (find-file log-file)))

;; (ert-deftest-async skip-big-file-test (end)
;;   (letrec
;;       ((l-log "l.log")
;;        (override-file-limit
;;         (lambda () (set (make-local-variable 'color-log-mode-big-file-size) 3)))
;;        (check-SGR-exanded
;;         (lambda ()
;;           (let ((expected (f-read-text "x.log"))
;;                 (notfaced (buffer-string)))
;;             (remove-hook 'color-log-mode-hook override-file-limit)
;;             (remove-hook 'color-log-mode-evaled-hook check-SGR-exanded)
;;             (when buffer-read-only
;;               (funcall end (concat "Buffer for [" l-log "] is immutable")))
;;             (if (equal notfaced expected)
;;                 (funcall end)
;;               (funcall end (format "Expected:\n%s\nGot:\n%s\n" expected notfaced)))))))
;;     (copy-file "./x.log" l-log t)
;;     (without-buffers)
;;     (find-file "l.txt")
;;     (add-hook 'color-log-mode-evaled-hook check-SGR-exanded)
;;     (add-hook 'color-log-mode-hook override-file-limit)
;;     (find-file l-log)))

;; (ert-deftest-async skip-file-without-any-SGR-test (end)
;;   (letrec
;;       ((check-SGR-exanded
;;         (lambda ()
;;           (let ((plain (f-read-text "l.txt"))
;;                 (notfaced (buffer-string)))
;;             (remove-hook 'color-log-mode-evaled-hook check-SGR-exanded)
;;             (when buffer-read-only
;;               (funcall end "Buffer for [l.log] is in read only mode"))
;;             (if (equal plain notfaced)
;;                 (funcall end)
;;               (funcall end (format "Expected:\n%s\nGot:\n%s\n%s" plain notfaced (buffer-name))))))))
;;     (copy-file "./l.txt" "l.log" t)
;;     (add-hook 'color-log-mode-evaled-hook check-SGR-exanded)
;;     (without-buffers)
;;     (find-file "l.log")))

(provide 'ert-scope-test)
;;; ert-scope-test.el ends here

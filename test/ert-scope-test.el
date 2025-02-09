;;; ert-scope-test.el --- test ert-scope package  -*- lexical-binding: t -*-
(require 'ert)
(require 'ert-async)
(require 'ert-scope)

;;; Code:

(setq ert-async-timeout 2
      ert-scope-async-timeout-ahead 1)

(ert-deftest ert-scope-with-temp-dir-success-test ()
  (cl-loop for i from 0 to 5
           do (ert-scope-with-temp-dir
               cdir tdir
               (ert-scope-buffers
                (should-not (get-buffer "foo.txt"))
                (find-file (concat tdir "/foo.txt"))
                (should (= (point-max) 1))
                (should (equal (buffer-name) "foo.txt"))
                (insert "hello world")
                (save-buffer)))))

(ert-deftest ert-scope-with-temp-dir-error-test ()
  (cl-flet
      ((unscoped-test (tdir)
         (ert-scope-buffers
          (should-not (get-buffer "foo.txt"))
          (find-file (concat tdir "/foo.txt"))
          (should (= (point-max) 1))
          (should (equal (buffer-name) "foo.txt"))
          (insert "hello world")
          (save-buffer))))
    (should-error
     (ert-scope-with-temp-dir cdir tdir (unscoped-test tdir) (error "Oops")))
    (ert-scope-with-temp-dir cdir tdir (unscoped-test tdir))))

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

(ert-deftest-async ert-scope-unwind-protect-end-unwinds-test (end)
  (letrec
      ((unwinded nil)
       (ahead-of-ert-end
        (lambda (&optional error-message)
          (funcall end (if (= 1 unwinded) error-message "Unwind form is not executed")))))
    (ert-scope-unwind-protect
     ahead-of-ert-end
     (run-at-time 0 nil (lambda () (funcall ahead-of-ert-end)))
     (setq unwinded 0) ;; 2 forms
     (setq unwinded (1+ unwinded)))))

(ert-deftest-async ert-scope-unwind-protect-timeout-unwinds-test (end)
  (letrec
      ((unwind-called 0)
       (ahead-of-ert-end
        (lambda (&optional error-message)
          (if (= unwind-called 0)
              (funcall end (format "unwind-called 0 : %s" error-message))
            (if (equal "ert-scope timeout" error-message)
                (funcall end)
              (funcall end error-message))))))
    (ert-scope-unwind-protect
     ahead-of-ert-end
     (message "Wait for time out...")
     (message "Unwind section") ;; 2 forms
     (setq unwind-called 1))))

(ert-deftest-async ert-scope-unwind-protect-nested-unwinds-test (end)
  (letrec
      ((unwinded nil)
       (ahead-of-ert-end
        (lambda (&optional error-message)
          (if error-message
              (funcall end error-message)
            (when (= unwinded 3)
              (funcall end))))))
    (ert-scope-unwind-protect
     ahead-of-ert-end
     (ert-scope-unwind-protect
      ahead-of-ert-end
      (run-at-time 0 nil (lambda () (funcall ahead-of-ert-end)))
      (setq unwinded 0) ;; 2 forms
      (setq unwinded (1+ unwinded)))
     (setq unwinded (+ 2 unwinded)))))

(ert-deftest-async ert-scope-with-temp-dir-async-test (end)
  (ert-scope-with-temp-dir-async
   end cdir tdir
   (ert-scope-buffers
     (find-file (concat tdir "/foo.txt"))
     (should (= 1 (point-max)))
     (insert "foo")
     (save-buffer))
   (run-at-time 0 nil end)))

(ert-deftest-async ert-scope-with-temp-dir-async-2-test (end)
  (ert-scope-with-temp-dir-async
   end cdir tdir
   (ert-scope-buffers
     (find-file (concat tdir "/foo.txt"))
     (should (= 1 (point-max)))
     (insert "foo")
     (save-buffer))
   (run-at-time 0 nil end)))

(ert-deftest-async ert-scope-buffers-async-test (end)
  (ert-scope-buffers-async
   end
   (should-not (get-buffer "foo.txt"))
   (get-buffer-create "foo.txt")
   (run-at-time 0 nil end)))

(ert-deftest-async ert-scope-buffers-async-2-test (end)
  (ert-scope-buffers-async
   end
   (should-not (get-buffer "foo.txt"))
   (get-buffer-create "foo.txt")
   (run-at-time 0 nil end)))

(ert-deftest-async ert-scope-with-temp-dir-async-and-buffer-test (end)
  (ert-scope-with-temp-dir-async
   end cdir tdir
   (ert-scope-buffers-async
    end
    (should-not (get-buffer "foo.txt"))
    (find-file (concat tdir "/foo.txt"))
    (should (= 1 (point-max)))
    (insert "foo")
    (save-buffer)
    (run-at-time 0 nil end))))

(ert-deftest-async ert-scope-with-temp-dir-async-and-buffer-2-test (end)
  (ert-scope-with-temp-dir-async
   end cdir tdir
   (ert-scope-buffers-async
    end
    (should-not (get-buffer "foo.txt"))
    (find-file (concat tdir "/foo.txt"))
    (should (= 1 (point-max)))
    (insert "foo")
    (save-buffer)
    (run-at-time 0 nil end))))


(provide 'ert-scope-test)
;;; ert-scope-test.el ends here

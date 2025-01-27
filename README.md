# ert-scope package

The package does not provide anything interactive.  It has a test
wrapping macros improving ert test isolation.

ert (built-in emacs package) doesn't close buffers created during test
execution.  So a buffer created inside of one `ert-deftest` can
conflict wih another at `find-file` invocation and
`with-current-buffer` using a buffer name as an argument would select
a garbage buffer used in previous test. Emacs buffer is the most
ubiqitous global entity in the system.  This aspect makes ert tests
inherently fragile ande less isolated.

Example where buffer and file for "foo.txt" are killed and removed once
test scope ends:

``` emacs-lisp
(require 'ert)
(require 'ert-async)
(require 'ert-scope)

(ert-deftest-async async-test (end)
  (ert-scope-with-temp-dir-async
   end
   (ert-scope-buffers-async
    end
    (should-not (get-buffer "foo.txt"))
    (find-file "foo.txt")
    (should (= 1 (point-max)))
    (insert "foo")
    (save-buffer)
    (run-at-time 0 nil end))))
```

Not async version:
``` emacs-lisp
(require 'ert)
(require 'ert-scope)

(ert-deftest sync-test ()
  (ert-scope-with-temp-dir
    (should-not (get-buffer "foo.txt"))
    (find-file "foo.txt")
    (should (= 1 (point-max)))
    (insert "foo")
    (save-buffer)))
```

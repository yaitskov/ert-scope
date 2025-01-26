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

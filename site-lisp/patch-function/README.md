patch-function.el
=================

Customize elisp functions via simple patching.

Emacs is extensible via advice and hooks.  However, there are occasions where these mechanisms are insufficient.  One can simply copy and redefine the entire function but that can be a problem when the overridden library is updated.  `patch-function` addresses this need by allowing you to change functions through search-replace instructions.  I have found a surprising number of uses for this function.

<!--
Installation
------------

### Melpa Installation

[![MELPA](https://melpa.org/packages/patch-function-badge.svg)](https://melpa.org/#/patch-function)

    M-x package-install RET patch-function RET

### `use-package` Installation
```lisp
(use-package better-shell
    :ensure t)
```

-->
Usage
-----

This package provides a single function called `patch-function`, shown
here.

```lisp
(patch-function FUNC-SYMBOL [REGEXP REP]...)
```

For each [REGEXP REP] pair, replace all occurrences of REGEXP with REP
in FUNC-SYMBOLS's source definition.  The patches are made only in
memory and source files on disk are not changed.  It works by loading
the source code for the function in a buffer, patching the code,
evaluating it, then reverting the code.  It will then close the
function's buffer unless it was already open.  This function is
idempotent.  Calling this function with no arguments will restore the
function's original definition.

REGEXP is a regular expression string to search for in
FUNC-SYMBOL's definition.  If any of the REGEXPs are not found,
the function is not patched and an error is signaled.

REP is the replacement for each REGEXP match.  It can be a string
or an arbitrary form.  Using forms is usually more convenient for
any non-trivial change.  If your replacement is an unbalanced
form, you will have to pass it in as a string.

Examples:

Here's a patch with a string replacement.

```lisp
    ;; Patch tramp shell prompt to show my current directory.

    (patch-function ’tramp-sh-handle-start-file-process
     "(tramp-file-name-localname v)"
     "\"$PWD\"")
```

And here's a patch with a form replacement.

``` lisp
    ;; Patch ‘gnus-dired-attach’ to display the name of the
    ;; compose buffer to which the file will be attached.

    (require ’gnus-dired) ;; load the function before patching it

    (patch-function ’gnus-dired-attach
      "\"Attach files to existing mail composition buffer\\? \""
     ’(format "Attach files to existing mail composition buffer%s"
              (if (= (length bufs) 1)
                  (concat " (" (car bufs) ")?")
                "?")))
```

Another Package
===============
A more sophisticated package called `el-patch` solves the same problem.  Find it at https://github.com/raxod502/el-patch
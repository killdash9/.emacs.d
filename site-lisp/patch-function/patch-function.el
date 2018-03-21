;;; patch-function.el --- Customize elisp functions via patching

;;; Commentary:
;; patch-function allows you to customize an elisp function by
;; specifying changes to portions of its source code.  See the
;; patch-function documentation for more info.

;;; Code:

(defun patch-function (func-symbol &rest replacements)
  "Redefine FUNC-SYMBOL using the given REPLACEMENTS.

REPLACEMENTS is a series of search and replace arguments, like
this:

 (patch-function FUNC-SYMBOL [REGEXP REP]...)

For each [REGEXP REP] pair, replace all occurrences of REGEXP
with REP in FUNC-SYMBOLS's source definition.  The patches are
made only in memory and source files on disk are not changed.  It
works by loading the source code for the function in a buffer,
patching the code, evaluating it, then reverting the code.  It
will then close the function's buffer unless it was already open.
This function is idempotent.  Calling this function with no
arguments will restore the function's original definition.

REGEXP is a regular expression string to search for in
FUNC-SYMBOL's definition.  If any of the REGEXPs are not found,
the function is not patched and an error is signaled.

REP is the replacement for each REGEXP match.  It can be a string
or an arbitrary form.  Using forms is usually more convenient for
any non-trivial change.  If your replacement is an unbalanced
form, you will have to pass it in as a string.

Examples:

Here's a patch with a string replacement.

    ;; Patch tramp shell prompt to show my current directory.

    (patch-function 'tramp-sh-handle-start-file-process
     \"(tramp-file-name-localname v)\"
     \"\\\"$PWD\\\"\")

And here's a patch with a form replacement.

    ;; Patch `gnus-dired-attach' to display the name of the
    ;; compose buffer to which the file will be attached.

    (require 'gnus-dired) ;; load the function before patching it

    (patch-function 'gnus-dired-attach
      \"\\\"Attach files to existing mail composition buffer\\\\? \\\"\"
     '(format \"Attach files to existing mail composition buffer%s\"
              (if (= (length bufs) 1)
                  (concat \" (\" (car bufs) \")?\")
                \"?\")))"

  (unless (functionp func-symbol)
    (user-error "First argument must be a function"))
  (when (= 1 (% (length replacements) 2))
    (user-error "Expecting an even number of REPLACEMENTS arguments"))
  (let ((buffer-list (buffer-list)))
    (cl-destructuring-bind (buf . point) (find-function-noselect func-symbol t)
      (with-current-buffer buf ;; temporarily switch to the buffer
        ;; create an empty undo stack so that we can revert changes below
        (let ((buffer-undo-list)
              (start (point))
              end)
          (save-mark-and-excursion ;; save point and mark
            (unwind-protect
                (progn
                  (narrow-to-defun)
                  ;; loop over search-replace pairs
                  (while replacements
                    (let ((regexp (car replacements))
                          (rep (cadr replacements)))
                      ;; convert rep to string if needed
                      (unless (stringp rep) (setq rep (pp-to-string rep)))
                      ;; search and replace
                      (goto-char (point-min))
                      (re-search-forward regexp)
                      (cl-loop do (replace-match rep)
                               while (re-search-forward regexp nil t))
                     
                      (setq replacements (cddr replacements))))

                  (setq end (point-max)) ; mark the end of the function
                  ;; widen and eval-region to pick up any lexical
                  ;; scoping directives at top of buffer
                  (widen)
                  (eval-region start end))
              
              (widen) ; just in case there was an error before the widen above
              ;; undo just our changes
              (primitive-undo (length buffer-undo-list) buffer-undo-list)
              ))
          ;; kill the buffer unless it was already open
          (unless (member buf buffer-list) (kill-buffer))))))
  (message "Patched function %s" func-symbol))
(provide 'patch-function)

;;; patch-function.el ends here

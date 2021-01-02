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
made only in memory and source files on disk are left unchanged.
It works by loading the source code for the function in a buffer,
patching the code, evaluating it, then reverting the code.  It
will then close the function's buffer unless it was already open.
This function is idempotent.  Calling this function with no
replacements will restore the function's original definition.

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
  (setq replacements (patch-function-collapse replacements))
  (when (= 1 (% (length replacements) 2))
    (user-error "Found a search without a replacement"))
  (let ((buffer-list (buffer-list))
        docs-updated)
    (cl-destructuring-bind (buf . point) (find-function-noselect func-symbol t)
      (with-current-buffer buf ;; temporarily switch to the buffer
        ;; create an empty undo stack so that we can revert changes below
        (let ((buffer-undo-list)
              (start (point))
              end
              (r replacements))
          (save-mark-and-excursion ;; save point and mark
            (unwind-protect
                (save-restriction
                  (narrow-to-defun)
                  ;; loop over search-replace pairs
                  (while r
                    (let ((regexp (car r))
                          (rep (cadr r))
                          (inhibit-read-only t))
                      ;; search and replace
                      (goto-char (point-min))
                      (re-search-forward regexp)
                      (cl-loop do (replace-match rep)
                               while (re-search-forward regexp nil t))
                      (setq r (cddr r))))

                  (when replacements
                    ;; note in the documentation that is has been patched
                    (goto-char (point-min))
                    (when (and replacements (eq (char-after) ?\())
                      (forward-char)
                      (when (looking-at "defun")
                        (forward-sexp 2)
                        (when (looking-at "[ \t\r\n\v\f] *(")
                          (forward-sexp)
                          (if (not (looking-at "[ \t\r\n\v\f] *\""))
                              (insert "\"\"")
                            (forward-sexp))
                          (backward-char)
                          (when (eq (char-after) ?\")
                            (insert "\n\nThis function has been patched by the following code\n\n")
                            (let ((doc (pp-to-string (pp-to-string
                                                      (append (list 'patch-function `',func-symbol)
                                                              (mapcar
                                                               (lambda (s)
                                                                 (if (not (stringp s)) `',s s))
                                                               replacements))))))
                              (insert (substring doc 1 (- (length doc) 1))))
                            (setq docs-updated t))))))
                  
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
          (unless (member buf buffer-list) (kill-buffer)))))
    (if replacements
        (if docs-updated
            (message "Patched function %s" func-symbol)
          (message "Patched function %s but unable to update its docs" func-symbol))
      (message "Unpatched function %s" func-symbol))))

(defun patch-function-collapse (l &optional s)
  "Convert sequences of non-strings in L into into strings.
S is the string being built as part of the recursion."
  (let* ((e (car l))
         (stringp (stringp e))
         (tail (cdr l)))
    (cond 
     ((null l)
      (if s (list s)))
     (s
      (if stringp (cons s (cons e (patch-function-collapse tail)))
        (patch-function-collapse tail (concat s " " (pp-to-string e)))))
     (stringp
      (cons e (patch-function-collapse tail)))
     (t
      (patch-function-collapse tail (pp-to-string e))))))


(defun patch-function-test-function ()
  "A test function."
  (interactive)
  (let ((o (append
            '(1 2 replace-me 4 5)
            '(6 7 8))))
    (message "%s" o)
    o)
  )

(defun test-patch-function ()
  "Test the patch function."
  (interactive)
  (patch-function 'patch-function-test-function
                  "replace-me"
                  "3"
                  "(6 7 8)"
                  "'(9 10)\n")
  (cl-assert
   (equal '(1 2 3 4 5 6 7 8 9 10) (patch-function-test-function))))

(provide 'patch-function)

;;; patch-function.el ends here

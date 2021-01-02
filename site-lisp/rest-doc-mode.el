;;; rest-doc-mode --- Syntax highlighting for rest documentation

;;; Commentary:

;;; Code:

(require 'json-mode)
(require 'outline)

(defconst rest-doc-method-regex "^\\<\\(GET\\|PUT\\|POST\\|PATCH\\|DELETE\\)\\>")
(defconst rest-doc-font-lock-keywords
  (append
   (list
    `(,rest-doc-method-regex . font-lock-builtin-face)
    '("\\<\\(200\\|400\\)\\>" . font-lock-type-face)
    '("\\<\\(OK\\)\\>" . font-lock-comment-face)
    '("\\?[^\n]*" . font-lock-variable-name-face)
    '("\\(/[^?\n]*\\)\\>" . font-lock-function-name-face))
   json-font-lock-keywords-1)
  "Minimal highlighting expressions for rest-doc mode.")

(defvar rest-doc-mode-map nil "Keymap for `rest-doc-mode'.")

(progn
  (setq rest-doc-mode-map (make-sparse-keymap))
  (define-key rest-doc-mode-map (kbd "TAB") 'rest-doc-tab-press)
  (define-key rest-doc-mode-map (kbd "<backtab>") 'org-global-cycle)
  )

(defun rest-doc-global-cycle ()
  "Cycle collapse state of entire document."
  (interactive)
  (org-global-cycle)
  (beginning-of-visual-line))

(defun rest-doc-tab-press ()
  "Do the right thing when tab is pressed."
  (interactive)
  (if (and (outline-on-heading-p)
           ;; see if indentation is correct
           (save-excursion
             (beginning-of-line)
             (let (before after)
               (skip-chars-forward " \t")
               (setq before (point))
               (indent-for-tab-command)
               (setq after (point))
               (= before after))))
      (rest-doc-toggle-subtree)
    (indent-for-tab-command))
  )

(defun rest-doc-toggle-subtree ()
  "Show or hide the current subtree depending on its current state."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (not (outline-invisible-p (line-end-position)))
        (outline-hide-subtree)
      (outline-show-subtree))))

(defun rest-doc-outline-level ()
"Return the level of the outline."
(if (looking-at rest-doc-method-regex)
    1
  (if (looking-at "200 ")
      2
    (let ((col (let (buffer-invisibilityp-spec)
                 (save-excursion
                   (skip-chars-forward " \t")
                   (current-column)))))
      (+ 3 col)))))

(define-derived-mode rest-doc-mode json-mode "rest-doc"
  "Syntax highlighting for rest documentation"
  (set (make-local-variable 'font-lock-defaults) '(rest-doc-font-lock-keywords t))

  ;; outline uses this regexp to find headers. I match lines with no indent and indented "class"
  ;; and "def" lines.
  (setq outline-regexp (concat ".*\\({\\|\\[\\)[ \t]*$\\|200 \\|"
                               rest-doc-method-regex
                               " +/"))
  ;; enable our level computation
  (setq outline-level 'rest-doc-outline-level)
  ;; do not use their \C-c@ prefix, too hard to type. Note this overides some bindings.
  (setq outline-minor-mode-prefix "\C-t")
  ;; turn on outline mode
  (outline-minor-mode t)
  ;; initially hide all but the headers
  ;;(hide-body)
  ;; make paren matches visible
  (show-paren-mode 1)
  (use-local-map rest-doc-mode-map)
  
  )

(provide 'rest-doc-mode)
;;; rest-doc-mode.el ends here

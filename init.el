;;; * Custom File
(setq custom-file "~/.emacs.d/custom.el")

;;; * Package Framework
(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                         ("org"       . "https://orgmode.org/elpa/")
                         ("melpa"     . "http://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)
(setq package-enable-at-startup nil) ;; keep it from re-loading the packages after the init file has run

;;; * Load Path
;; adding this to the load path after package-initialize causes
;; site-lisp paths to override elpa paths.
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

;; add all subdirectories of site-lisp to load path as well
(mapc
 (lambda (path) (add-to-list 'load-path path))
 (split-string (s-trim-right (shell-command-to-string "find ~/.emacs.d/site-lisp -type d -d 1")) "[\r\n]+"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-compute-statistics t)

(use-package use-package-chords
  :ensure t)

(use-package patch-function)

;;; * Secrets file
(and (file-exists-p "~/Dropbox/.emacs-secrets.el" ) (load "~/Dropbox/.emacs-secrets.el"))

;;; * exec-path-from-shell -- load this first
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize) ; set the right PATH variable
  )

;(setq use-package-verbose t)

;;; * Packages
;;; ** Look and Feel
;;; *** tool-bar-mode
(use-package tool-bar
  :init (tool-bar-mode -1))

;;; *** scroll-bar
(use-package scroll-bar
  :init (scroll-bar-mode -1))

;;; *** frame
(use-package frame
  :config
  (progn
    ;; Set transparency of emacs
    (defun transparency (value)
      "Sets the transparency of the frame window. 0=transparent/100=opaque"
      (interactive "nTransparency Value 0 - 100 opaque:")
      (set-frame-parameter (selected-frame) 'alpha value))
    (global-set-key [C-s-268632070] 'toggle-frame-fullscreen) ;; ctrl-command-F
    ))

;;; *** menu-bar
(use-package menu-bar
  :commands fix-menu-bars
  :config
  (defun fix-menu-bars ()
    (interactive)
    (menu-bar-mode -1)
    (menu-bar-mode 1)
    ))

;;; *** random-edit
(use-package random-edit)

;;; *** zone
(use-package zone
  :commands zone-when-idle
  :defer 5
  :config
  (progn
    (use-package battery
      :config
      (defadvice zone (around zone-dont-run-on-battery activate)
        "Only zone when under power"
        (when (or t (equal (cdr (assoc ?L (funcall battery-status-function))) "AC"))
          ad-do-it))
      )
    (use-package zone-matrix
      :config
      (progn
        (setq zmx-unicode-mode t)
        (setq zone-programs (vconcat zone-programs [zone-matrix]))
        (setq zone-programs [random-edit])
        ))
    (setq zone-programs (remove-if (lambda (x) (member x '(zone-pgm-jitter zone-pgm-dissolve))) zone-programs))
    ;(zone-when-idle 3600)
    ))

;;; *** fns.c
(progn
  (fset 'yes-or-no-p 'y-or-n-p))

;;; *** subr.el
;; don't ask if I want to kill a buffer with a live process attached to it.  Just do it.
;;(progn
;;  (setq kill-buffer-query-functions
;;        (remq 'process-kill-buffer-query-function
;;              kill-buffer-query-functions)))

;;; *** variable-pitch-modes
(use-package variable-pitch-modes)

;;; *** rainbow-mode
; this package colors html-style colors such as #ff7700
(use-package rainbow-mode
  :ensure t
  :config (rainbow-mode 1)
  :diminish
  )
;;; *** diminish
(use-package diminish
  :ensure t)

;;(eval-after-load "isearch" '(require 'isearch+))
;;; ** Games
;;; *** 2048-game
(use-package 2048-game
  :ensure t
  :commands 2048-game)

;;; *** tron
(use-package tron
  :commands (tron tron-demo)
  :config
  (setq tron-score-file "~/.emacs.d/games/tron-scores"))

;;; *** tetris
(use-package tetris
  :commands tetris
  :config
  (setq tetris-score-file
        "~/.emacs.d/games/tetris-scores")
  )
;;; *** xkcd
(use-package xkcd
  :ensure t
  :commands xkcd)
;;; ** Editing
;;; *** type-break-mode
(use-package type-break
  :config (progn
            (setq type-break-file-name nil)
            (type-break-mode 1)
            (setq type-break-demo-functions
                  '(type-break-demo-boring type-break-demo-life type-break-demo-hanoi tron-demo)
                  ;'(tron-demo)
                  ))
  )
;;; *** hippie-expand
(use-package hippie-expand
  ;;:bind ("M-s-÷" . hippie-expand)
  :bind ("M-/" . hippie-expand))

;;; *** yasnippet
'(use-package yasnippet
  :commands yas-global-mode
  :ensure t
  :defer 5
  :config (progn
	    (yas-global-mode 1)
	    (diminish 'yas-minor-mode " Y")))

;;; *** zencoding-mode
(use-package zencoding-mode
  :commands zencoding-mode
  :ensure t
  :init
  (progn
    ;; Auto-start on any markup modes
    (add-hook 'sgml-mode-hook 'zencoding-mode)
    (add-hook 'web-mode-hook 'zencoding-mode)
    (add-hook 'nxml-mode-hook 'zencoding-mode)))

;;; *** delsel
(use-package delsel
  :init (delete-selection-mode))

;;; *** indent.c
(progn                                  ;indent.c
  (setq-default indent-tabs-mode nil)
  ;(setq indent-line-function 'insert-tab) This causes a tab to be inserted when pressing enter in fundamental mode
  )

;;; *** undo-tree
(use-package undo-tree
  :ensure t
  :bind (("s-z" . undo-tree-undo)
         ("s-Z" . undo-tree-redo))
  :init (global-undo-tree-mode))

;;; *** move-text -- used to be move-text package, but that was buggy on emacs 25, so I pulled most of this from
;;; https://www.emacswiki.org/emacs/basic-edit-toolkit.el
(progn
  (defun move-text-internal (arg)
    "Move region (transient-mark-mode active) or current line."
    (let ((remember-point (point)))
      ;; Can't get correct effect of `transpose-lines'
      ;; when `point-max' is not at beginning of line
      ;; So fix this bug.
      (goto-char (point-max))
      (if (not (bolp)) (newline))       ;add newline to fix
      (goto-char remember-point)
      ;; logic code start
      (cond ((and mark-active transient-mark-mode)
             (if (> (point) (mark))
                 (exchange-point-and-mark))
             (let ((column (current-column))
                   (text (delete-and-extract-region (point) (mark))))
               (forward-line arg)
               (move-to-column column t)
               (set-mark (point))
               (insert text)
               (exchange-point-and-mark)
               (setq deactivate-mark nil)))
            (t
             (let ((column (current-column)))
               (beginning-of-line)
               (when (or (> arg 0) (not (bobp)))
                 (forward-line 1)
                 (when (or (< arg 0) (not (eobp)))
                   (transpose-lines arg))
                 (forward-line -1))
               (move-to-column column t))
             ))))

  (defun move-text-down (arg)
    "Move region (transient-mark-mode active) or current line
  arg lines down."
    (interactive "*p")
    (move-text-internal arg))

  (defun move-text-up (arg)
    "Move region (transient-mark-mode active) or current line
  arg lines up."
    (interactive "*p")
    (move-text-internal (- arg)))

  (defun move-text-default-bindings ()
    "Bind `move-text-up' and `move-text-down' to M-up and M-down."
    (global-set-key [M-up] 'move-text-up)
    (global-set-key [M-down] 'move-text-down))

  
  (defun move-text-indent (&rest args)
    (when
        (member major-mode
                '(emacs-lisp-mode lisp-mode
                                  clojure-mode    scheme-mode
                                  haskell-mode    ruby-mode
                                  rspec-mode      ;;python-mode
                                  c-mode          c++-mode
                                  objc-mode       latex-mode
                                  plain-tex-mode  php-mode
                                  js2-mode java-mode))
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            ;; reactivate the mark
            (activate-mark)
            (setq deactivate-mark nil))
        (indent-according-to-mode))))
  (advice-add 'move-text-up :after 'move-text-indent)
  (advice-add 'move-text-down :after 'move-text-indent)
  
  (bind-key "M-<up>" 'move-text-up )
  (bind-key "M-<down>" 'move-text-down )
  )

;;; *** pretty-lambdada
(use-package pretty-lambdada
  :ensure t
  :defer 5
  :config (pretty-lambda-for-modes))

;;; *** saveplace
(use-package saveplace
  :defer 5
  :config
  (set-default 'save-place t))

;;; *** executable
(use-package executable
  :defer t
  :config
  ;; make scripts executable automatically
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p))

;;; *** paren
(use-package paren
  :defer 5
  :config
  (show-paren-mode 1))

;;; *** smartparens
(use-package smartparens
  :ensure t
  :defer 5
  :config
  (progn
    (bind-key "C-<right>" 'sp-slurp-hybrid-sexp smartparens-mode-map)
    (bind-key "C-M-k" 'sp-kill-hybrid-sexp smartparens-mode-map)
    ;; TODO: allow backward kills
    (defun sp-kill-hybrid-sexp-skip-ws-first (&rest args)
      (and
       (looking-at "[\n\r \t]")
       (re-search-forward "[^\n\r \t]" nil t)
       (backward-char)))
    (advice-add 'sp-kill-hybrid-sexp :before #'sp-kill-hybrid-sexp-skip-ws-first)))

;;; *** time
(use-package time
  :config
  (progn
    (setq display-time-mail-file 'none
          display-time-format "%d %l:%M"
          display-time-default-load-average nil)
    (display-time-mode 1)))

;;; *** simple
(use-package simple
  :bind (("s-d" . kill-whole-line)
         ("M-SPC" . cycle-spacing)
         ("M-s-<down>" . duplicate-line-down)
         ("M-s-<up>" . duplicate-line-up)
         ("s-!" . shell-command-on-buffer)
         ("C-o" . killdash9/open-line))
  :config
  (progn

    (defun killdash9/open-line ()
      "Move to the beginning of the line, then open a new line"
      (interactive)
      (beginning-of-line)
      (open-line 1))

    (setq save-interprogram-paste-before-kill t)
    (define-key key-translation-map (kbd "<C-backspace>") (kbd "<deletechar>"))
    (defadvice mark-whole-buffer (after mark-whole-buffer-activate-mark activate)
      (activate-mark))
    ;(column-number-mode 1)

    (defun escape-yank()
      "escapes yanked code if inside string"
      (interactive)
      (if (nth 3 (syntax-ppss)) ;; Checks if inside a string
          (insert-for-yank (replace-regexp-in-string "[\\\"]"
                                                     "\\\\\\&"
                                                     (current-kill 0)
                                                     t))
        (call-interactively 'yank)))


    (defun join-region (beg end join-string)
      "Apply join-line over region."
      (interactive "r
sJoin string:")
      (let ((beg (copy-marker beg))
            (end (copy-marker end)))
        (if (or t mark-active) ;; do we care if mark is active?
            (progn
              (goto-char beg)
              (while (< (point) end)
                (join-line 1)
                (insert join-string))))))

    (defun maybe-indent-region (&rest args)
      (and (not current-prefix-arg)
           (member major-mode
                   '(emacs-lisp-mode lisp-mode
                                     clojure-mode    scheme-mode
                                     haskell-mode    ruby-mode
                                     rspec-mode      ;;python-mode
                                     c-mode          c++-mode
                                     objc-mode       latex-mode
                                     plain-tex-mode  php-mode
                                     js2-mode java-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))
    
    ;; This auto-indents on paste
    (dolist (command '(yank yank-pop))
      (advice-add command :after #'maybe-indent-region))

    (defun duplicate-line-up()
      (interactive)
      (move-beginning-of-line 1)
      (kill-line)
      (yank)
      (move-beginning-of-line 1)
      (open-line 1)
      (yank)
      (move-beginning-of-line 1)
      )

    (defun duplicate-line-down()
      (interactive)
      (move-beginning-of-line 1)
      (kill-line)
      (yank)
      (open-line 1)
      (forward-line 1)
      (yank)
      (move-beginning-of-line 1)
      )

    (defun shell-command-on-buffer ()
      (interactive)
      (let ((default-directory (if (file-exists-p default-directory) default-directory "/tmp/")))
        (shell-command-on-region (point-min) (point-max) (read-shell-command "Shell command on buffer: ") nil)))

    ;; Out of the box, pressing C-SPC twice activates the mark.  This
    ;; makes a third C-SPC activate the region that existed at the
    ;; time of the first press.
    (defun cycle-active-mark (orig-func &rest args)
      (if (eq transient-mark-mode 'lambda)
          (if  (eq (point) (mark))
              (progn (pop-mark) (activate-mark)) ; third press.  Highlight original region.
            (let ((last-command nil))  ; so set-mark-command doesn't treat this as a repeat
              (apply orig-func args))) ; fourth press
        (apply orig-func args)))       ; first and second press

    (advice-add 'set-mark-command :around 'cycle-active-mark)
    (advice-remove 'set-mark-command 'cycle-active-mark)
    (advice-add 'other-window :after 'clear-transient-mark-mode)

    (defun clear-transient-mark-mode (&rest args)
      (transient-mark-mode 0))

    ;; Press C-u C-u C-SPC to unpop.
    (defun unpop-to-mark-command ()
      "Unpop off mark ring. Does nothing if mark ring is empty."
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))

        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring))))
        (message "Mark unpopped")
        (setq this-command 'unpop-to-mark-command)
        ))

    (defun set-mark-command-maybe-unpop (orig-func &rest args)
      (let ((prefix (prefix-numeric-value (car args))))
        (if (and (not (boundp 'set-mark-recurse)) (> prefix 4)) ;; unpopping
            (unpop-to-mark-command)
          (when (and (not (boundp 'set-mark-recurse)) (not (eq (point) (mark))) (= prefix 4)) ;;popping
            ;; if we're not already on the mark, then push and pop to save place so we can unpop
            (let ((set-mark-recurse t))
              (let ((current-prefix-arg nil)) (call-interactively 'set-mark-command))
              (let ((current-prefix-arg '(4))) (call-interactively 'set-mark-command))))
          (apply orig-func args))))

    (advice-add 'set-mark-command :around 'set-mark-command-maybe-unpop)
    (advice-remove 'set-mark-command 'set-mark-command-maybe-unpop)

    (defun exchange-point-and-mark-advice (orig-func &rest args)
      (let ((tmm transient-mark-mode))
        (apply orig-func args)
        (if (eq (car-safe tmm) 'only)
            (setq transient-mark-mode tmm))))

    (advice-add 'exchange-point-and-mark :around 'exchange-point-and-mark-advice)

    ))


;;; *** autorevert
(use-package autorevert
  :defer 5
  :config (global-auto-revert-mode 1)
  :diminish auto-revert-mode)

;;; *** multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-." . mc/mark-next-like-this)
         ;("C-," . mc/mark-previous-like-this)
         ("C-M-." . mc/mark-all-like-this-or-edit-lines)
         ("C-S-s-SPC" . mc/mark-all-like-this-or-edit-lines)
         ("C-S-SPC" . mc/mark-next-like-this-or-edit-lines))
  :config
  (progn
    (defun mc/mark-next-like-this-or-edit-lines ()
      (interactive)
      (if (or (= (line-number-at-pos) (line-number-at-pos (mark)))
              (not (use-region-p)))
          (call-interactively 'mc/mark-next-like-this-symbol)
        (call-interactively 'mc/edit-lines)))
    
    (defun mc/mark-all-like-this-or-edit-lines ()
      (interactive)
      (if (or (= (line-number-at-pos) (line-number-at-pos (mark)))
              (not (use-region-p)))
          (call-interactively 'mc/mark-all-like-this-dwim)
        (call-interactively 'mc/edit-lines)))

    (patch-function 'mc--select-thing-at-point "(set-mark (car bound))" "\\&(activate-mark)")
    ))

;;; *** wgrep
(use-package wgrep
  :ensure t
  :config
  (progn
    ;; disabling grep's read only mode enters wgrep mode.  Mirrors dired.
    (defun add-C-x-C-q-binding ()
      (define-key wgrep-original-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))
    (add-hook 'wgrep-setup-hook 'add-C-x-C-q-binding)))

;;; *** wgrep-ag
(use-package wgrep-ag
  :ensure t)

;;; *** visible-mark
'(use-package visible-mark
  :defer 5
  :init
  '(defface visible-mark-active ;; put this before (require 'visible-mark)
    '((((type tty) (class mono)))
      (t (:background "magenta"))) "")
  :config (global-visible-mark-mode 1))

;;; *** expand-region
(use-package expand-region
  :ensure t
  :bind* ("M-e" . er/expand-region)
  :config
  (add-hook 'java-mode-hook 'er/add-cc-mode-expansions))

;;; *** editfns.c
(progn                                  ;editfns.c
  (put 'narrow-to-region 'disabled nil))

;;; *** broadcast-mode
(use-package broadcast
  :commands (broadcast-mode))

;;; *** avy-zap

'(use-package avy-zap
  :bind (("M-Z" . avy-zap-to-char-dwim)
         ("M-z" . avy-zap-up-to-char-dwim))
  :config (setq avy-timeout-seconds .3)
  :ensure t)

;;; *** key-chord
(use-package key-chord
  :ensure t
  :config
  (progn
    (key-chord-define-global "kk" 'kill-this-buffer)
    (key-chord-define-global "KK" 'kill-this-buffer-and-delete-window)
    (key-chord-define emacs-lisp-mode-map "xx" 'eval-defun)
    (key-chord-define lisp-interaction-mode-map "xx" 'eval-defun)
    (key-chord-define-global "xb" 'helm-mini ;'ido-switch-buffer
                             )
    (key-chord-define-global "xf" 'helm-find-files ;'ido-find-file
                             )
    (key-chord-define-global "xs" 'save-buffer)
    (key-chord-define-global "CC" 'calc-dispatch)
    (key-chord-define-global "aa" (lambda () (interactive) (org-agenda nil "u")))
    (key-chord-define-global "ii" (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
    (key-chord-define-global "LL" (lambda () (interactive) (find-file "~/org/life.org")))
    (key-chord-define-global "PP" 'org-passwords)
    (key-chord-define emacs-lisp-mode-map "XX" 'eval-buffer)

    (defun notes ()
      (interactive)
      (let ((notes-filename (expand-file-name "~/.emacs.d/notes")))
        (if (equal (buffer-file-name) notes-filename)
            (progn
              (save-buffer)
              (erase-buffer)
              (normal-mode)
              (message "Notes saved and buffer erased"))
          (message "Switched to notes file")
          (find-file notes-filename))))
    
    (key-chord-define-global "``" 'notes)
    
    (key-chord-mode 1)))
;;; *** flycheck
(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))
;;; *** flyspell
(use-package flyspell
  :config
  (global-set-key (kbd "<mouse-3>") 'flyspell-correct-word)
  )
;;; *** hide-lines
(use-package hide-lines
  :ensure t)
;;; *** help-at-pt
(use-package help-at-pt
  :config
  (progn
    (setq help-at-pt-display-when-idle t)
    (setq help-at-pt-timer-delay 0.1)
    (help-at-pt-set-timer)))
;;; ** Terminal
;;; *** tramp
(use-package tramp
  :config
  (progn
    (setq tramp-auto-save-directory "/tmp")
    (setq tramp-methods
          ;; add quotes qround %u to support domain\user
          (mapcar
           (lambda (method)
             (let ((method-name (car method)))
               (cons method-name
                     (mapcar
                      (lambda (prop)
                        (let ((propname (car prop))
                              (propvallist (cdr prop)))
                          (cons propname
                                (mapcar
                                 (lambda (propval)
                                   (if (and (eq 'tramp-login-args propname) (string-equal "scp" method-name))
                                       (mapcar
                                        (lambda (arggroup)
                                          (mapcar
                                           (lambda (arg)
                                             (if (string-equal arg "%u") "'%u'" arg)) arggroup))
                                        propval)
                                     propval))
                                 propvallist))))
                      (cdr method))
                     ))) tramp-methods))
    ;; disable vc for tramp
    (setq vc-ignore-dir-regexp
          (format "\\(%s\\)\\|\\(%s\\)"
                  vc-ignore-dir-regexp
                  tramp-file-name-regexp))
    (defun tramp-set-auto-save ()
      (auto-save-mode -1)) ;; disable auto-save
    (defun tramp-handle-find-backup-file-name (filename)
      "Like `find-backup-file-name' for Tramp files."
      (with-parsed-tramp-file-name filename nil
        (tramp-run-real-handler 'find-backup-file-name (list filename))))
    )
  )

(use-package tramp-sh
  :config
  (progn
    ;; fix tramp shell prompt.
    (patch-function
     'tramp-sh-handle-start-file-process
     "(tramp-file-name-localname v)" "\"$PWD\"")
    ))

(use-package docker-tramp
  :ensure t
  :config
  (defun no-default-host-for-docker (orig-func method)
    ;(message "method is %s" method)
    (if (string-equal "docker" method)
        (progn
          '(message "%s"
                            ;; This gets the wrong backtrace!
                            (with-temp-buffer
                              (let ((standard-output (current-buffer)))
                                (backtrace)
                                (buffer-string))))
          nil )
      (funcall orig-func method)))
  
  (advice-add 'tramp-parse-default-user-host :around 'no-default-host-for-docker )
  )

;;; *** multi-term
'(use-package multi-term
  :ensure t
  :bind
  (("<M-s-right>" . multi-term-next)
   ("<M-s-left>" . multi-term-prev)
   ("<M-S-s-right>" . multi-term)
   ("<M-S-s-left>" . multi-term)))

;;; *** term
(use-package term
  :commands ansi-term
  :config
  (progn
    ;; make paste and auto-complete work in terminal
    (add-hook
     'term-mode-hook
     (lambda ()
       (define-key term-raw-map (kbd "C-y") 'term-paste)
       (define-key term-raw-map (kbd "s-v") 'term-paste)
       (define-key term-raw-map (kbd "M-/")
         (lambda ()
           (interactive)
           (let ((beg (point)))
             (dabbrev-expand nil)
             (kill-region beg (point)))
           (term-send-raw-string (substring-no-properties (current-kill 0)))))))

    ;; Keep terminal from sending trailing whitespace
    (defadvice term-send-input (before remove-trailing-whitespace () activate)
      "Remove trailing whitespace before sending it in the terminal"
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-max))
          (delete-horizontal-space)))))


  (setq term-prompt-regexp  "^[^#$%>\n]*[#$%>] *")

  (defun setup-ansi-term ()
    (set (make-local-variable 'scroll-margin) 0)
    (message "it is set up")
    )
  (add-hook 'term-mode-hook 'setup-ansi-term)

  )

;;; *** comint
(use-package comint
  :init
  (progn


    (defun goto-end-of-buffer (&rest args)
      (goto-char (point-max)))
    ;; move to end of buffer before calling comint-prevous-input
    (advice-add 'comint-previous-input :before #'goto-end-of-buffer )
    (advice-add 'comint-next-input :before #'goto-end-of-buffer )

    ;; restart shell if it has exited.
    (defun comint-send-input-reattach (&rest args)
      (when (and (eq major-mode 'shell-mode) (not (get-buffer-process (current-buffer))))
        (let ((dir default-directory))
          (cd "/")
          (cd dir)
          (shell (current-buffer)))))

    (advice-add 'comint-send-input :before 'comint-send-input-reattach )

    ;; this next bit addresses a bug caused by gnuplot-mode, which
    ;; incorrectly sets the global value of comint-process-echoes to
    ;; t, causing shells created after that time to freeze up waiting
    ;; for the shell to echo.
    (defun comint-reset-variable ()
      (setq-default comint-process-echoes nil))
    (advice-add 'comint-mode :before 'comint-reset-variable )
    ))

;;; *** better-shell
(use-package better-shell
  :bind* (("C-'" . better-shell-shell)
          ("C-;" . better-shell-remote-open)))

;;; *** shell
(use-package shell
  :commands shell
  :config
  (progn

    (defun shell-command-on-current-file (command)
      "run a command on the current file and revert the buffer"
      (interactive "sShell command: ")
      (shell-command
       (concat command " "
               (shell-quote-argument (buffer-file-name))))
      ;(revert-buffer t t t)
      )

    (defun shell-clear-screen (input)
      (when (equal input "clear\n")
        (comint-clear-buffer))) ; make clear command work in shell
    
    (defun shell-hide-some-escape-chars (output)
      (replace-regexp-in-string "\\[[0-9]+[GKB]" "" output))

    (add-to-list
     'comint-preoutput-filter-functions
     'shell-hide-some-escape-chars) ;; hiding some escape characters

    (add-to-list
     'comint-input-filter-functions
     'shell-clear-screen)

    (defun fix-ansi-color-codes ()
      (interactive)
      (goto-char (point-max))
      (insert "echo -e \"\033[m\"")
      (comint-send-input nil t))
    
    (defun shell-active-space ()
      (interactive)
      (cond ((save-excursion
               (beginning-of-line)
               (looking-at "cd $"))
             (helm :sources
                   (helm-build-in-buffer-source "Projectile projects"
                     :data 'projectile-relevant-known-projects
                     :fuzzy-match helm-projectile-fuzzy-match
                     :action '(("cd to project" .
                                (lambda (project)
                                  (insert project)
                                  (comint-send-input)))))
                   :buffer "*helm projectile*"
                   :prompt (projectile-prepend-project-name "cd: "))
             )
            ((save-excursion
               (beginning-of-line)
               (backward-char 1)
               (beginning-of-line)
               (or
                (looking-at "mysql>")
                (looking-at "[^ ]*:[^ ]*>")))
             (sql-magic-space))
            
            (t (insert " "))))

    (define-key shell-mode-map " " 'shell-active-space)
    (add-hook 'shell-mode-hook 'dirtrack-mode )))

;;; *** mysql-shell
(use-package mysql-shell
  :commands (sql-magic-space)
  :bind (:map shell-mode-map ("s-l" . mysql-cycle-limit)))

;;; *** cssh
(use-package cssh
  :config (progn
            (cssh-define-global-bindings)
            (defun cssh-dir ()
              (interactive)
              (find-file "~/.emacs.d/cssh")
              )))

;(bind-key "C-;" 'shell-remote-open)

;;; ** External System Integration

(use-package gnuplot
  :ensure t)

;;; *** elfeed
(use-package elfeed
  :ensure t
  :config

  (defun elfeed-show-refresh--my-style ()
    (interactive)
    (elfeed-show-refresh--mail-style)
    (let ((inhibit-read-only t)) (elfeed-inline-enclosure)))

  (defun elfeed-inline-enclosure ()
    "Inline the enclosures when they are images"
    (goto-char (point-max))
    (cl-loop for enclosure in (elfeed-entry-enclosures elfeed-show-entry)
             do (when (string-prefix-p "image/" (cadr enclosure)) (elfeed-insert-link (concat "<img src='" (car enclosure) "' />")))
             (insert "\n"))
    (goto-char (point-min)))

  (setq elfeed-show-refresh-function #'elfeed-show-refresh--my-style)
  (defun elfeed-switch-to-entry-buffer (&rest args)
    "This is so that the shr image rendering will have a current window to use a max for image size."
    (switch-to-buffer "*elfeed-entry*"))
  
  (advice-add 'elfeed-show-entry :before #'elfeed-switch-to-entry-buffer)

  (defun elfeed-show-visit-eww ()
    "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument, visit the current entry in the
browser defined by `browse-url-generic-program'."
    (interactive)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link
        (message "Sent to eww: %s" link)
        (eww-browse-url link))))

  (define-key elfeed-show-mode-map  "e" 'elfeed-show-visit-eww)
  
  )

;;; *** slack
(use-package slack
  :commands (slack-start)
  :after (spaceline)
  :bind
  (("C-, c" . slack-channel-select)
   ("C-, i" . slack-im-select))
  :ensure t
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (setq slack-request-timeout 500
        slack-display-team-name nil
        lui-time-stamp-format "[%Y-%m-%d %H:%M]"
        lui-fill-column 70
        )
  
  (patch-function 'slack-channel-list-update
                  "(mapc #'(lambda (room)
                            (slack-room-info-request room team))
                        (oref team channels))"
                  "")

  (patch-function 'slack-group-list-update
                  "(mapc #'(lambda (room)
                            (slack-room-info-request room team))
                        (oref team groups))"
                  "")

  (patch-function 'slack-im-update-room-list
                  "(mapc #'(lambda (room)
                          (slack-room-info-request room team))
                      (oref team ims))"
                  "")

  

  (unless slack-current-team
    (slack-register-team
     :name slack-name
     :default t
     :modeline-enabled nil
     :client-id slack-client-id
     :client-secret (slack-client-secret)
     :token slack-token
     :subscribed-channels slack-subscribed-channels
     :full-and-display-names t
     ))

  (defun tracking-shorten-advice (orig-func &rest args)
    (mapcar
     (lambda (s)
       (replace-regexp-in-string "^.*: *" "" s))
     (apply orig-func args)))

  (advice-add 'tracking-shorten :around 'tracking-shorten-advice)

  (spaceline-define-segment slack-track
    "Show the Slack buffers with new messages."
    (when (fboundp 'tracking-status)
      (tracking-status)))

  (spaceline-emacs-theme 'slack-track)
  (spaceline-toggle-slack-track-on)
  
  (defun slack-user-status (id team)
    "")

  (with-eval-after-load 'tracking
    (define-key tracking-mode-map [f11]
      #'tracking-next-buffer))
  ;; Ensure the buffer exists when a message arrives on a
  ;; channel that wasn't open.
  (setq slack-buffer-create-on-notify t)

  (define-key slack-mode-map "@"
    (defun endless/slack-message-embed-mention ()
      (interactive)
      (call-interactively #'slack-message-embed-mention)
      (insert " ")))
  
  (defun endless/-author-at (pos)
    (if (or (> pos (point-max)) (< pos (point-min)))
        nil
      (replace-regexp-in-string
       (rx "\n" (* anything)) ""
       (or (get-text-property pos 'lui-raw-text) ""))))

  (defun endless/-remove-slack-author ()
    "Remove author here if it's the same as above."
    (let ((author-here (endless/-author-at (point)))
          (author-above (endless/-author-at (1- (point)))))
      (when (and (looking-at-p (regexp-quote author-here))
                 (equal author-here author-above))
        (delete-region (1- (point))
                       (1+ (line-end-position))))))

  (defun endless/remove-slack-author-hook ()
    "For usage in `lui-pre-output-hook'."
    (when (derived-mode-p 'slack-mode)
      (save-excursion
        (goto-char (point-min))
        (save-restriction
          (widen)
          (endless/-remove-slack-author)))))

  (add-hook 'lui-pre-output-hook
            #'endless/remove-slack-author-hook)

  ;; Pretty straightforward.
  (define-key slack-mode-map (kbd "C-c C-d")
    #'slack-message-delete)
  (define-key slack-mode-map (kbd "C-c C-e")
    #'slack-message-edit)
  (define-key slack-mode-map (kbd "C-c C-r")
    #'slack-message-add-reaction)
  (define-key slack-mode-map (kbd "C-c C-k")
    #'slack-channel-leave)

  )

(use-package alert
  :commands (alert)
  :ensure t
  :config
  (setq alert-default-style 'message)
  (setq alert-user-configuration nil
        alert-notifier-command "/opt/local/bin/terminal-notifier")
  (add-to-list 'alert-user-configuration
               '(((:category . "slack")) notifier nil))
  )

;;; *** org-gcal
(use-package org-gcal
  :ensure t)

;;; *** org-mime
;; http://kitchingroup.cheme.cmu.edu/blog/2016/10/29/Sending-html-emails-from-org-mode-with-org-mime/
(use-package org-mime
  :ensure t
  :config
  (progn
    (patch-function
     'org-mime-compose
     "(message-mail "
     "(compose-mail ")
    (defun org-mime-compose-after (&rest args)
      (message-goto-to))
    (advice-add 'org-mime-compose :after 'org-mime-compose-after)))

;;; *** impatient-mode
(use-package impatient-mode
  :ensure t)

;;; *** async
(use-package async
  :ensure t
  :config
  (progn
    ;; makes sending mails asynchronous so they don't block emacs.  
    (require 'smtpmail-async)
    '(setq send-mail-function 'async-smtpmail-send-it
          message-send-mail-function 'async-smtpmail-send-it)
    '(setq send-mail-function 'smtpmail-send-it
          message-send-mail-function 'smtpmail-send-it)
    ))

;;; *** reveal-in-osx-finder
(use-package reveal-in-osx-finder
  :ensure t
  :bind ("s-o". reveal-in-osx-finder))

;;; *** dig
(use-package dig
  :config
  (put 'dig 'interactive-form '(interactive (list (completing-read "Host: " (progn (require 'cssh) (cssh-get-hosts-list)))))))

;;; *** helm-spotify
(use-package helm-spotify
  :ensure t
  :bind ("<f8>" . helm-spotify))

;;; *** butler
'(use-package butler
  :commands butler-status
  :ensure t
  :init
  (progn
    ;; a little hack to address a bug in butler
    (defun butler-handle-unknown-expected (orig-func timestamp expected)
      (if (= -1 expected)
          " |??????????| "
        (funcall orig-func timestamp expected)))
    
    (advice-add 'generate-progress-string :around #'butler-handle-unknown-expected)

    (setq web-log-info nil)))

;;; *** jenkins
(use-package jenkins
  :commands jenkins jenkins-job-call-build
  :ensure t
  :config
  (patch-function 'jenkins-job-details-screen
                  "(plist-get (cdr it) :timestring)"
                  "(plist-get (cdr it) :timestring)
                            (if (equal (plist-get (cdr it) :building) :json-true) \"building...\" \"\")"
                  "Job #%s, %s %s"
                  "Job #%s, %s %s %s"
                  )
  )

;;; *** ahg
(use-package ahg
  :bind-keymap* ("C-c h g" . ahg-global-map)
  :defer t
  :config
  (progn

    (defun hg-nuke ()
      (interactive)
      (shell-command-on-current-file "hg rm")
      (kill-buffer))
    
    (global-set-key (kbd "C-c h g SPC") 'ahg-do-command)
    ;; Ahg key binding for allowing clicking to get a diff
    (define-key ahg-log-file-line-map [mouse-1]
      (lambda ()
        (interactive)
        (let* ((r1 (ahg-log-revision-at-point t))
               (r2 (ahg-first-parent-of-rev r1))
               (fn (ahg-log-filename-at-point (point))))
          (ahg-diff r2 r1 (list fn)))))))

;;; *** gist
(use-package gist
  :ensure t
  :config )

;;; *** magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (progn
    (setq magit-diff-refine-hunk 'all)
    '(defalias 'vc-print-log 'magit-log-buffer-file )
    (global-magit-file-mode 1)

    ;; fix for weird bug.  Sometimes, only when a file has been
    ;; opened, file-accessible-directory-p returns true for a file.
    ;; This fix ensures that it has to be a directory for it to return
    ;; true.
    (defun my-file-accessible-directory-p (orig-func &rest args)
      (and (apply orig-func args)
           (file-directory-p (car args))))
    (advice-add 'file-accessible-directory-p :around 'my-file-accessible-directory-p)
    ;; view specfic version when looking at log
    (define-key magit-commit-section-map "O" 'magit-find-file)

    (magit-define-popup-option 'magit-log-popup ?b "Before" "--before")
    (magit-define-popup-switch 'magit-log-popup
      ?m "Omit merge commits" "--no-merges")
    
    ))

;;; *** git-messenger
(use-package git-messenger
  :ensure
  :bind ("C-x v p" . git-messenger:popup-message)
  :config
  (progn

    ;; Use magit-show-commit for showing status/diff commands
    (setq git-messenger:show-detail t
          git-messenger:use-magit-popup t)
    
    '(patch-function 'git-messenger:hg-commit-message "-T" "--template") ;; work with older hg version
    '(patch-function 'git-messenger:hg-commit-date "-T" "--template") ;; work with older hg version
    '(patch-function 'git-messenger:popup-common
                    ;; *git-messenger* buffer needs to be recreated to ensure that its default-directory is the right one.
                    "(with-current-buffer " "(when (get-buffer \"*git-messenger*\") (kill-buffer \"*git-messenger*\"))
 (with-current-buffer ")
    '(patch-function 'git-messenger:popup-common
                    ;; don't do magit popup if it's not git
                    "git-messenger:use-magit-popup" '(and (eq 'git vcs) git-messenger:use-magit-popup))

    '(patch-function 'git-messenger:popup-common )))

;;; *** git-gutter
(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :bind (("C-c g p" . git-gutter:popup-hunk)
         ("C-c g s" . git-gutter:stage-hunk)
         ("C-c g r" . git-gutter:revert-hunk))
  :demand t
  :config
  (progn
    (set-face-foreground 'git-gutter-fr:modified "yellow")
    (custom-set-variables
     '(git-gutter:update-interval 2)) 
    (setq git-gutter:handled-backends '(git hg))
    (global-git-gutter-mode +1)
    ))

;;; *** git-timemachine
(use-package git-timemachine
  :ensure)

;;; *** magit-gh-pulls
'(use-package magit-gh-pulls
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

;;; *** restclient
(use-package restclient
  :mode (("\\.rest$" . restclient-mode))
  :ensure t
  :config
  (progn
    (defun restclient-expand-whitespace ()
      (interactive)
      (with-current-buffer restclient-same-buffer-response-name
        (goto-char (point-min))
        (while (search-forward "\\n" nil t)
          (replace-match "\n" nil t))
        (goto-char (point-min))
        (while (search-forward "\\t" nil t)
          (replace-match "\t" nil t)))
      (goto-char (point-min)))
    
    (defun restclient-indexer-process ()
      (interactive)
      (restclient-expand-whitespace)
      (highlight-lines-matching-regexp "indexer")
      (highlight-lines-matching-regexp "Caused by" 'hi-pink))

    (add-hook 'restclient-response-loaded-hook 'restclient-indexer-process)

    ;; fix syntax table so hippie-expand works on query parameters
    (modify-syntax-entry ?& "." restclient-mode-syntax-table) ;; punctuation
    
    ))
;;; *** ob-restclient
(use-package ob-restclient
  :ensure t)

;;; *** google-this
'(use-package google-this
  :bind ("s-g" . google-this-lucky-search))

;;; *** nodejs-repl
(use-package nodejs-repl
  :ensure t
  :commands nodejs-repl)

;;; *** play-sound
(unless (and (fboundp 'play-sound-internal)
             (subrp (symbol-function 'play-sound-internal)))
  (use-package play-sound))

;;; *** chrome helper functions
(defun chrome-url ()
  "Get current chrome url."
  (do-applescript
   (concat
    "tell application \"Google Chrome\"\n"
    " return URL of active tab of front window\n"
    "end tell\n")))

(defun chrome-host ()
  "Get current chrome host."
  (replace-regexp-in-string
   "https?://\\([^/]+\\)/.*" "\\1" (chrome-url)))

;;; ** Getting Around (search, navigation)

;;; *** Elisp navigation
;; from http://emacsredux.com/blog/2014/06/18/quickly-find-emacs-lisp-sources/
(use-package elisp-slime-nav
  :ensure t
  :diminish
  :config
  (progn
   (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
     (add-hook hook 'elisp-slime-nav-mode))))

;;; *** beacon
(use-package beacon
  :ensure t
  :config
  (progn
    (setq beacon-blink-when-window-scrolls nil)
    (beacon-mode 1))
  :diminish)

;;; *** spaceline
(use-package spaceline
  :ensure t
  :config
  (progn
    (require 'spaceline-config)
    (setq powerline-default-separator 'roundstub)
    (spaceline-emacs-theme)
    (setq spaceline-always-show-segments t)
    (spaceline-toggle-selection-info-off)
    (spaceline-toggle-buffer-size-off)
    ;(spaceline-toggle-mu4e-alert-segment-on)
    ;(spaceline-toggle-erc-track-on)

    (spaceline-define-segment buffer-id
      "Name of buffer."
      (s-trim (powerline-buffer-id 'mode-line-buffer-id))
      :priority -1000)
    
    ))

;;; *** avy
(use-package avy
  :ensure t
  :bind* ("M-j" . avy-goto-char-timer)
  :config (setq avy-timeout-seconds .3))

;;; *** projectile
(setq helm-projectile-fuzzy-match t)
(use-package projectile
  :ensure t
  :commands projectile-global-mode
  :init (projectile-mode)
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-completion-system 'helm
          projectile-require-project-root nil)
    (defun projectile-project-root-advice (orig-func &rest args)
      (if (file-remote-p default-directory)
          nil ;; no projectile stuff in remote directores.  
        (apply orig-func args)))
    (advice-add 'projectile-project-root :around 'projectile-project-root-advice)
    (projectile-cleanup-known-projects)

    ;; this allows it to work in buffers that are not associated with a file, but are associated with a directory like a shell
    ;; note: 
    ;; after upgrading projectile, this failed, so quoted it out.  Perhaps it's not needed any more
    '(patch-function 'projectile-project-root
                    "(or buffer-file-name 'none)"
                    '(or buffer-file-name default-directory 'none)
                    )
    )
  )

;;; *** helm-projectile
(use-package helm-projectile
  :ensure t
   :config (progn
             ;; (setq helm-projectile-fuzzy-match t) ;; this should be the default
             (helm-projectile-on)
             (defun my-find-file-in-projects (&optional arg)
               (interactive "P")
               (if (projectile-project-p)
                   (projectile-maybe-invalidate-cache arg))
               (helm :sources 'helm-source-projectile-files-in-all-projects-list
                     :buffer "*helm projectile*"
                     :prompt "Find file in projects (M-n for current file): "
                     :default (and (projectile-project-p) buffer-file-name
                                   (file-relative-name
                                    buffer-file-name (projectile-project-root)))))
             
             (defalias 'helm-projectile-find-file-in-known-projects 'my-find-file-in-projects  )))

;;; *** helm-ag
(use-package helm-ag
  :ensure t
  :config
  (progn
    (custom-set-variables '(helm-ag-use-grep-ignore-list t))
    ;; ag-truncate is used to tame ag results from minimized javascript files
    (custom-set-variables '(helm-ag-base-command "~/.emacs.d/ag-truncate.sh --nocolor --nogroup --smart-case --column"))
    (defun helm-find-files-ag (candidate)
      "Default action to grep files from `helm-find-files'.  Replaces helm-find-files-grep."
      (let ((default-directory candidate))
        (helm-projectile-ag)))
    (defalias 'helm-find-files-grep 'helm-find-files-ag)
    (key-chord-define-global "SS" 'helm-projectile-ag)
    (key-chord-define-global "FF" 'helm-projectile-find-file)
    '(defalias 'helm-ff-run-grep 'helm-projectile-ag)
    ;; this is when you press C-s after C-c p p
    (defalias 'helm-projectile-grep 'helm-find-files-ag)))

;;; *** grep
(use-package grep
  :config
  (progn
    (add-to-list 'grep-find-ignored-files "*.pdf")
    (add-to-list 'grep-find-ignored-files "*.orig")
    (add-to-list 'grep-find-ignored-files "pts.js")
    (add-to-list 'grep-find-ignored-files "*.log")
    (add-to-list 'grep-find-ignored-files "*.min.js")
    (add-to-list 'grep-find-ignored-files "*.log.*")
    (add-to-list 'grep-find-ignored-files "#*#")
    (add-to-list 'grep-find-ignored-files "*.orig")
    (add-to-list 'grep-find-ignored-files "*.phar")
    (add-to-list 'grep-find-ignored-directories "templates_c")
    (add-to-list 'grep-find-ignored-directories "ci_system")
    (add-to-list 'grep-find-ignored-directories "bin5")
    (add-to-list 'grep-find-ignored-directories "bin")
    (add-to-list 'grep-find-ignored-directories "build/")
    (add-to-list 'grep-find-ignored-directories "docs/api")))

;;; *** ag
(use-package ag
  :ensure t
  :config
  (progn
    ;; integrate helm-ag into ag and wgrep-ag
    ;; helm-ag has its own inline editing, which I don't like as well as helm-ag/wgrep-ag. 
    (add-hook 'helm-ag-mode-hook 'ag-mode)
    (defalias 'helm-ag-edit 'helm-ag--run-save-buffer )))

;;; *** smartscan
(use-package smartscan
  :ensure t
  :bind (("M-n" . smartscan-symbol-go-forward)
         ("M-p" . smartscan-symbol-go-backward)))

;;; *** isearch.el
(progn                          ;isearch.el
  (setq isearch-allow-scroll t) ;; so scrolling doesn't exit incremental search
  )

;;; *** isearch+
                                        ;(use-package isearch+) do we need this with swiper?

;;; *** replace.el
(progn
  (defun occur-save ()
    (interactive)
    (with-current-buffer (marker-buffer (get-text-property (point) 'occur-target))
      (call-interactively 'save-buffer)))
  (define-key occur-edit-mode-map (kbd "C-x C-s" ) 'occur-save)
  ;; key bindings to mirror dired
  (define-key occur-edit-mode-map (kbd "C-x C-q" ) 'occur-cease-edit)
  (define-key occur-mode-map (kbd "C-x C-q") 'occur-edit-mode)
  )

;;; *** emacs-anzu
;; cool highlighting when doing search and replace
(use-package anzu
  :ensure t
  :config
  (progn
    (global-set-key [remap query-replace] 'anzu-query-replace)
    (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
    (global-set-key [remap isearch-query-replace] 'anzu-isearch-query-replace)
    (global-set-key [remap isearch-query-replace-regexp] 'anzu-isearch-query-replace-regexp)
    
    ))

;;; *** swiper
(use-package swiper
  :ensure t
  ;; :bind (("C-s" . swiper-or-isearch-forward)
  ;;        ("C-r" . swiper-or-isearch-backward))
  :config
  (progn
    (global-set-key [C-s-268632083] 'swiper-helm)

    (setq ivy-display-style 'fancy)
    ;; old way (member major-mode isearch-modes)
    (defun can-use-swiper ()
      (< (buffer-size) 5000000))
    
    (defun swiper-or-isearch-forward ()
      (interactive)
      (if (can-use-swiper)
          (call-interactively 'swiper-helm)
        (call-interactively 'isearch-forward)))
    
    (defun swiper-or-isearch-backward ()
      (interactive)
      (if (can-use-swiper)
          (call-interactively 'swiper-helm)
        (call-interactively 'isearch-backward)))))



;;; *** swiper-helm
(use-package swiper-helm
  :ensure t
  :config
  (progn
    (setq swiper-helm-display-function 'helm-default-display-buffer)
    ;; the following should have been replaced by similar code under (use-package helm)
    ;; (require 'which-func)
    ;; (defun swiper-update-which-func (&rest args)
    ;;   (which-func-update-1 (helm-persistent-action-display-window)))
    ;; (advice-add 'swiper--update-sel :after 'swiper-update-which-func )
    ))


;;; *** cmds.c
(progn                                  ;cmds.c
  (bind-key "s-<right>" 'end-of-line)
  (bind-key "s-<left>" 'beginning-of-line))

;;; *** smex
'(use-package smex
  :bind* (("M-x" . smex)
          ("M-X" . smex-major-mode-commands)
          ;; This is your old M-x.
          ("M-s-≈" . execute-extended-command) ; this is alt-command-x
          ;; (global-set-key (kbd "M-x") 'execute-extended-command)
          ))

;;; *** ffap
(use-package ffap
  :config
  (progn
    (defun my-ffap-guesser (orig-func &rest args)
      (let* ((ffap-url-regexp nil)
             (retval (apply orig-func args)))
        ;(message "retval is %s" retval)
        (cond
         ((or
           (member retval '("/" "/**" "//"))
           (eq major-mode 'dired-mode))

          nil)
         ((s-starts-with-p "//" retval)
          (replace-regexp-in-string "\\" "/" retval))
         (t retval))
        ))
    (advice-add 'ffap-guesser :around #'my-ffap-guesser)))

;;; *** mwheel
(use-package mwheel
  :init
  (progn
    ;; scroll one line at a time (less "jumpy" than defaults)
    (setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; one line at a time
    (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
    (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
    (setq scroll-step 1) ;; keyboard scroll one line at a time
    ))

;;; *** recentf
(use-package recentf
  :init
  (recentf-mode))

;;; *** table navigation
(use-package table
  :config
    (defun around-table-navigate (orig-func &rest args)
    (let ((line (count-lines 1 (point))))
      (apply orig-func args)
      (let ((diff (- line (count-lines 1 (point)))))
        (next-logical-line diff)
        (forward-word)
        (backward-word)
        )))

  (advice-add 'table-forward-cell :around 'around-table-navigate)
  (advice-add 'table-backward-cell :around 'around-table-navigate)

  )

;;; *** syntax-subword-mode
;; better word navigation
(use-package syntax-subword
  :ensure t
  :config
  (progn
    (global-syntax-subword-mode 1)
    (setq syntax-subword-skip-spaces 'consistent)))

;;; *** dumb-jump
(use-package dumb-jump
  :ensure t)

;;; *** helm
;; better word navigation
(use-package helm
  :bind* (("M-x" . helm-M-x ;;execute-extended-command
           )
          ("s-r" . helm-imenu)
          ;("M-y" . helm-show-kill-ring) ;; find something better to bind this to. 
          ("C-h a" . helm-apropos)
          ("C-? SPC" . helm-all-mark-rings)
          ("C-x C-f" . helm-find-files)
          ("s-g" . helm-google-suggest))
  :chords (("hh" . helm-resume))
  :commands (helm-resume)
  :diminish helm-mode
  :config (progn
            (helm-mode)
            (custom-set-variables
             '(helm-imenu-fuzzy-match t)
             '(helm-apropos-fuzzy-match t)
             '(helm-autoresize-min-height helm-autoresize-max-height)
             )
            (bind-key "M-r" 'helm-comint-input-ring shell-mode-map )
            (advice-add 'helm-comint-input-ring :before 'comint-kill-input)
            
            (defalias 'man 'helm-man-woman)
            (helm-autoresize-mode t)
            ;; helm-mini -- highlight @ words in buffer when they are selected
            (defvar helm-highlight-@pattern-last-buf nil "The last buffer we searched through.")
            (defvar helm-highlight-@pattern-last-regexp nil "The last pattern we searched for.")

            (defun helm-highlight-@pattern (&rest args)
              (if (not current-prefix-arg)
                  (lexical-let ((regexp (cl-loop with pattern = helm-pattern
                                                 for p in (split-string pattern)
                                                 when (string-match "\\`@\\(..+\\)" p)
                                                 return (match-string 1 p)))
                                (buf (current-buffer)))
                    (when regexp
                      (progn
                        (when (not (and (eq helm-highlight-@pattern-last-buf buf)
                                        (equal helm-highlight-@pattern-last-regexp regexp)))
                          (beginning-of-buffer))
                        (let ((font-lock-mode nil))
                          (highlight-regexp regexp))
                        (setq helm-highlight-@pattern-last-buf buf
                              helm-highlight-@pattern-last-regexp regexp)
                        (when (and (eq this-command 'helm-maybe-exit-minibuffer))
                          (re-search-backward regexp nil t))
                        (or (re-search-forward regexp nil t) (progn (beginning-of-buffer) (re-search-forward regexp nil t)))
                        (set-transient-map `(keymap) nil
                                           (lambda () (with-current-buffer buf (unhighlight-regexp regexp)))))))))

            (advice-add 'helm-switch-to-buffers :after 'helm-highlight-@pattern)
            (advice-add 'helm-buffers-list-persistent-action :after 'helm-highlight-@pattern)

            (setq helm-locate-command
                  (cl-case system-type
                    (gnu/linux "locate %s -e -r %s")
                    (berkeley-unix "locate %s %s")
                    (windows-nt "es %s %s")
                    (darwin "mdfind -name %s %s") 
                    (t "locate %s %s")))

            ;; fix helm ffap.  There is a bug where a if a remote file has a
            ;; file reference, it gets discarded as being a candidate for ffap
            ;; behavior by helm because it is remote.  This addresses that.
            ;; note: after update, the patch stopped matching, maybe they've fixed the bug
            '(patch-function
             'helm-find-files-input
             "(and file-at-pt[ \t\r\n]*(not remp)[ \t\r\n]*(file-exists-p file-at-pt))"
             '(and file-at-pt
                   (or (not remp) 
                       (file-exists-p file-at-pt))))

            ;; don't let helm sort if the completing read supplies its own sort function
            ;; this was motivated by mu4e when doing recipient autocompletion.  We want to keep
            ;; mu4e's sorting
            ;; note: patch stopped matching after upgrade. maybe not needed anymore?
            '(patch-function 'helm--completion-in-region
                            "(null helm-completion-in-region-default-sort-fn)"
                            '(or (null helm-completion-in-region-default-sort-fn)
                                 (completion-metadata-get metadata 'display-sort-function)))

            ;; make which-function upadte
            (require 'which-func)
            (defun update-which-func (&rest args)
              (which-func-update-1 (helm-persistent-action-display-window)))
            ;(add-hook 'helm-after-persistent-action-hook 'update-which-func)
            (add-hook 'helm-move-selection-after-hook 'update-which-func)
            ))

(use-package helm-git-grep
  :ensure t)

;;; ** Major Modes

;;; *** go ***
(use-package go-mode
  :mode "\\.go$"
  :ensure t)

;;; *** typesscript-mode
(use-package typescript-mode
  :mode "\\.ts$"
  :ensure t)

;;; *** dot-mode
(use-package graphviz-dot-mode
  :mode "\\.dot$"
  :ensure t
  )
;;; *** ssh-config-mode
(use-package ssh-config-mode
  :ensure t)
;;; *** python-mode
(use-package python-mode
  :ensure t
  :mode "\\.py$"
  :config
  (progn

    (setq py-jython-command "/usr/local/bin/jython")

    (defun find-and-fix-camel-case ()
      (interactive)
      (while (find-camel-case)
        (if (and (not (nth 3 (syntax-ppss))) (y-or-n-p "Fix? " ))
            (camel-to-underscore)
          (forward-word))
        
        )
      )

    (defun find-camel-case ()
      (interactive)
      (let ((case-fold-search))
        (re-search-forward "\\<[a-z0-9]+[A-Z][a-z]+" )
        (backward-word)
        t
        )
      )

    (defun camel-to-underscore ()
      (interactive)
      (let ((start (point)) (end))
        (forward-sexp)
        (setq end (point))
        (goto-char start)
        (syntax-subword-right 1)
        (while (< (point) end)
          (insert "_")
          (syntax-subword-downcase 1)
          )
        )
      )

    
    ))

;;; *** dockerfile-mode
(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile")

;;; *** pdf-view
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq 
   pdf-misc-print-programm "/usr/bin/lpr" 
   pdf-misc-print-programm-args '("-o media=Letter" "-o fitplot")))

;;; *** js2-mode
(use-package js2-mode
  :ensure t
  :mode "\\.js$"
  :config
  (progn
    (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
    (defun js2-short-mode-name ()
      (setq mode-name "JS"))
    (add-hook 'js2-mode-hook 'js2-short-mode-name)
    (defun js2-indent-comments ()
      (local-set-key (kbd "RET") 'c-indent-new-comment-line))
    (add-hook 'js2-mode-hook 'js2-indent-comments)
    (use-package ac-js2
      :ensure t
      :config
      (progn
        ;(add-hook 'js2-mode-hook 'ac-js2-mode) commented out to try tern
        (setq ac-js2-evaluate-calls t) ;; installation instructions from ac-js2, for auto-complete in browser)
        (add-hook 'js2-mode-hook 'jquery-doc-setup)

        ))

    (use-package js2-refactor
      :ensure t
      :config (progn
                (js2r-add-keybindings-with-prefix "C-j")
                (add-hook 'js2-mode-hook 'js2-refactor-mode)
                (define-key js2-mode-map (kbd "C-k") 'js2r-kill)))))

(use-package tern-auto-complete
  :ensure t
  :config
  (progn
    (tern-ac-setup)
    (add-hook 'js2-mode-hook 'tern-mode)))

;;; *** json-mode
(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)
         ("\\.har\\'" . json-mode)
         ("\\.tern-project\\'" . json-mode)))

;;; *** jquery-doc
(use-package jquery-doc
  :ensure t
  ;; Explicitly required in modes that use this.
  :config (progn
            (add-hook 'html-mode-hook 'jquery-doc-setup))
  
  )

;;; *** css-mode
(use-package css-mode
  :mode (("\\.css$" . css-mode)
         ("\\.less$" . css-mode))
  :config
  (use-package css-eldoc
    :ensure t
    :config (css-eldoc-enable)))

;;; *** skewer-mode
(use-package skewer-mode
  :ensure t
  :commands run-skewer
  :config
  (progn
    (add-hook 'js2-mode-hook 'skewer-mode)
    (add-hook 'css-mode-hook 'skewer-css-mode)
    (add-hook 'html-mode-hook 'skewer-html-mode)))

;;; *** yaml-mode
(use-package yaml-mode
  :mode "\\.yaml$"
  :ensure t
  )

;;; *** flymake-yaml
(use-package flymake-yaml
  :ensure t
  :config (add-hook 'yaml-mode-hook 'flymake-yaml-load)
  )

;;; *** php-mode

(defun maio/electric-semicolon ()
  "Insert a semicolon at the end of the line."
  (interactive)
  (end-of-line)
  (when (not (looking-back ";"))
    (insert ";")))

(use-package php-mode
  :ensure t
  :mode (("\\.inc\\'" . php-mode)
         ("\\.php\\'" . php-mode))
  :config
  (progn
    
    (define-key php-mode-map ";" 'maio/electric-semicolon)
    (define-key php-mode-map (kbd "M-r") 'sp-raise-sexp)
    (define-key php-mode-map "M" nil)
    (add-hook 'php-mode-hook 'smartparens-mode)
    (setq php-mode-coding-style 'symfony2)
    (bind-key [(control .)] nil php-mode-map)
    (add-hook 'php-mode-hook 'flycheck-mode)
    ;; we don't want this to overshadow our multi-cursor selection
    (use-package php-eldoc
      :ensure t
      :config
      (add-hook 'php-mode-hook 'php-eldoc-enable))
    (use-package php-extras
      :ensure t)
    '(use-package flymake-php ;; using flycheck now
       :ensure t
       :config
       (add-hook 'php-mode-hook 'flymake-php-load))))

;;; *** geben
(use-package geben
  :commands geben
  :ensure t
  :config
  (progn
    ;; The following bookmarks are useful for php debugging
    ;; 
    ;; Reload in PHP Debugger:
    ;; javascript:document.cookie = "XDEBUG_SESSION=1;path=/";document.location.reload()
    ;; Start PHP Debugger:
    ;; javascript:(function(){document.cookie = "XDEBUG_SESSION=1;path=/"})()
    ;; Stop PHP Debugger:
    ;; javascript:(function(){document.cookie = "XDEBUG_SESSION=;path=/; expires=Thu, 01 Jan 1970 00:00:01 GMT;"})()
    
    ;; geben tooltip
    (defun geben-eval-tooltip (window object pos)
      (when (eq object (current-buffer))
        (let ((expression (geben-expression-at-pos pos)))
          (when (and expression
                     (or (not (equal expression geben-eval-result-expression))
                         (> (- (float-time) geben-eval-result-time) 2)))
            (lexical-let ((buffer (current-buffer)) (expression expression))
              (when (not (equal expression geben-pending-eval-expression))
                                        ;(m "requesting" expression)
                (setq geben-pending-eval-expression expression)
                (geben-plist-append
                 (geben-eval-expression expression)
                 :callback
                 (lambda (session cmd msg err)
                   (with-current-buffer buffer
                     (let* ((value (geben-dbgp-decode-value (car-safe (xml-get-children msg 'property))))
                            (xy (cdr (mouse-pixel-position)))
                            (x (car xy))
                            (y (cdr xy))
                            (pos (nth 1 (posn-at-x-y x y (selected-frame)))))
                       (setq geben-pending-eval-expression nil)
                       (when (equal expression (geben-expression-at-pos pos))
                                        ;(m "setting tooltip")
                         (tooltip-show (prin1-to-string value))
                         (setq geben-eval-result-expression expression)
                         (setq geben-eval-result-time (float-time))
                         ))))))
              nil)))))

    (defun geben-expression-at-pos (pos)
      (save-excursion
        (goto-char pos)
        (let ((symbol (symbol-at-point)))
          (when symbol
            (beginning-of-thing 'symbol)
            (when (= (char-after) ?$)
              (let ((beg (point)))
                (end-of-thing 'symbol)
                (skip-syntax-forward " ")
                (when (and
                       (or
                        (= (char-after) ?\[)
                        (and (= (char-after) ?\-)(= (char-after (+ 1 (point))) ?\>) ))
                       (thing-at-point 'sexp))
                  (forward-sexp))
                (let ((end (point)))
                  (let ((expression (buffer-substring beg end)))
                    (when (not (search "(" expression)) ;; don't want to eval function calls as tooltips.  Could be dangerous.
                      expression)))))))))


    (defun geben-install-tooltip ()
      (set (make-local-variable 'geben-pending-eval-expression) nil)
      (set (make-local-variable 'geben-eval-result-expression) nil)
      (set (make-local-variable 'geben-eval-result-time) nil)
      (let ((inhibit-read-only t))
        (put-text-property (point-min) (point-max) 'help-echo 'geben-eval-tooltip)))

    (defun geben-find-current-file ()
      (interactive)
      (let ((line (line-number-at-pos (point)))) (geben-dbgp-command-source (car geben-sessions) (concat "file://" (buffer-file-name))) (goto-line line)))

    (defmacro geben-with-current-or-active-session (binding &rest body)
      (declare (indent 1)
               (debug (symbolp &rest form)))
      (macroexpand-all
       `(let ((,binding
               (if (memq geben-current-session geben-sessions)
                   geben-current-session
                 (car geben-sessions))))
          (when ,binding
            ,@body))))

    (patch-function 'geben-where "geben-with-current-session" "geben-with-current-or-active-session" )

    (add-hook 'geben-mode-hook 'geben-install-tooltip)

    ;; this is from http://www.dave-cohen.com/node/1000010  There's other good stuff there too
    ;; geben won't connect because its "Already in debugging"  This might help.
    (defun my-geben-release ()
      (interactive)
      (geben-stop)
      (dolist (session geben-sessions)
        (ignore-errors
          (geben-session-release session))))

    (defadvice flymake-start-syntax-check-process (after
                                                   cheeso-advice-flymake-start-syntax-check-1
                                                   (cmd args dir)
                                                   activate compile)
      ;; set flag to allow exit without query on any
      ;;active flymake processes
      (set-process-query-on-exit-flag ad-return-value nil))))

;;; *** web-mode
(use-package web-mode
  :mode (("\\.tpl\\'" . web-mode)
         ("\\.jsp\\'" . web-mode)
         ("\\.asp\\'" . web-mode))
  :ensure t
  :config
  (progn
    (add-hook 'web-mode-hook
             (lambda ()
               (define-key web-mode-map (kbd "M-s s") 'smarty-assignment) ;; we don't want this to overshadow our multi-cursor selection
               ))
    (require 'jquery-doc)
    (add-hook 'web-mode-hook 'jquery-doc-setup))
  )

;;; *** javacc-mode
(use-package javacc-mode
  :mode "\\.jj$")

;;; *** jad-mode
(use-package jad-mode
  :mode "\\.class$")

;;; *** log4j-mode
(use-package log4j-mode
  :ensure t
  :mode (("\\.log$" . log4j-mode)
         ("\\.log\\." . log4j-mode)
         ("catalina\\.out" . log4j-mode))
  :config
  (progn
    (defun log4j-mode-turn-on-font-lock ()
      (set (make-local-variable 'font-lock-support-mode) 'jit-lock-mode))
    (defun highlight-stack-trace (&rest args) ;; args so it can be used in advice
      (interactive)
      (highlight-lines-matching-regexp "Caused by" 'hi-pink)
      (highlight-lines-matching-regexp "at com.ancestry\\|at com.footnote" 'hi-green))
    
    (add-hook 'log4j-mode-hook 'log4j-mode-turn-on-font-lock )
    (add-hook 'log4j-mode-hook 'highlight-stack-trace )))

;;; *** cc-mode
(use-package cc-mode
  :mode (("\\.jad$" . java-mode)
         ("\\java$" . java-mode))
  :config
  (progn

    (defun my-c-setup ()
      (c-set-offset 'substatement-open 0)
      (add-to-list 'imenu-generic-expression
                   '("Lisp Function" "\\s-*DEFUN\\s-*(\\s-*\"\\([^\"]*\\)" 1)))

    (add-hook 'java-mode-hook 'better-java-indexing)
    (add-hook 'java-mode-hook 'smartparens-mode)
    (add-hook 'c-mode-hook 'my-c-setup)
    (add-hook 'java-mode-hook 'my-c-setup)
    (defun java-indent-comments ()
      (local-set-key (kbd "RET") 'c-indent-new-comment-line))
    (add-hook 'java-mode-hook 'java-indent-comments)
    
    (define-key c-mode-base-map ";" 'maio/electric-semicolon)
    
    (defun c-newline ()
      "Inserts a blank line for new braces"
      (interactive)
      (call-interactively 'newline)
      (when (and (looking-at "}")
                 (save-excursion
                   (previous-line)
                   (let* ((p1 (line-beginning-position))
                          (p2 (line-end-position))
                          (line (buffer-substring-no-properties p1 p2)))
                     (string-match "{[ \t]*$" line))
                   ))
        (newline)
        (indent-according-to-mode)
        (previous-line)
        (indent-according-to-mode)))
    (define-key c-mode-base-map (kbd "RET") 'c-newline)
    
    (setq c-default-style '((java-mode . "java")
                            (awk-mode . "awk")
                            (other . "gnu")))

    (defun better-java-indexing ()
      (make-local-variable 'collected-names)
      (setq imenu-prev-index-position-function
            (lambda ()
              (if (= (point) (point-max)) (setq collected-names '()))
              (let (found done)
                (while (not (or done found))
                  (progn
                    (setq done (not (setq found (re-search-backward "[]A-Za-z0-9_>][ \r\n\t]+[A-Za-z_][A-Za-z0-9_]*[ \r\n\t]*(" nil t))))
                    (forward-char)
                    (setq found
                          (and ;; see if this is a false match
                           found
                           (not (member (word-at-point) '("new" "else" "return")))
                           (let ((s (syntax-ppss)))
                             (and
                              (not (nth 4 s))   ;; not in comment
                              (not (nth 3 s)))) ;; not in string
                           (forward-word)
                           (let ((s (syntax-ppss)))
                             (and
                              (not (nth 4 s))   ;; not in comment
                              (not (nth 3 s)))) ;; not in string
                           (backward-word) ;; position at beginning of word
                           ))))
                found)))
      (setq imenu-extract-index-name-function
            (lambda()
              (let ((word (word-at-point)))
                (set-text-properties 0 (length word) nil word)
                (push word collected-names)
                (concat
                 word
                 (let ((count (count-if (lambda (e) (equal word e)) collected-names)))
                   (if (> count 1) (concat "<" (number-to-string count) ">")) ))))))) ())

;;; *** thread-dump
(use-package thread-dump
  :ensure t
  :config
  (progn

    (defun thread-dump-command ()
      "Inserts a command to get the thread dump of the currently-running java process.  Use this in a shell."
      (interactive)
      (insert "file=`sudo ps -ef | grep java|grep -o 'logging.dir=[^ ]*'|sed 's,logging.dir=,,'`/catalina.out;sudo killall -s SIGQUIT java;sleep 1;tac -s 'Full thread dump ' -r $file |awk '/^Heap$/{l=10} {l--;print} l==0 {exit} ' > /tmp/threaddump"))

    ;(advice-add 'thread-dump-overview-visit-thread :after 'highlight-stack-trace) struggling to get this to work

    ))
;;; *** vtl
(use-package vtl
  :mode (".*/email.*\\.txt$" . vtl-mode))       ;opens our email templates as vtl

;;; *** lisp-mode

(use-package lisp-mode
  :init
  (add-hook 'eval-expression-minibuffer-setup-hook
            (lambda ()
              (paredit-mode 1)
              (eldoc-mode 1)
              ))
  :config
  (progn

    (add-hook 'ielm-mode-hook 'enable-paredit-mode)
    (defun eval-and-replace ()
      "Replace the preceding sexp with its value."
      (interactive)
      (backward-kill-sexp)
      (condition-case nil
          (prin1 (eval (read (current-kill 0)))
                 (current-buffer))
        (error (message "Invalid expression")
               (insert (current-kill 0)))))

    (defun my-emacs-lisp-mode-hook ()
      (add-hook 'after-save-hook 'check-parens nil t)
      (add-to-list 'imenu-generic-expression
                   '("Used Packages"
                     "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))

    (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

    (bind-key "C-h C-f" 'find-function-at-point emacs-lisp-mode-map)
    (bind-key "C-h C-f" 'find-function-at-point lisp-interaction-mode-map)

    (bind-key "C-h C-v" 'find-variable-at-point emacs-lisp-mode-map)
    (bind-key "C-h C-v" 'find-variable-at-point lisp-interaction-mode-map)

    (define-key 'help-command (kbd "C-k") 'find-function-on-key)
    
    (use-package paredit
      :ensure t
      :diminish paredit-mode
      :config
      (progn
        (add-hook 'emacs-lisp-mode-hook 'paredit-mode))
      ;; making paredit work with delete-selection-mode
      ;; (from http://whattheemacsd.com/setup-paredit.el-03.html )
      ;; (put 'paredit-forward-delete 'delete-selection 'supersede)
      ;; (put 'paredit-backward-delete 'delete-selection 'supersede)
      ;; (put 'paredit-open-round 'delete-selection t)
      ;; (put 'paredit-open-square 'delete-selection t)
      ;; (put 'paredit-doublequote 'delete-selection t)
      ;; (put 'paredit-newline 'delete-selection t)
      )
    (use-package eldoc
      :diminish eldoc-mode
      :config
      (progn
        (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
        ))))

;;; *** nxml-mode
(use-package nxml-mode
  :mode  (("\\.xml$" . nxml-mode)
          ("\\.xsd$" . nxml-mode))
  :config
  (progn
    ;; make nxml operate on nested element not just tags
    (setq nxml-sexp-element-flag t)

    ;; this is a slight mod to the existing function that doesn't
    ;; consider a line for indentation if it is all whitespace
    (defun nxml-compute-indent-in-delimited-token (pos open-delim close-delim)
      "Return the indent for a line that starts inside a token with delimiters.
OPEN-DELIM and CLOSE-DELIM are strings giving the opening and closing
delimiters.  POS is the position of the first non-whitespace character
of the line.  This expects the xmltok-* variables to be set up as by
`xmltok-forward'."
      (cond ((let ((end (+ pos (length close-delim))))
               (and (<= end (point-max))
                    (string= (buffer-substring-no-properties pos end)
                             close-delim)))
             (goto-char xmltok-start))
            ((progn
               (goto-char pos)
               ;; This is the expression that was changed.
               ;; Was
               ;; (forward-line -1)
               ;; new expression follows.  Skips empty lines.
               (loop
                do (forward-line -1)
                while (looking-at "[ \t]*$"))
               (<= (point) xmltok-start))
             (goto-char (+ xmltok-start (length open-delim)))
             (when (and (string= open-delim "<!--")
                        (looking-at " "))
               (goto-char (1+ (point)))))
            (t (back-to-indentation)))
      (current-column))
    
    (defun nxml-fix-comments ()
      (setq comment-continue " "))
    (add-hook 'nxml-mode-hook 'nxml-fix-comments)))

;;; *** mz-comment-fix
(use-package mz-comment-fix
  :config
  ;; fixes nested comment behavior in nxmlo
  (add-to-list 'comment-strip-start-length (cons 'nxml-mode 3)))

;;; *** auto-complete
(use-package auto-complete
  :ensure t
  :defer 5
  :diminish auto-complete-mode
  :config (progn
            (ac-config-default)))

;;; *** apples-mode
(use-package apples-mode
  :mode "\\.\\(applescri\\|sc\\)pt\\'"
  :ensure t
  :config (bind-key "C-c C-c" 'apples-run-region/buffer apples-mode-map))

;;; *** groovy-mode
(use-package groovy-mode
  :mode "\\.gradle$"
  :ensure t)

;;; *** csv-mode
(use-package csv-mode
  :mode "\\.csv$"
  :ensure t)

;;; *** markdown-mode
(use-package markdown-mode
  :mode "\\.md$"
  :ensure t)

;;; *** sage-shell-mode
(use-package sage-shell-mode
  :ensure t
  :config
  (progn
    ;; Run SageMath by M-x run-sage instead of M-x sage-shell:run-sage
    (sage-shell:define-alias)

    ;; Turn on eldoc-mode in Sage terminal and in Sage source files
    (add-hook 'sage-shell-mode-hook #'eldoc-mode)
    (add-hook 'sage-shell:sage-mode-hook #'eldoc-mode)

    (setq sage-shell:use-prompt-toolkit t)
    )
  )

(use-package ob-sagemath
  :ensure t
  :config
  (progn
    (setq org-babel-default-header-args:sage '((:session . t)
                                               (:results . "output")))
    )
  )

;;; *** conf-mode
(use-package conf-mode
  :mode (("\\.hgrc$" . conf-mode)
         ("\\.service$" . conf-mode))
  :ensure t)

;;; *** ses
(use-package ses-mode
  :mode "\\.ses$"
  :ensure ses
  :init (add-hook 'ses-mode-hook 'linum-mode))

;;; *** highlight
;; only used for hlt-highlight-region by calc below
(use-package highlight
  :ensure t)

;;; *** idle-highlight-mode
(use-package idle-highlight-mode
  :ensure t)

;;; *** calc
(use-package calc
  :init
  (progn
    (setq calc-settings-file "~/.emacs.d/calc-settings.el"))
  :config
  (progn
    (defun calc-call (fun &rest args)
      (string-to-number
       (math-format-value
        (apply
         (intern (concat "calcFunc-" (symbol-name fun)))
         (mapcar
          (lambda (s) (math-read-number-simple (number-to-string s)))
          args)))))
    (require 'hi-lock) ;; for hi-green
    (defun calc-embedded-refresh-everything ()
      (interactive)
      '(calc-embedded-update-formula nil)
      (calc-embedded-activate 1)
      (calc-embedded-update-formula 1)
      (mapc (lambda (info)
              (hlt-highlight-region (elt info 2) (elt info 3) 'hi-green))
            (cdr (assq (current-buffer) calc-embedded-active)))
      (set-transient-map '(keymap) nil (lambda () (hlt-unhighlight-region)))
      )))

;;; ** File, Window and Buffer Management

;;; *** purpose
(use-package window-purpose
  :ensure t
  :config
  (progn
    ;(purpose-mode 1)
    ;(add-to-list 'purpose-user-mode-purposes '(jabber-chat-mode . chat))
    ;(purpose-compile-user-configuration) ; activates changes
    ))
;;; *** windmove
(use-package windmove
  :bind (("s-<right>" . windmove-right)
         ("s-<left>"  . windmove-left)
         ("s-<up>"    . windmove-up)
         ("s-<down>"  . windmove-down)))

;;; *** buffer-move
(use-package buffer-move
  :ensure t
  :bind (( "s-S-<right>" . buf-move-right)
         ( "s-S-<left>"  . buf-move-left)
         ( "s-S-<up>"    . buf-move-up)
         ( "s-S-<down>"  . buf-move-down)))

;;; *** ibuffer
(use-package ibuffer
  :config
  (defalias 'list-buffers 'ibuffer))

;;; *** buffer.c
(progn                                  ;buffer.c
  (bind-key* "C-x C-k" 'kill-this-buffer)
  ;; some bindings from- http://www.masteringemacs.org/articles/2014/02/28/my-emacs-keybindings/
  (defun kill-this-buffer ()
    (interactive)
    (kill-buffer (current-buffer)))
  (defun kill-this-buffer-and-delete-window ()
    (interactive)
    (kill-this-buffer)
    (delete-window))

  ;; See https://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow
  (setq bidi-display-reordering nil)
  
  (put 'erase-buffer 'disabled nil)
  (setq case-fold-search t)
  (setq-default tab-width 4)
  ;; from http://whattheemacsd.com/file-defuns.el-01.html
  (defun rename-current-buffer-file ()
    "Renames current buffer and file it is visiting."
    (interactive)
    (let ((name (buffer-name))
          (filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
          (error "Buffer '%s' is not visiting a file!" name)
        (let ((new-name (read-file-name "New name: " filename)))
          (if (get-buffer new-name)
              (error "A buffer named '%s' already exists!" new-name)
            (rename-file filename new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil)
            (message "File '%s' successfully renamed to '%s'"
                     name (file-name-nondirectory new-name))))))))

;;; *** minibuf.c
(progn                                  ;minibuf.c
  (setq enable-recursive-minibuffers t))

;;; *** winner-mode
(use-package winner
  :init (winner-mode 1))

;;; *** files.el
(progn                                  ;files.el

  ;; make dired faster for mounts by doing a plain ls instead of
  ;; ls -l and faking the details
  (progn
    (setq insert-directory-program  "~/.emacs.d/nfsfastls")
    (write-region "#!/bin/bash
last=${@: -1}
if [[ \"$last\" =~ /mnt/* ]]
then
   ls $last | awk '{print \"0 OO 00 0000 \"$0 }'
else
   ls \"$@\"
fi" nil insert-directory-program)
    (chmod insert-directory-program  #o755))
  
  ;; Settings for backups
  (setq make-backup-files t
        vc-make-backup-files t
        delete-old-versions t
        kept-new-versions 200
        kept-old-versions 0
        backup-by-copying t
        version-control t
        backup-dir  "~/.saves"
        backup-directory-alist `(("." . ,backup-dir))
        tramp-backup-directory-alist backup-directory-alist)



  ;; make sure backup directory exists
  (if (not (file-exists-p backup-dir))
      (make-directory backup-dir))

  ;; force backup on every save
  (defun force-backup-of-buffer ()
    (setq buffer-backed-up nil))
  (add-hook 'before-save-hook 'force-backup-of-buffer)


  ;; Color colors in theme files
  (defun hexcolour-luminance (color)
    "Calculate the luminance of a color string (e.g. \"#ffaa00\", \"blue\").
  This is 0.3 red + 0.59 green + 0.11 blue and always between 0 and 255."
    (let* ((values (x-color-values color))
           (r (car values))
           (g (cadr values))
           (b (caddr values)))
      (floor (+ (* .3 r) (* .59 g) (* .11 b)) 256)))

  (defun hexcolour-add-to-font-lock ()
    (interactive)
    (font-lock-add-keywords
     nil
     `((,(concat "#[0-9a-fA-F]\\{3\\}[0-9a-fA-F]\\{3\\}?\\|"
                 (regexp-opt (x-defined-colors) 'words))
        (0 (let ((colour (match-string-no-properties 0)))
             (put-text-property
              (match-beginning 0) (match-end 0)
              'face `((:foreground ,(if (> 128.0 (hexcolour-luminance colour))
                                        "white" "black"))
                      (:background ,colour)))))))))

  (defun color-if-theme (&rest args)
    (if (s-ends-with-p "-theme.el" (buffer-file-name))
        (hexcolour-add-to-font-lock)))

  (add-hook 'find-file-hook 'color-if-theme)
  )

;;; *** backup-walker
(use-package backup-walker
  :ensure t
  :commands backup-walker-start)

;;; *** gnus-dired
(use-package gnus-dired) ;; load it here so it can be patched below.

;;; *** dired
(use-package dired
  :defer t
  :bind
  (:map
   dired-mode-map
   ("a" . gnus-dired-attach))
  :config
  (progn
    (progn

      (setq wdired-allow-to-change-permissions t)

      (patch-function
       'gnus-dired-attach "\"Attach files to existing mail composition buffer\\? \""
       '(format "Attach files to existing mail composition buffer%s"
                (if (= (length bufs) 1) 
                    (concat " (" (car bufs) ")?") 
                  "?"))
       )

      (put 'dired-find-alternate-file 'disabled nil)

      ;; from http://stackoverflow.com/questions/19907939/how-can-one-quickly-browse-through-lots-of-files-in-emacs
      ;; little modification to dired-mode that let's you browse through lots of files
      '(add-hook 'dired-mode-hook
                (lambda()
                  (define-key dired-mode-map (kbd "C-o") 'dired-view-current)     ; was dired-display-file
                  (define-key dired-mode-map (kbd "n")   'dired-view-next)           ; was dired-next-line
                  (define-key dired-mode-map (kbd "p")   'dired-view-previous))) ; was dired-previous-line

      (defun dired-view-next ()
        "Move down one line and view the current file in another window."
        (interactive)
        (dired-next-line 1)
        (dired-view-current))

      (defun dired-view-previous ()
        "Move up one line and view the current file in another window."
        (interactive)
        (dired-previous-line 1)
        (dired-view-current))

      (defun dired-view-current ()
        "View the current file in another window (possibly newly created)."
        (interactive)
        (require 'windmove)
        (if (not (window-parent))
            (split-window))                                   ; create a new window if necessary
        (let ((file (dired-get-file-for-visit))
              (dbuffer (current-buffer)))
          (other-window 1)                                          ; switch to the other window
          (unless (equal dbuffer (current-buffer))                 ; don't kill the dired buffer
            (if (or view-mode (equal major-mode 'dired-mode))   ; only if in view- or dired-mode
                (kill-buffer)))                                                    ; ... kill it
          (let ((filebuffer (get-file-buffer file)))
            (if filebuffer                              ; does a buffer already look at the file
                (switch-to-buffer filebuffer)                                    ; simply switch 
              (view-file file))                                                    ; ... view it
            '(text-scale-set -2) ;; shrink for newspapers
            ) 
          (let ((pdffile (replace-regexp-in-string "\\.txt$" ".pdf" file)))
            (when (file-exists-p pdffile)
              (if (windmove-find-other-window 'right)
                  (select-window (windmove-find-other-window 'right) t)
                (split-window-right)
                (other-window 1))
              (view-file pdffile)))
          (other-window -2) ; give the attention back to the dired buffer
          ))
      )

    ;; This is a cool function that spins up dired on a list of files.
    ;; It expects one file per line, and expects their paths to be
    ;; absolute or relative to the file list.
    (defun dired-virtual-vanilla ()
      (interactive)
      (let ((bufname (concat "*Dired " (buffer-name) "*"))
            (line (line-number-at-pos (point))))
        (ignore-errors (kill-buffer bufname))
        (dired (cons bufname (split-string (buffer-string) "\n" t)))
        (goto-line (+ 1 line))
        (dired-move-to-filename)))
    (defalias 'dired-file-list 'dired-virtual-vanilla)

    (defun dired-view-corresponding-pdf ()
      "View the current file in another window (possibly newly created)."
      (interactive)
      (if (not (window-parent))
          (split-window))                                   ; create a new window if necessary
      (let ((file (replace-regexp-in-string "\\.txt$" ".pdf" (dired-get-file-for-visit)))
            (dbuffer (current-buffer)))
        (other-window 1)                                          ; switch to the other window
        (unless (equal dbuffer (current-buffer))                 ; don't kill the dired buffer
          (if (or view-mode (equal major-mode 'dired-mode))   ; only if in view- or dired-mode
              (kill-buffer)))                                                    ; ... kill it
        (let ((filebuffer (get-file-buffer file)))
          (if filebuffer                              ; does a buffer already look at the file
              (switch-to-buffer filebuffer)                                    ; simply switch 
            (view-file file))                                                    ; ... view it
          (other-window -1))))

    
    '(add-hook 'dired-mode-hook
              (lambda()
                (define-key dired-mode-map (kbd "P")   'dired-view-corresponding-pdf)))
    
    (add-hook 'dired-load-hook
              (lambda ()
                (load "dired-x")))
    ;; adapted from http://stackoverflow.com/questions/18121808/emacs-ediff-marked-files-in-different-dired-buffers
    (defun mkm/ediff-marked-pair ()
      "Run ediff-files on a pair of files marked in dired buffer"
      (interactive)
      (let* ((marked-files (dired-get-marked-files nil nil))
             (other-win (get-window-with-predicate
                         (lambda (window)
                           (with-current-buffer (window-buffer window)
                             (and (not (eq window (selected-window)))
                                  (eq major-mode 'dired-mode))))))
             (other-marked-files (and other-win
                                      (with-current-buffer (window-buffer other-win)
                                        (dired-get-marked-files nil)))))
        (cond ((= (length marked-files) 2)
               (ediff-files (nth 0 marked-files)
                            (nth 1 marked-files)))
              ((and (= (length marked-files) 1)
                    (= (length other-marked-files) 1))
               (ediff-files (nth 0 marked-files)
                            (nth 0 other-marked-files)))
              ((= (length marked-files) 1)
               (let ((single-file (nth 0 marked-files)))
                 (ediff-files single-file
                              (read-file-name
                               (format "Diff %s with: " single-file)
                               nil (m (if (string= single-file (dired-get-filename))
                                          nil
                                        (dired-get-filename))) t))))
              (t (error "Mark no more than two files")))))
    (bind-key "=" 'mkm/ediff-marked-pair dired-mode-map)))


;;; *** dired-x
(use-package dired-x
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window))
  )

;;; *** dired-narrow
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;;; *** window.el customizations
(progn                                  ;window.el customizations

  ;; get window configurations to ignore point
  (progn
    (defun pointless-window-configuration-to-register (register &optional _arg)
      "Store the window configuration of the selected frame in register REGISTER.
Use \\[jump-to-register] to restore the configuration.
Argument is a character, naming the register.

Interactively, reads the register using `register-read-with-preview'."
      (interactive (list (register-read-with-preview
                          "Window configuration to register: ")
                         current-prefix-arg))
      ;; current-window-configuration does not include the value
      ;; of point in the current buffer, so record that separately.
      (set-register register (cons 'pointless (window-state-get))))

    (defun pointless-describe-register-1 (orig-func register &optional verbose)
      (let ((val (get-register register)))
        (if (and (consp val) (eq (car val) 'pointless))
            (progn
              (princ "Register ")
              (princ (single-key-description register))
              (princ " contains a pointless window configuration."))
          (funcall orig-func register verbose))))
    
    (defun pointless-jump-to-register (orig-func &rest args)
      (let ((val (get-register (car args))))
        (if (and (consp val) (eq (car val) 'pointless))
            (cl-letf
                (((symbol-function 'set-window-point) 'ignore) ;; make these no-ops.
                 ((symbol-function 'set-window-start) 'ignore))
              (window-state-put (cdr val) (frame-root-window)))
          (apply orig-func args))))
    
    (advice-add 'jump-to-register :around #'pointless-jump-to-register)
    (advice-add 'describe-register-1 :around #'pointless-describe-register-1)
    (bind-key "C-x r w" 'pointless-window-configuration-to-register))
  
  (bind-key* "M-o" 'other-window)
  (bind-key "C-x 6" 'toggle-window-split)
  (defun other-window-1 (&optional size)
    (when (called-interactively-p 'any)
      (other-window 1)
      ;; switch the new window to the next thing in the buffer stack
      (let ((next-buffer
             (cl-find-if-not
              (lambda (buf)
                (or (get-buffer-window buf) ; already visible?
                    (= ? (elt (buffer-name buf) 0)))) ; internal?
              (buffer-list))))
        (when next-buffer
          (switch-to-buffer next-buffer)))))
  
  (advice-add 'split-window-below :after #'other-window-1 )
  (advice-add 'split-window-right :after #'other-window-1 )
  (setq scroll-error-top-bottom t)
  (setq scroll-preserve-screen-position t)
  (bind-key "C-S-v" 'scroll-up-line)
  (bind-key "M-V" 'scroll-down-line)
  (bind-key "C-=" 'enlarge-window)

  (bind-key "S-M-s-<left>" 'shrink-window-horizontally)
  (bind-key "S-M-s-<right>" 'enlarge-window-horizontally)
  (bind-key "S-M-s-<down>" 'shrink-window)
  (bind-key "S-M-s-<up>" 'enlarge-window)

  ;; horizontal scrolling with mouse
  (setq hscroll-step 10)
  (global-set-key [wheel-right] (lambda () (interactive) (scroll-left hscroll-step t)))
  (global-set-key [wheel-left] (lambda () (interactive) (scroll-right hscroll-step t)))

  ;; from http://whattheemacsd.com/buffer-defuns.el-03.html
  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))

  ;; Toggle window dedication
  (defun toggle-window-dedicated ()
    "Toggle whether the current active window is dedicated or not"
    (interactive)
    (message
     (if (let (window (get-buffer-window (current-buffer)))
           (set-window-dedicated-p window
                                   (not (window-dedicated-p window))))
         "Window '%s' is dedicated"
       "Window '%s' is normal")
     (current-buffer)))

;;; *** savehist
  (use-package savehist
    :init
    (progn
      (savehist-mode 1)
      (setq savehist-ignored-variables '(hist)) ;;not sure what hist is, but it's not a list so autosave croaks on it
      (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring)))))

;;; *** jka-cmpr-hook
(use-package jka-cmpr-hook
  :config
  (progn
    (add-to-list 'jka-compr-compression-info-list ["\\.xrif\\'" "compressing" "gzip"
                                                   ("-c" "-q")
                                                   "uncompressing" "gzip"
                                                   ("-c" "-q" "-d")
                                                   t nil "\213"] )

    (add-to-list 'jka-compr-mode-alist-additions '("\\.xrif\\'" . nxml-mode))
    (jka-compr-update)))

;;; *** buffer-flip
(use-package buffer-flip
  :chords (("u8" . buffer-flip))
  :bind  (:map buffer-flip-map
               ( "8" .   buffer-flip-forward) 
               ( "*" .   buffer-flip-backward) 
               ( "C-g" . buffer-flip-abort))
  :config
  (progn
    (setq buffer-flip-skip-patterns
          '("^\\*helm\\b"
            "^\\*swiper\\*$"))))

;;; *** ns-win (drag n drop)
(use-package ns-win
  :config
  (global-set-key [M-drag-n-drop] 'ns-drag-n-drop))

;;; *** archive
(use-package arc-mode
  :config
  (progn

    (defun archive-extract-wrapper (orig-func &rest args)
      (let ((archive-remote (or archive-remote (string-match "^http" (buffer-file-name))))
            (default-directory "/tmp"))
        (apply orig-func args)))
    (advice-add 'archive-extract :around 'archive-extract-wrapper)
    
    ))

;;; ** org
(use-package org
  ;; special upgrade instructions for org.  Exit emacs, then delete the elpa dir for the old one, then run emacs -q, M-x package-reinstall RET org
  :commands org-mode
  :mode ("\\.org$" . org-mode)
  :bind*
  (( "C-c l" . org-store-link)
   ( "C-c c" . org-capture)
   ( "C-c a" . org-agenda)
   ( "C-c C-x C-i" . org-clock-in)
   ( "C-c C-x C-o" . org-clock-out)
   ( "C-c C-x C-j" . org-clock-goto)
                                        ;( "s-r" . helm-org-in-buffer-headings)
   ;; these come from mac shortcut keys (automator services)
   ;;( "s-O" . org-capture)
   ( "<C-f9>" . org-capture-mail)
   ( "<C-f10>" . org-capture-chrome)
   :map
   org-mode-map
   ( "C-M-I" . org-table-project) 
   )
  :ensure t
  :config
  (progn

    ;;
    ;; export inline images
    ;;

    (defun org-html--svg-image (source attributes info)
      "Return \"object\" embedding svg file SOURCE with given ATTRIBUTES.
INFO is a plist used as a communication channel.

The special attribute \"fallback\" can be used to specify a
fallback image file to use if the object embedding is not
supported.  CSS class \"org-svg\" is assigned as the class of the
object unless a different class is specified with an attribute."
      (let ((fallback (plist-get attributes :fallback))
            (attrs (org-html--make-attribute-string
                    (org-combine-plists
                     ;; Remove fallback attribute, which is not meant to
                     ;; appear directly in the attributes string, and
                     ;; provide a default class if none is set.
                     '(:class "org-svg") attributes '(:fallback nil)))))
        (format "%s"
                (replace-regexp-in-string "^.*\\(\n.*\\)*<svg " "<svg "
                                          (org-file-contents source)))))
    
    (defun org-html--format-image (source attributes info)
      "Return \"img\" tag with given SOURCE and ATTRIBUTES.
SOURCE is a string specifying the location of the image.
ATTRIBUTES is a plist, as returned by
`org-export-read-attribute'.  INFO is a plist used as
a communication channel."
      (if (string= "svg" (file-name-extension source))
          (org-html--svg-image source attributes info)
        (org-html-close-tag
         "img"
         (org-html--make-attribute-string
          (org-combine-plists
           (list :src (base64-src source) :foo "bar"
                 :alt (if (string-match-p "^ltxpng/" source)
                          (org-html-encode-plain-text
                           (org-find-text-property-in-string 'org-latex-src source))
                        (file-name-nondirectory source)))
           attributes))
         info)))

    (defun base64-src (file)
      (concat "data:image/" (file-name-extension file) ";base64,"
              (with-temp-buffer
                (insert-file file)
                (base64-encode-region (point-min) (point-max) t)
                (buffer-substring (point-min) (point-max)))))
    

    ;;    (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
    ;;    (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
    
    (defun org-table-project (increase)
      (interactive (list (let ((val (string-to-number
                                     (org-table-get-field (org-table-current-column))))
                               (prev-val (string-to-number
                                          (save-excursion (org-table-get-field (- (org-table-current-column) 1))))))
                           (if (and (> val 0) (> prev-val 0))
                               (number-to-string (- val prev-val))
                             (read-from-minibuffer
                              "Increase: "
                              )
                             ))
                         ))
      (let ((n (string-to-number (org-table-get-field))))
        (org-table-next-field)
        (org-table-blank-field)
        (insert (number-to-string (+ (string-to-number increase) n)))
        (org-table-align)))
    
    ;; org-gnuplot fixup.  Resetting to aqua terminal at the beginning of each script, since writing to a file in #+PLOT doesn't reset the terminal back to aqua automatically
    (defun gnuplot-script-around (orig-func &rest args)
      (concat "
#init.el -- resetting term each time 
set term aqua
set output
"
              (apply orig-func args)
              ""
              ))
    (advice-add 'org-plot/gnuplot-script :around #'gnuplot-script-around)
    
    (key-chord-define org-mode-map "uu" 'calc-embedded-refresh-everything)
    (setq org-ellipsis "…")
    (key-chord-define org-mode-map "**" 'org-ctrl-c-star)
    (add-hook 'org-mode-hook
              (lambda ()
                (push '("->" . ?→) prettify-symbols-alist)
                (prettify-symbols-mode 1)))
    
    (add-hook 'org-mode-hook
              (lambda ()
                (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
                (set-face-attribute 'org-code nil :inherit 'fixed-pitch)))

    (add-hook 'org-mode-hook 'toggle-truncate-lines )
    (add-hook 'org-mode-hook 'toggle-word-wrap )
    ;; fixup links 
    (defun org-html-link-merc (orig-func &rest args)
      (replace-regexp-in-string "file:///Users/rblack/code/hg/\\([^/]+\\)/" "http://merc.footnote.com/\\1/file/tip/"
                                (apply orig-func args)))

    (advice-add 'org-export-file-uri :around 'org-html-link-merc)
    
    (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
    (defun org-capture-mail ()
      (interactive)
      (org-capture nil "m"))
    (defun org-capture-chrome ()
      (interactive)
      (org-capture nil "c"))

    (setq org-export-babel-evaluate nil) ;; don't evaluate code on export
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (dot .t)
       (shell . t)
       (awk .t)
       (python . t)
       (R . t)
       (ruby . t)
       (ditaa . t)
       (dot . t)
       (sql . t)
       (octave . t)
       (sqlite . t)
       (perl . t)
       (java . t)
       (js . t)
       (gnuplot .t)
       (restclient . t)))

    (patch-function
     'org-babel-execute:sql
     "(with-temp-file in-file"
     "(let ((command-override (cdr (assq :command params))))
      (when command-override
        (setq command
              (format \"%s < %s > %s\" command-override
                      (org-babel-process-file-name in-file)
                      (org-babel-process-file-name out-file)))))
      (with-temp-file in-file")

    ;; from https://gist.github.com/wallyqs/724e61e9d070b1d4e95b
    '(patch-function 'org-babel-eval
                     "(progn
	    (with-current-buffer err-buff
	      (org-babel-eval-error-notify exit-code (buffer-string)))
	    (save-excursion
	      (when (get-buffer org-babel-error-buffer-name)
		(with-current-buffer org-babel-error-buffer-name
		  (unless (derived-mode-p 'compilation-mode)
		    (compilation-mode))
		  ;; Compilation-mode enforces read-only, but Babel expects the buffer modifiable.
		  (setq buffer-read-only nil))))
	    nil)"
                     ;; W: force to return output even if there was an error
	                 '(progn
	                    (concat (buffer-string)
		                        (with-current-buffer err-buff
		                          ;; (org-babel-eval-error-notify exit-code (buffer-string)
		                          (buffer-string)
		                          )))
                     )

    ;; putting it back to normal
    '(patch-function 'org-babel-eval
                   
                    )

    (defun redisplay-images (result &optional result-params info hash indent lang)
      (org-redisplay-inline-images))

    (advice-add 'org-babel-insert-result :after 'redisplay-images)

    (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

    ;; agenda setup

    (setq

     org-agenda-files
     `( "~/org/ia-tasks.org" "~/org/np-tasks.org" "~/org/fg-tasks.org" "~/org/flagged.org" "~/Dropbox/orgfiles/gcal.org" "~/Dropbox/orgfiles/mollicoa.org" "~/Dropbox/orgfiles/sar.org" ,exchange-cal-file)
     org-agenda-include-diary nil
     org-agenda-prefix-format
     (quote
      ((agenda . " %i %-12:c%?-12t% s%-6 (condition-case nil (let ((sum (org-clock-sum-current-item))) (if (> sum 0) (concat (org-duration-from-minutes sum ) \" /\") \"\"))  (error \"\")) %-5 e")
       (timeline . "  % s")
       (todo . " %i %-12:c")
       (tags . " %i %-12:c")
       (search . " %i %-12:c")))
     org-agenda-restore-windows-after-quit nil
     org-agenda-skip-scheduled-if-done t
     org-agenda-start-on-weekday nil
     org-agenda-window-setup (quote current-window)


     org-agenda-custom-commands
     '(
       ("p" "Packing list"
        (
         (tags-todo "russ"
                    ((org-agenda-prefix-format "[ ]")
                     (org-agenda-sorting-strategy '(tag-up priority-down))
                     (org-agenda-todo-keyword-format "")
                     (org-agenda-overriding-header "Russell Packing List\n------------------\n")
                     ))
         (tags-todo "molli"
                    ((org-agenda-prefix-format "[ ]")
                     (org-agenda-sorting-strategy '(tag-up priority-down))
                     (org-agenda-todo-keyword-format "")
                     (org-agenda-overriding-header "Molli Packing List\n------------------\n")
                     ))
         (tags-todo "tyler"
                    ((org-agenda-prefix-format "[ ]")
                     (org-agenda-sorting-strategy '(tag-up priority-down))
                     (org-agenda-todo-keyword-format "")
                     (org-agenda-overriding-header "Tyler Packing List\n------------------\n")
                     ))
         (tags-todo "casey"
                    ((org-agenda-prefix-format "[ ]")
                     (org-agenda-sorting-strategy '(tag-up priority-down))
                     (org-agenda-todo-keyword-format "")
                     (org-agenda-overriding-header "Casey Packing List\n------------------\n")
                     ))
         (tags-todo "aly"
                    ((org-agenda-prefix-format "[ ]")
                     (org-agenda-sorting-strategy '(tag-up priority-down))
                     (org-agenda-todo-keyword-format "")
                     (org-agenda-overriding-header "Alyson Packing List\n------------------\n")
                     ))
         (tags-todo "lydia"
                    ((org-agenda-prefix-format "[ ]")
                     (org-agenda-sorting-strategy '(tag-up priority-down))
                     (org-agenda-todo-keyword-format "")
                     (org-agenda-overriding-header "Lydia Packing List\n------------------\n")
                     ))

         )
        ((org-agenda-with-colors nil)
         (org-agenda-compact-blocks t)
         (org-agenda-remove-tags t)
         (ps-number-of-columns 2)
         (ps-landscape-mode t))
        ("~/agenda.ps"))
       ;;other commands go here
       ("u" "Agenda and unscheduled TODOs"
        ((agenda "")
         (tags-todo "-exclude"
                    ((org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'deadline 'scheduled 'timestamp))
                     (org-agenda-overriding-header "Unscheduled Todos")))))))

    ;; Set to the location of your Org files on your local system
    (setq org-directory "~/org")
    ;; Set to the name of the file where new notes will be stored
    (setq org-mobile-inbox-for-pull "~/org/flagged.org")
    ;; Set to <your Dropbox root directory>/MobileOrg.
    (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
    (defun org-table-sum-column ()
      (interactive)
      (insert ":=vsum(@1..@-1);N")
      )
    (bind-key (kbd "C-c g") 'grab-mac-link org-mode-map)
    (bind-key (kbd "C-c s") 'org-table-sum-column org-mode-map)
    (bind-key (kbd "C-s") 'isearch-forward org-mode-map)
    (bind-key (kbd "C-r") 'isearch-backward org-mode-map)
    ;;(add-hook 'org-mode-hook 'visual-line-mode)
    ;;(require 'org-mac-iCal)
    ;;(run-with-timer 0 (* 8 60 60) 'org-mac-iCal) ; update from calendars every 8 hours

    (defun reward-for-done-task (task-plist &optional invert)
      "Apply the currency deltas for the task at point. This function is called
when a task is marked 'done'.
If 'invert' is non-nil, the deltas are all multiplied by -1 before being
applied."
      ;; We must check that the new state is a 'done' state, because when repeating
      ;; items are marked done, they immediately revert to 'todo', creating a total
      ;; of 2 state changes. We must only apply deltas ONCE for repeating items.
      (let ((to-state (plist-get task-plist :to))
            (from-state (plist-get task-plist :from)))
        (when (and (member from-state (cons 'todo org-not-done-keywords))
                   (member to-state (cons 'done org-done-keywords)))
          (play-sound '(sound :file "~/Downloads/Cash Register Cha Ching-SoundBible.com-184076484.mp3"))
          )))


    (add-hook 'org-trigger-hook 'reward-for-done-task)

    (setq org-capture-templates
          '(
            ("a" "Appointment" entry (file  "~/Dropbox/orgfiles/gcal.org" )
             "* %?\n%^T\n\n")
            ("n" "Newspapers TODO" entry (file+olp+datetree "~/org/np-tasks.org")
             "* TODO %? %i
")
            ("f" "Find-A-Grave TODO" entry (file+olp+datetree "~/org/fg-tasks.org")
             "* TODO %? %i
")
            ("b" "Business Tasks TODO" entry (file+olp+datetree "~/org/business-tasks.org")
             "* TODO %? %i
")
            ("j" "Business Tasks TODO" entry (file+olp+datetree "~/org/business-journal.org")
             "* Journal Entry%?
")
            ("i" "iArchives TODO" entry (file+olp+datetree "~/org/ia-tasks.org")
             "* TODO %? %i
")
            ("M" "Work Meeting" entry (file+olp+datetree "~/org/meetings.org")
             "* %?
")

            ("l" "Work TODO with link" entry (file+headline "~/org/life.org" "Work" "Tasks")
             "* TODO %?%a
:PROPERTIES:
:CREATED: %U
:END:
%i")
            ("r" "Rental Business" entry (file+olp "~/org/life.org" "Rental Business" "Tasks")
             "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:
%i")
            ("h" "Home TODO" entry (file+olp "~/org/life.org" "Home" "Tasks")
             "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:
%i
")
            ("s" "SAR TODO" entry (file+olp "~/org/life.org" "SAR" "Tasks")
             "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:
%i")
;;             ("t" "General TODO" entry (file+headline "~/org/life.org" "Tasks")
;;              "* TODO %?
;; :PROPERTIES:
;; :CREATED: %U
;; :END:
;; %i")
            ("p" "Password" entry (file org-passwords-file)
             "* %^{Title|%(org-passwords-chrome-title)}
:PROPERTIES:
:URL: %^{URL|%(org-passwords-chrome-url)}
:PASSWORD: %^{PASSWORD|%(org-passwords-generate-password-with-symbols nil 16)}
:CREATED: %U
:END:
%^{USERNAME}p")))
    (use-package org-crypt
      :config
      (progn
        (org-crypt-use-before-save-magic)
        (setq org-tags-exclude-from-inheritance (quote ("crypt")))
        (setq org-crypt-key nil)))

                                        ;(use-package ox-reveal)

    (use-package org-inlinetask
      :config
      (progn
        (defun org-html-format-inlinetask-default-function
            (todo todo-type priority text tags contents info)
          "Default format function for a inlinetasks.
See `org-html-format-inlinetask-function' for details."
          (format "<div class=\"inlinetask\">\n<b>%s</b>%s\n%s</div>"
                  (org-html-format-headline-default-function
                   todo todo-type priority text tags info)
                  (org-html-close-tag "br" nil info)
                  (or contents ""))) ;; this patch is here because when contents is nil it was printing nil
        )
      )
    ;; make sure org-captures end with a newline to keep stuff from getting screwed up
    (defun org-capture-ensure-newline ()
      (when (not (eq 10 (char-before (point-max)))) (goto-char (point-max)) (insert "\n")))

    (add-hook 'org-capture-prepare-finalize-hook 'org-capture-ensure-newline)
    
    ))

(use-package ox-reveal
  :ensure t
  :config
  (setq org-reveal-root "file:///Users/rblack/lib/reveal.js")
  )

(use-package hexrgb
  :ensure t)

(use-package grab-mac-link
  :ensure t
  :commands grab-mac-link)

'(use-package org-alert
  :ensure t
  :config
  (org-alert-enable))

'(use-package org-wild-notifier
  :ensure t
  :config
  (org-wild-notifier-mode))

(use-package ox-twbs
  :ensure t
  )

(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list
        '("⚫" "○" "•" "◦" "◎" "◉" ))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package calendar
  :config
  (defun calendar-insert-date ()
    "Capture the date at point, exit the Calendar, insert the date."
    (interactive)
    (seq-let (month day year) (save-match-data (calendar-cursor-to-date))
      (calendar-exit)
      (insert (format "%d-%02d-%02d" year month day))))

  (define-key calendar-mode-map (kbd "RET") 'calendar-insert-date)
)

(use-package calfw
  :ensure t
  :config
  (require 'calfw) 
  (setq cfw:org-overwrite-default-keybinding t)

  (defun mycalendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "Green")  ; orgmode source
      ;;(cfw:ical-create-source "gcal" "https://somecalnedaraddress" "IndianRed") ; devorah calender
      ;;(cfw:ical-create-source "gcal" "https://anothercalendaraddress" "IndianRed") ; google calendar ICS
      ))) 
  (setq cfw:org-overwrite-default-keybinding t))

(use-package calfw-org
  :ensure t)

(use-package calfw-ical
  :ensure t)



;;; ** ews-fetch-calendar
(defun exchange-sync ()
  "Fetch the exchange calendar into an org file."
  (interactive)
  (let ((display-buffer-alist
         
         (list (cons "\\*Async Shell Command\\*.*" 
                     (cons #'display-buffer-no-window nil))))) ;; suppress async shell command window
   
    (async-shell-command (concat  "~/lib/ews-orgmode/ews-fetch-calendar.py > " exchange-cal-file ".tmp && mv " exchange-cal-file ".tmp " exchange-cal-file))
    (run-at-time 3600 nil 'exchange-sync) ;; reschedule next one
    ))

;(setq exchange-sync-calendar (run-at-time 60 nil 'exchange-sync)) ;; start after 1 minute, and refresh once an hour.
;(cancel-timer exchange-sync-calendar)

;;; ** org-passwords
(use-package org-passwords
  :bind ( "C-*" . org-passwords) ; this is the binding that gets invoked by the applescript.
  :commands org-passwords
  :demand t
  :config
  (progn
    (setq org-passwords-random-words-dictionary "/usr/share/dict/words")
    (key-chord-define org-passwords-mode-map "PP" 'org-passwords-copy-password)
    (key-chord-define org-passwords-mode-map "UU" 'org-passwords-copy-username)))

;;; ** midnight
(use-package midnight
  :defer 60
  :config
  (setq midnight-hook '(clean-buffer-list
                        ;; org-mobile-push
                        org-gcal-fetch
                        xkcd
                        ))
  (midnight-delay-set 'midnight-delay 43200))

;;; ** package
(use-package package
  ;; :idle (progn
  ;;         ;; this block deletes obsolte packages
  ;;         (package-list-packages t)
  ;;         (package-menu-mark-obsolete-for-deletion)
  ;;         (package-menu-execute t)
  ;;         (kill-this-buffer))
  )
;;; ** doc-view
(use-package doc-view
  :init
  (add-hook 'doc-view-mode-hook
            (lambda ()
              (define-key doc-view-mode-map (kbd "r") 'doc-view-rotate-current-page) ;; we don't want this to overshadow our multi-cursor selection
              ))
  :config
  (defun doc-view-rotate-current-page ()
    ;; from http://stackoverflow.com/questions/2684547/rotate-document-in-emacs-doc-view-mode
    "Rotate the current page by 90 degrees.
Requires ImageMagick installation"
    (interactive)
    (when (eq major-mode 'doc-view-mode)
      ;; we are assuming current doc-view internals about cache-names
      (let ((file-name (expand-file-name (format "page-%d.png" (doc-view-current-page)) (doc-view--current-cache-dir))))
        ;; assume imagemagick is installed and rotate file in-place and redisplay buffer
        (call-process-shell-command "convert" nil nil nil "-rotate" "-90" (concat "\"" file-name "\"") (concat "\"" file-name "\""))
        (clear-image-cache)
        (doc-view-goto-page (doc-view-current-page))))))
;;; ** artist
(use-package artist
  :config
  (define-key artist-mode-map [down-mouse-3] 'artist-mouse-choose-operation))
;;; ** ediff




(use-package ediff
  :defer t
  :config
  (progn

    ;; restores window configuration when ediff quits
    (defun my-ediff-load-hook ()
      (add-hook 'ediff-before-setup-hook
                (lambda ()
                  (setq ediff-saved-window-configuration (current-window-configuration))))
      
      (let ((restore-window-configuration
             (lambda ()
               (set-window-configuration ediff-saved-window-configuration))))
        (add-hook 'ediff-quit-hook restore-window-configuration 'append)
        (add-hook 'ediff-suspend-hook restore-window-configuration 'append)))

    ;;(add-hook 'ediff-load-hook 'my-ediff-load-hook)
    (my-ediff-load-hook)

    (defun ediff-copy-both-to-C ()
      (interactive)
      (let ((n ediff-current-difference)
            (b ediff-control-buffer))
        (ediff-copy-diff n nil 'C nil
                         (concat
                          (ediff-get-region-contents n 'A b)
                          (ediff-get-region-contents n 'B b)))))
    
    (defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
    (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

    ;; Tried to hack in refinements on ancestor.  This ended up being too hard.
    '(defun refine-ancestor-advice (orig-func &optional file-A file-B file-C reg-num)
      (if (and ediff-3way-job (get-buffer-window ediff-ancestor-buffer) (not file-C))
          (let ((tmp-buffer (get-buffer-create ediff-tmp-buffer))
                (ediff-buffer-C ediff-ancestor-buffer)
                file-Ancestor)
            (ediff-wordify
             (ediff-get-diff-posn 'Ancestor 'beg n)
             (ediff-get-diff-posn 'Ancestor 'end n)
             ediff-ancestor-buffer
             tmp-buffer
             ediff-control-buffer)
            (setq file-Ancestor
                  (ediff-make-temp-file tmp-buffer "fineDiffAncestor"))
            (funcall orig-func file-A file-B file-Ancestor reg-num))
        (funcall orig-func file-A file-B file-C reg-num))
      )

    '(advice-add 'ediff-setup-fine-diff-regions :around #'refine-ancestor-advice)
    (advice-remove 'ediff-setup-fine-diff-regions  #'refine-ancestor-advice)

    ))

;;; sqlite-mode
(use-package sqlite-dump
  :config
  (modify-coding-system-alist 'file "\\.sqlite\\'" 'raw-text-unix)
  (add-to-list 'auto-mode-alist '("\\.sqlite\\'" . sqlite-dump))
  (modify-coding-system-alist 'file "\\.sqlite3\\'" 'raw-text-unix)
  (add-to-list 'auto-mode-alist '("\\.sqlite3\\'" . sqlite-dump))
  (modify-coding-system-alist 'file "\\.localstorage\\'" 'raw-text-unix)
  (add-to-list 'auto-mode-alist '("\\.localstorage\\'" . sqlite-dump))
  (modify-coding-system-alist 'file "\\.db\\'" 'raw-text-unix)
  (add-to-list 'auto-mode-alist '("\\.db\\'" . sqlite-dump))
  
  )

;;; ** diff-mode
(use-package diff-mode
  :bind
  (:map
   diff-mode-map
   ("C-c M-w" . diff-copy-+))
  :config
  (progn
    "Lets you copy sections that begin with + (added hunks)"
    (defun diff-copy-+ ()
     (interactive)
     (let (start end)
       (beginning-of-line)
       (unless (looking-at "\\+")
         (user-error "No + here"))
       (while (looking-at "\\+")
         (setq start (point))
         (previous-logical-line))
       (next-logical-line)
       (while (looking-at "\\+")
         (next-logical-line)
         (setq end (point)))
       (kill-new (replace-regexp-in-string
                  "^\\+" ""
                  (buffer-substring-no-properties start end)))
       (message "section copied")
       ))))

;;; ** try-package
(use-package try :ensure t )

;;; ** which-key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

;;; ** mu4e-alert
'(use-package mu4e-alert
  :ensure t
  :config
  (progn (setq mu4e-alert-interesting-mail-query "flag:unread AND maildir:/inbox")
         (mu4e-alert-enable-mode-line-display)
         (patch-function 'mu4e-alert-enable-mode-line-display
                         "'mu4e-msg-changed-hook"
                         '(if (boundp 'mu4e-message-changed-hook)
                              'mu4e-message-changed-hook
                            'mu4e-msg-changed-hook))
         (patch-function 'mu4e-alert-disable-mode-line-display
                         "'mu4e-msg-changed-hook"
                         '(if (boundp 'mu4e-message-changed-hook)
                              'mu4e-message-changed-hook
                            'mu4e-msg-changed-hook))
         ))
;;; ** Company-Specific Packages
;;; *** jenkins-build
(use-package jenkins-build
  :commands (dfb dbb sfb sbb lfb lbb ncb pb))

;;; *** project-search
'(use-package project-search
  :commands (dff dgf dbf sff sbf lff lbf ncf ffphp ffe fbe)
  :bind (("C-f" . dff)
         ("C-b" . dbf)))

;;; *** project-versions
(use-package project-versions
  :commands (dv sv lv))

;;; *** smarty-assignment
(use-package smarty-assignment
  :commands smarty-assignment)

;;; *** mogile-edit
(use-package mogile-edit
  :commands (ocr ocr-email))

;;; *** mark-state-mode
(use-package mark-state-mode
  :config (mark-state-mode))

(use-package rest-doc-mode
  :mode "\\.rest-doc$"
  )

(use-package elm-mode
  :ensure t
  )

(use-package flycheck-elm
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-elm-setup)
  )

(use-package lsp-mode
  :ensure t
  :config (add-hook 'elm-mode-hook #'lsp)
  (setq lsp-prefer-flymake nil)
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :requires lsp-mode flycheck
  :config

  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)

  (add-hook 'lsp-mode-hook 'lsp-ui-mode))


;;; * Misc Functions

(defun scale-image-if-image-mode (&rest args)
  ""
  (if (eq major-mode 'image-mode)
      (image-transform-set-scale
       (expt text-scale-mode-step
		     text-scale-mode-amount))))

(defun image-scale-register-hook ()
  (add-hook 'text-scale-mode-hook 'scale-image-if-image-mode))

(add-hook 'image-mode-hook 'image-scale-register-hook)


;; Fold at indentation
;; from https://stackoverflow.com/questions/1587972/how-to-display-indentation-guides-in-emacs/4459159#4459159
(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line."
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))
(global-set-key [(M C i)] 'aj-toggle-fold)

;; If call-process is called with an invalid value in
;; default-directory, such as when the buffer comes from
;; browse-url-emacs, it will fail.  This ensures that the call can
;; proceed by setting the directory to "" when its' invalid.
(defun ensure-directory-call-process (orig-func &rest args)
  "Ensure that there is a valid directory when `call-process' is called.
ORIG-FUNC is the `call-process’ function, ARGS are the arguments
passed to that function.  If `call-process’ is called with an
invalid value in `default-directory’, such as when the buffer comes
from `browse-url-emacs’, it will fail.  This ensures that the call
can proceed by setting the directory to \"\" when its' invalid."
  (if (file-accessible-directory-p default-directory)
      (apply orig-func args)
    (let ((default-directory ""))
      (apply orig-func args))))

(advice-add 'call-process :around 'ensure-directory-call-process)

(defun display-default-directory ()
  "Display the value of `default-directory’, or variable `buffer-file-name’ with prefix arg."
  (interactive)
  (kill-new (message
             (if current-prefix-arg
                 buffer-file-name
               default-directory))))

(key-chord-define-global "DD" 'display-default-directory)


;use c-h for backspace, rebind help map s-h
;(global-set-key (kbd "s-h") help-map)
;(global-set-key (kbd "C-h") 'delete-backward-char)
;(global-set-key (kbd "M-h") 'backward-kill-word)

(defun my-mm-url-form-encode-xwfu (orig-func &rest args)
  "Fix for a NPE bug in eww/gnus.
ORIG-FUNC and ARGS are standard advice parameters."
  (if (car args)
      (apply orig-func args)
    ""))

(advice-add 'mm-url-form-encode-xwfu :around #'my-mm-url-form-encode-xwfu)

(defun mm-url-form-encode-xwfu (chunk)
  "Escape characters in a string for application/x-www-form-urlencoded.
Blasphemous crap because someone didn't think %20 was good enough for encoding
spaces.  Die Die Die."
  ;; This will get rid of the 'attributes' specified by the file type,
  ;; which are useless for an application/x-www-form-urlencoded form.
  (if (consp chunk)
      (setq chunk (cdr chunk)))

  (mapconcat
   (lambda (char)
     (cond
      ((= char ?  ) "+")
      ((memq char mm-url-unreserved-chars) (char-to-string char))
      (t (upcase (format "%%%02x" char)))))
   (mm-encode-coding-string chunk
                            (if (fboundp 'find-coding-systems-string)
                                (car (find-coding-systems-string chunk))
                              buffer-file-coding-system))
   ""))

(defun get-random-element (list)
  "Return a random element of LIST."
  (if (not (and (list) (listp list)))
      (nth (random (1- (1+ (length list)))) list)
    (error "Argument to get-random-element not a list or the list is empty")))

(defun switch-to-random-buffer ()
  "Switch to a random buffer."
  (switch-to-buffer
   (get-random-element
    (remove-if-not
     (lambda (b)
       (with-current-buffer b
         buffer-file-name))
     (buffer-list)))
   t))

(defun mandelbrot ()
  "Draw the mandelbrot set."
  (interactive)
  (pop-to-buffer (get-buffer-create "*mandelbrot*"))
  (when (not (boundp 'mandelbrot-xoffset)) (setq mandelbrot-xoffset 0))
  (when (not (boundp 'mandelbrot-yoffset)) (setq mandelbrot-yoffset 0))
  (when (not (boundp 'mandelbrot-zoom)) (setq mandelbrot-zoom 1.5))
  (let ((w 400) (h 300) (d 64))
    (fundamental-mode) (erase-buffer)
    (set-buffer-multibyte nil)
    (insert (format "P6\n%d %d\n255\n" w h))
    (dotimes (y h)
      (dotimes (x w)
        (let* ((cx (+ mandelbrot-xoffset (* mandelbrot-zoom (/ (- x (/ w 1.45)) w 0.45))))
               (cy (+ mandelbrot-yoffset (* mandelbrot-zoom (/ (- y (/ h 2.0)) h 0.5))))
               (zr 0) (zi 0)
               (v (dotimes (i d d)
                    (if (> (+ (* zr zr) (* zi zi)) 4) (return i)
                      (psetq zr (+ (* zr zr) (- (* zi zi)) cx)
                             zi (+ (* (* zr zi) 2) cy))))))
          (insert-char (floor (* 256 (/ v 1.0 d))) 3))))
    (image-mode)
    (use-local-map (copy-keymap image-mode-map))
    (defmacro mandelbrot-key (key &rest actions)
      `(local-set-key (kbd ,key)
                      (lambda ()
                        (interactive)
                        ,@actions 
                        (mandelbrot))))
    (mandelbrot-key "r"
                    (setq mandelbrot-xoffset 0)
                    (setq mandelbrot-yoffset 0)
                    (setq mandelbrot-zoom 1.5))
    (mandelbrot-key "<left>" (setq mandelbrot-xoffset (+ mandelbrot-xoffset (* -.5 mandelbrot-zoom))))
    (mandelbrot-key "<right>" (setq mandelbrot-xoffset (+ mandelbrot-xoffset (* .5 mandelbrot-zoom))))
    (mandelbrot-key "<up>" (setq mandelbrot-yoffset (+ mandelbrot-yoffset (* -.5 mandelbrot-zoom))))
    (mandelbrot-key "<down>" (setq mandelbrot-yoffset (+ mandelbrot-yoffset (* .5 mandelbrot-zoom))))
    (mandelbrot-key "=" (setq mandelbrot-zoom (* mandelbrot-zoom .666)))
    ))

(defun random-sort-lines (beg end)
  "Sort lines in region between BEG and END randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

(defun remove-^M ()
  "Remove ^M at end of line in the whole buffer.
This is done in ahg-diff-mode so that extra ^M's are not added when applying
hunks with \\<diff-mode-map> \\[diff-apply-hunk].  Plus it is a lot more readable
without the ^M's getting in the way."
  (interactive)
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat (char-to-string 13) "$")
                                (point-max) t)
        (replace-match "")))))

(defmacro measure-time (&rest body)
  "Measure and return the running time of BODY."
  (declare (indent defun))
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))

(defun func-region (start end func)
  "Run FUNC over the region between START and END in current buffer."
  (save-excursion
    (when (not (region-active-p))
        ;; whole buffer
        (setq start (point-min))
        (setq end (point-max)))
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun urlencode-region (start end)
  "Url encode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun urldecode-region (start end)
  "Url decode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))

(defun hex-decode-region (start end)
  "Un-hex-encode the region between START and END in current buffer.  Expects straight hex, no percents."
  (interactive "r")
  (func-region start end #'decode-hex-string))

(defun flate-decode-region (start end)
  "Use deflate raw zlib encoding (flate encoding)"
  (interactive "r")
  (shell-command-on-region
   start end
   ;; "cat ./flatefile|(printf \"\\x1f\\x8b\\x08\\0\\0\\0\\0\\0\";cat)|gunzip"
   "perl -MCompress::Zlib -e'undef$/;print uncompress<>||die(\"Decompression error\\n\")'"
   nil t
   )
  )

(defun flate-decode-string ()) 

(defun decode-hex-string (hex-string)
  "Hex decode HEX-STRING."
  (let ((res nil))
    (dotimes (i (/ (length hex-string) 2) (apply #'concat (reverse res)))
      (let ((hex-byte (substring hex-string (* 2 i) (* 2 (+ i 1)))))
        (push (format "%c" (string-to-number hex-byte 16)) res)))))

(defun chunked-decode ()
  "Decode chunked encoding.
Place cursor at beginning of content body (after http headers)."
  (interactive)
  (loop with chars with start with end
        ;; parse hex chunk length
        do (insert "#x")
        (left-char 2)
        (setq start (point))
        (setq chars (read (current-buffer)))
        (setq end (point))
        (kill-region start end)
        ;; eat EOL
        (when (eq 13 (char-after (point)))
          (delete-char 1)) ;; delete 0x0d CR
        (delete-char 1) ;; delete 0x0a LF
        (right-char chars) ;; skip chunk data
        ;; eat EOL
        (when (eq 13 (char-after (point)))
            (delete-char 1))
        (delete-char 1)
        until (eq chars 0) ;; this means we're at the end
        ))

(defun killdash9/comment-dwim (orig-func &rest args)
  (when (not (region-active-p))
    (beginning-of-line)
    (set-mark (line-end-position))
    (activate-mark))
  (apply orig-func args)
  ;;(c-indent-line-or-region)
  (indent-for-tab-command)
  )

(advice-add 'comment-dwim :around #'killdash9/comment-dwim)

;; Allow binary plist files to be edited
(add-to-list 'jka-compr-compression-info-list
             ["\\.plist$"
              "converting text XML to binary plist"
              "plutil"
              ("-convert" "binary1" "-o" "-" "-")
              "converting binary plist to text XML"
              "plutil"
              ("-convert" "xml1" "-o" "-" "-")
              nil nil "bplist"])

;;It is necessary to perform an update!
(jka-compr-update)

(defun save-and-refresh-chrome()
  "Save the current file and reload Chrome."
  (interactive)
  (when buffer-file-name
    (save-buffer))
  (do-applescript "tell application \"Google Chrome\" to reload front window's active tab"))

(key-chord-define-global "RR" 'save-and-refresh-chrome)

(require 'cl-lib)

(defvar punctuation-marks '()
  "List of Punctuation Marks that you want to count.")

(defun count-raw-word-list (raw-word-list)
  (cl-loop with result = nil
           for elt in raw-word-list
           do (cl-incf (cdr (or (assoc elt result)
                                (first (push (cons elt 0) result)))))
           finally return (sort result
                                (lambda (a b) (string< (car a) (car b))))))

(defun word-frequency-histogram ()
  (interactive)
  (let* ((words (split-string
                 (downcase (buffer-string))
                 (format "[ %s\f\t\n\r\v]+"
                         (mapconcat #'identity punctuation-marks ""))
                 t))
         (punctuation-marks (cl-remove-if-not
                             (lambda (elt) (member elt punctuation-marks))
                             (split-string (buffer-string) "" t )))
         (raw-word-list (append punctuation-marks words))
         (word-list (count-raw-word-list raw-word-list)))
    (with-current-buffer (get-buffer-create "*word-statistics*")
      (erase-buffer)
      (insert "| word | occurences |
               |-----------+------------|\n")

      (dolist (elt word-list)
        (insert (format "| '%s' | %d |\n" (car elt) (cdr elt))))

      (org-mode)
      (indent-region (point-min) (point-max))
      (beginning-of-buffer)
      (next-line 2)
      (org-cycle)
      (org-cycle)
      (org-table-sort-lines nil ?N)))
  (pop-to-buffer "*word-statistics*"))

(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))

(add-hook 'nxml-mode-hook 'hs-minor-mode)

;; optional key bindings, easier than hs defaults
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)


(defun my-sql-copy-column ()
  "Copy a mysql column to the kill ring."
  (interactive)
  (save-excursion
    (re-search-backward "^\\+-+\\+\\(\n|\\|-\\)")
;;    (re-search-backward "^\\+-+\\+\n|")
    (forward-line)
    (forward-char 2)
    (let ((start (point))
          (col (current-column)))
      (re-search-forward "^\+--")
      (forward-line -1)
      (beginning-of-line)
      (forward-char col)
      (re-search-forward " |\\( \\|\n\\)")
      (backward-char 3)
      (copy-rectangle-as-kill start (point))
      (with-temp-buffer
        (yank-rectangle)
        (delete-trailing-whitespace)
        (kill-new (buffer-string)))
      (message "Column copied"))))

(defun sql-format-region (start end)
  "Format the region from START to END using simple rules."
  (interactive "r")
  (func-region start end #'sql-format))

(defun sql-format (sql)
  "Format the SQL."
  (replace-regexp-in-string "\\b\\(where\\|and\\|or\\|join\\)\\b" "\n\\1" sql)
  )

(defun paste-copied-column-with-comma-separated-quotes ()
  "Paste copied column with comma-separated quotes."
  (interactive)
  (save-excursion
    (let ((start (point-marker)))
      (yank)
      (let ((end (point-marker)))
        (replace-regexp "$" "'," nil start end)
        (replace-regexp "^" "'" nil start end)
        (join-region start end)
        (end-of-line)
        (while (looking-back " \\|,")
          (delete-backward-char 1))
        ))
    
    ))

(define-key key-translation-map (kbd "<S-mouse-1>") (kbd "<mouse-2>"))

;;; * Customization Variables
(load custom-file)

(set-face-attribute 'default nil :height 140)

;;; * Load Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;(custom-theme-load-path (quote (custom-theme-directory t "~/.emacs.d/themes")) t)
;;(load-theme 'atari t)
;;(load-theme 'default-black t)
(use-package base16-theme
  :ensure t
  :config
  ;; (load-theme 'base16-greenscreen t)
  ;; try next, wombat
  ;;(load-theme 'base16-tube t)
  (load-theme 'base16-spacemacs t)
  )

;;; * Local Variables
;; Local Variables:
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: ";;; "
;; End:


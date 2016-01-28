;;; * Custom File
(setq custom-file "~/.emacs.d/custom.el")
;;; * Load Path
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
;; add all subdirectories of site-lisp to load path as well
(mapc
 (lambda (path) (add-to-list 'load-path path))
 (split-string (shell-command-to-string "find ~/.emacs.d/site-lisp -type d -d 1") "[\r\n]+"))

;;; * Package Framework
(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")))

(package-initialize)
(setq package-enable-at-startup nil) ;; keep it from re-loading the packages after the init file has run

(when (not (require 'use-package nil t))
    (package-refresh-contents)
    (package-install 'use-package))

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
        (when (equal (cdr (assoc ?L (funcall battery-status-function))) "AC")
          ad-do-it))
      )
    (use-package zone-matrix
      :config
      (progn
        (setq zmx-unicode-mode t)
        (setq zone-programs (vconcat zone-programs [zone-matrix]))
        ))
    (setq zone-programs (remove-if (lambda (x) (member x '(zone-pgm-jitter zone-pgm-dissolve))) zone-programs))
    (zone-when-idle 300)))

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

;;; *** hi-lock
(use-package hi-lock
  :config
  (progn
    (defun hi-lock-set-pattern-around (orig-func &rest args)
      (letf* (((symbol-function 'orig-make-overlay) (symbol-function 'make-overlay))
              ((symbol-function 'make-overlay)
               (lambda (&rest args)
                 (message "here we are")
                 (apply orig-make-overlay args))))
        (apply orig-func args)))
    (advice-remove 'hi-lock-set-pattern 'hi-lock-set-pattern-around)
    '(advice-add 'hi-lock-set-pattern :around 'hi-lock-set-pattern-around)))

;;(eval-after-load "isearch" '(require 'isearch+))
;;; ** Games
;;; *** 2048-game
(use-package 2048-game
  :ensure t
  :commands 2048-game)

;;; *** tron
(use-package tron
  :commands tron
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
(use-package type-break-mode
  :config ()
  )
;;; *** hippie-expand
(use-package hippie-expand
  ;;:bind ("M-s-÷" . hippie-expand)
  :bind ("M-/" . hippie-expand))

;;; *** yasnippet
(use-package yasnippet
  :commands yas-global-mode
  :defer 5
  :config (progn
	    (yas-global-mode 1)
	    (diminish 'yas-minor-mode " Y")))

;;; *** zencoding-mode
(use-package zencoding-mode
  :commands zencoding-mode
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
  :bind (("s-z" . undo-tree-undo)
         ("s-Z" . undo-tree-redo))
  :init (global-undo-tree-mode))

;;; *** move-text
(use-package move-text
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down))
  :config
  (progn
    (defun move-text-indent (&rest args)
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            ;; reactivate the mark
            (activate-mark)
            (setq deactivate-mark nil))
        (indent-according-to-mode)))
    (advice-add 'move-text-up :after 'move-text-indent)
    (advice-add 'move-text-down :after 'move-text-indent)))

;;; *** pretty-lambdada
(use-package pretty-lambdada
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

;;; *** simple
(use-package simple
  :bind (("s-d" . kill-whole-line)
         ("s-<down>" . end-of-buffer)
         ("s-<up>" . beginning-of-buffer)
         ("M-SPC" . cycle-spacing)
         ("M-s-<down>" . duplicate-line-down)
         ("M-s-<up>" . duplicate-line-up)
         ("s-\\" . shell-command-on-buffer))
  :config
  (progn
    (define-key key-translation-map (kbd "<C-backspace>") (kbd "<deletechar>"))
    (defadvice mark-whole-buffer (after mark-whole-buffer-activate-mark activate)
      (activate-mark))
    (column-number-mode 1)

    (defun escape-yank()
      "escapes yanked code if inside string"
      (interactive)
      (if (nth 3 (syntax-ppss)) ;; Checks if inside a string
          (insert-for-yank (replace-regexp-in-string "[\\\"]"
                                                     "\\\\\\&"
                                                     (current-kill 0)
                                                     t))
        (call-interactively 'yank)))


    (defun join-region (beg end)
      "Apply join-line over region."
      (interactive "r")
      (if mark-active
          (let ((beg (region-beginning))
                (end (copy-marker (region-end))))
            (goto-char beg)
            (while (< (point) end)
              (join-line 1)))))

    (defun maybe-indent-region (&rest args)
      (and (not current-prefix-arg)
           (member major-mode
                   '(emacs-lisp-mode lisp-mode
                                     clojure-mode    scheme-mode
                                     haskell-mode    ruby-mode
                                     rspec-mode      python-mode
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
      (next-line 1)
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
          (if  (= (point) (mark))
              (progn (pop-mark) (activate-mark))
            (let ((last-command nil))
              (apply orig-func args)))
        (apply orig-func args)))

    (advice-add 'set-mark-command :around 'cycle-active-mark)


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


    ))


;;; *** autorevert
(use-package autorevert
  :defer 5
  :config (global-auto-revert-mode 1))

;;; *** multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-." . mc/mark-next-like-this)
         ("C-," . mc/mark-previous-like-this)
         ("C-M-." . mc/mark-all-like-this-or-edit-lines))
  :config
  (progn
    (message "loaded multiple cursors")
    (defun mc/mark-all-like-this-or-edit-lines ()
      (interactive)
      (if (or (= (line-number-at-pos) (line-number-at-pos (mark)))
              (not (use-region-p)))
          (call-interactively 'mc/mark-all-like-this-dwim)
        (call-interactively 'mc/edit-lines)))))

;;; *** wgrep
(use-package wgrep
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
  :config (progn
            (add-hook 'java-mode-hook 'er/add-cc-mode-expansions)))

;;; *** editfns.c
(progn                                  ;editfns.c
  (put 'narrow-to-region 'disabled nil))

;;; *** broadcast-mode
(use-package broadcast
  :commands (broadcast-mode))

;;; *** avy-zap

(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim))
  :config (setq avy-timeout-seconds .3)
  :ensure t)

;;; *** key-chord
(use-package key-chord
  :ensure t
  :config
  (progn
    (key-chord-define-global "kk" 'kill-this-buffer)
    (key-chord-define-global "xx" 'eval-defun)
    (key-chord-define-global "xb" 'helm-mini ;'ido-switch-buffer
                             )
    (key-chord-define-global "xf" 'helm-find-files ;'ido-find-file
                             )
    (key-chord-define-global "xs" 'save-buffer)
    (key-chord-define-global "hh" 'helm-resume)
    (key-chord-define-global "CC" 'calc-dispatch)
    (key-chord-define-global "aa" (lambda () (interactive) (org-agenda nil "u")))
    (key-chord-define-global "x1" (kbd "C-x 1"))
    (key-chord-define-global "x2" (kbd "C-x 2"))
    (key-chord-define-global "x3" (kbd "C-x 3"))
    (key-chord-define-global "x6" (kbd "C-x 6"))
    (key-chord-define-global "x0" (kbd "C-x 0"))
    (key-chord-define-global "ii" (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
    (key-chord-define-global "LL" (lambda () (interactive) (find-file "~/org/life.org")))
    (key-chord-define-global "PP" 'org-passwords)
    (key-chord-mode 1)))
;;; *** flycheck
(use-package flycheck
  :ensure t)
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
                  tramp-file-name-regexp))))
;;; *** multi-term
(use-package multi-term
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
       (setq yas-dont-activate t)
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


  (setq term-prompt-regexp  "^[^#$%>\n]*[#$%>] *"))

;;; *** comint
(use-package comint
  :init
  (progn
    (defadvice comint-previous-input (before move-to-end-first
                                             (&optional arg try-vscroll))
      "Move to the end of file before comint-previous-input is called"
      (end-of-buffer))
    (defadvice comint-next-input (before move-to-end-first
                                         (&optional arg try-vscroll))
      "Move to the end of file before comint-next-input is called"
      (end-of-buffer))

    (ad-activate 'comint-next-input)
    (ad-activate 'comint-previous-input)))

;;; *** shell-switcher
(use-package shell-switcher
  :bind (("C-'" . shell-switcher-switch-buffer)
         ("C-x 4 '" . shell-switcher-switch-buffer-other-window)
         ("C-M-'" . shell-switcher-new-shell))
  :config (shell-switcher-mode 1))

;;; *** shell
(use-package shell
  :commands shell
  :config
  (progn
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
    (use-package mysql-shell)
    (add-hook
     'shell-mode-hook
     (lambda ()
       (dirtrack-mode t)
       ))))

;;; *** cssh
(use-package cssh
  :bind ("C-;" . shell-remote-open;cssh-term-remote-open
         )
  :config (progn
            (cssh-define-global-bindings)))

;;; *** shell-remote-open
(defun shell-remote-open ()
  "Prompt for a remote host to connect to, and open a shell there."
  (interactive)
  (let* ((remote-host (completing-read "Remote host: " (cssh-get-hosts-list)))
         (default-directory (concat "/" remote-host ":")))
    (shell (format "*shell/%s*" remote-host))))
;(bind-key "C-;" 'shell-remote-open)

;;; ** External System Integration
;;; *** dig
(use-package dig
  :config
  (put 'dig 'interactive-form '(interactive (list (completing-read "Host: " (progn (require 'cssh) (cssh-get-hosts-list)))))))

;;; *** helm-spotify
(use-package helm-spotify
  :ensure t
  :bind ("<f8>" . helm-spotify))

;;; *** butler
(use-package butler
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
    (setq butler-server-list
          '((jenkins "Jenkins" (server-address . "http://merc.footnote.com:50000/jenkins/view/news/"))))
    (setq web-log-info nil)))

;;; *** ahg
(use-package ahg
  :bind-keymap* ("C-c h g" . ahg-global-map)
  :defer t
  :config
  (progn
    (global-set-key (kbd "C-c h g SPC") 'ahg-do-command)
    ;; Ahg key binding for allowing clicking to get a diff
    (define-key ahg-log-file-line-map [mouse-1]
      (lambda ()
        (interactive)
        (let* ((r1 (ahg-log-revision-at-point t))
               (r2 (ahg-first-parent-of-rev r1))
               (fn (ahg-log-filename-at-point (point))))
          (ahg-diff r2 r1 (list fn)))))))

;;; *** magit
(use-package magit
  :bind ("C-x g" . magit-status)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (progn
    (setq magit-diff-refine-hunk 'all)
    '(defalias 'vc-print-log 'magit-log-buffer-file )
    (global-magit-file-buffer-mode 1)

    ;; fix for weird bug.  Sometimes, only when a file has been
    ;; opened, file-accessible-directory-p returns true for a file.
    ;; This fix ensures that it has to be a directory for it to return
    ;; true.
    (defun my-file-accessible-directory-p (orig-func &rest args)
      (and (apply orig-func args)
           (file-directory-p (car args))))
    (advice-add 'file-accessible-directory-p :around 'my-file-accessible-directory-p)))

;;; *** magit-gh-pulls
(use-package magit-gh-pulls
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

;;; *** restclient
(use-package restclient
  :mode (("\\.rest$" . restclient-mode)))

;;; *** google-this
'(use-package google-this
  :bind ("s-g" . google-this-lucky-search))

;;; *** nodejs-repl
(use-package nodejs-repl
  :commands nodejs-repl)

;;; *** jabber

;;playground for testing helm features
'(helm :sources `((name . "the name")
                 (candidates . ,(mapcar (lambda (x) x;(cons x (rot13 x))
                                          ) '("a" "b" "c" "d")))
                 (real-to-display . rot13)
                 (candidate-transformer (lambda (list) (mapcar 'rot13 list)))
                 ))



(use-package jabber
  :ensure t
  :commands jabber-connect-all
  :bind-keymap* ("C-c C-j" . jabber-global-keymap)
  :defer 5
  :config
  (progn
    ;(setq starttls-extra-arguments (list "--insecure" ))

    (add-hook 'jabber-chat-mode-hook 'visual-line-mode)

    ;; get backlog by pressing up past top
    (defun previous-line-or-jabber-backlog ()
      (interactive)
      (condition-case nil (previous-line)
        (error
         (message "Loading jabber history" )
         (jabber-chat-display-more-backlog 1))))
    (bind-key "<up>" 'previous-line-or-jabber-backlog jabber-chat-mode-map)
    (bind-key "C-p" 'previous-line-or-jabber-backlog jabber-chat-mode-map)

    ;; helmize it
    (defun helm-jabber-chat-source (name face filter)
      `((name . ,name)
        (candidates . ,(remove-if-not filter (jabber-concat-rosters)))
        (real-to-display . (lambda (x) (propertize (or (get x 'name) (symbol-name x)) 'face ',face)))
        (action . (lambda (x)
                    (jabber-chat-with
                     (jabber-read-account)
                     (symbol-name x))))))
    
    (defun helm-jabber-chat-with ()
      (interactive)
      (helm :sources (list (helm-jabber-chat-source "Jabber Online Contacts"
                                                    'jabber-roster-user-online (lambda (x) (get x 'connected)))
                           (helm-jabber-chat-source "Jabber Offline Contacts"
                                                    'jabber-roster-user-offline (lambda (x) (not (get x 'connected)))))))
    (bind-key "C-j" 'helm-jabber-chat-with jabber-global-keymap)
    
    ;; get jabber and dired-x to coexist
    (bind-key "C-x C-j" 'dired-jump)
    (bind-key "C-x 4 C-j" 'dired-jump-other-window)
    (bind-key* "C-c C-j" jabber-global-keymap)
    '(jabber-connect-all)))


;;; *** play-sound
(unless (and (fboundp 'play-sound-internal)
             (subrp (symbol-function 'play-sound-internal)))
  (use-package play-sound))
;;; *** emms
(use-package emms-player-mplayer
  :config
  (progn
    (define-emms-simple-player afplay '(file)
      (regexp-opt '(".mp3" ".m4a" ".aac"))
      "afplay")
    (setq emms-player-list `(,emms-player-afplay))))

;;; *** helm-gtags
(use-package helm-gtags
  :config
  (progn
    (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
    (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
    (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
    (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
    (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
    (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

;;; *** emacs-eclim
'(use-package eclim
  :ensure emacs-eclim
  :config
  (progn

    ;; Configuration
    (global-eclim-mode)
    (defun eclim-set-sole-autocomplete ()
      (setq ac-sources '(ac-source-emacs-eclim)))
    (add-hook 'java-mode-hook 'eclim-set-sole-autocomplete)
    (custom-set-variables
     '(eclim-eclipse-dirs '("/Applications/Eclipse.app/Contents/Eclipse/"))
     '(eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/eclim"))
    (use-package ac-emacs-eclim-source)
    (use-package eclimd
      :config
      (progn
        (setq eclimd-default-workspace "/Users/rblack/Documents/workspace"
              eclimd-wait-for-process nil))
      )
    (eclim-problems-show-errors)
    (key-chord-define eclim-mode-map "DD" 'eclim-java-show-documentation-for-current-element)

    ;; Patches
    
    ;; workaround for bug in files with # in their names.  Eclim
    ;; doesn't like them because it prepends a file:/// and tries to
    ;; parse as url, and # isn't a valid char in url.  Fix is to
    ;; url-escape with %23
    (defun eclim--escape-hash-filenames (orig-func &rest args)
      (let ((last-was-f))
        (mapcar
         (lambda (arg)
           (when last-was-f
             (setq arg (replace-regexp-in-string "#" "%23" arg)))
           (setq last-was-f (string-equal "-f" arg))
           arg)
         (apply orig-func args))))

    (advice-add 'eclim--expand-args :around #'eclim--escape-hash-filenames)

    (defun eclim--call-process-async (callback &rest args)
      "Like `eclim--call-process', but the call is executed
asynchronously. CALLBACK is a function that accepts a list of
strings and will be called on completion."
      (lexical-let ((handler callback)
                    (cmd (eclim--make-command args)))
        (when (not (find cmd eclim--currently-running-async-calls :test #'string=))
          (lexical-let
              ((buf (get-buffer-create (generate-new-buffer-name "*eclim-async*")))
               (tempfile (make-temp-file "eclim")))
            (when eclim-print-debug-messages
              (message "Executing: %s" cmd)
              (message "Using async buffer %s" buf)
              (message "Using temp file %s" tempfile))
            (push cmd eclim--currently-running-async-calls)

            (let ((proc (start-process-shell-command "eclim" buf (concat (eclim--make-command args) ">" tempfile))))
              (let ((sentinel (lambda (process signal)
                                (unwind-protect
                                    (save-excursion
                                      (setq eclim--currently-running-async-calls (remove-if (lambda (x) (string= cmd x)) eclim--currently-running-async-calls))
                                      (with-current-buffer buf
                                        (insert-file-contents-literally tempfile)
                                        (delete-file tempfile)
                                        (funcall handler (eclim--parse-result (buffer-substring 1 (point-max))))))
                                  (kill-buffer buf)))))
                (set-process-sentinel proc sentinel)))))))


    ;; so that when a problem is corrected, we refresh the problems so
    ;; it goes away
    (defun eclim-refresh-problems-if-necessary (&rest args)
      (when (and (string-equal (car args) "java_correct")
                 (member "-a" args))
        (eclim-problems-buffer-refresh)))

    (advice-add 'eclim--call-process :after #'eclim-refresh-problems-if-necessary)
    ;;(advice-remove 'eclim--call-process #'eclim-refresh-problems-if-necessary)

    (advice-add 'eclim--complete :after #'eclim--problems-update-maybe)

    (defun eclim-next-error ()
      "Go to the next error if there is one"
      (interactive)
      (loop
       with point = (point)
       with current = (eq (get-char-property point 'category) 'eclim-problem)
       with problem
       do (setq point (next-overlay-change point)
                problem (eq (get-char-property point 'category) 'eclim-problem))
       when (and current (not problem)) do (setq current nil)
       when (and (not current) problem) do (goto-char point) and return nil
       when (eq point (point-max)) do (with-temp-message "No more errors" (sleep-for .75)) and return nil))

    (defun eclim-previous-error ()
      "Go to the previous error if there is one"
      (interactive)
      (loop
       with point = (point)
       with current = t
       with problem
       do (setq point (previous-overlay-change point)
                problem (eq (get-char-property point 'category) 'eclim-problem))
       when (and current (not problem)) do (setq current nil)
       when (and (not current) problem) do (goto-char point) and return nil
       when (eq point (point-min)) do (with-temp-message "No previous errors" (sleep-for .75)) and return nil))

    ;; Key Bindings
    (define-key eclim-mode-map (kbd "C-c C-e c") 'eclim-java-call-hierarchy)
    (define-key eclim-mode-map (kbd "C-c C-e q") 'eclim-problems-correct)
    (define-key eclim-mode-map (kbd "s-,") 'eclim-previous-error)
    (define-key eclim-mode-map (kbd "s-.") 'eclim-next-error))
  )

;;; *** chrome helper functions
(defun chrome-url ()
  "Get current chrome url"
  (do-applescript
   (concat
    "tell application \"Google Chrome\"\n"
    " return URL of active tab of front window\n"
    "end tell\n")))

(defun chrome-host ()
  "Get current chrome host"
  (replace-regexp-in-string
   "https?://\\([^/]+\\)/.*" "\\1" (chrome-url)))

;;; ** Getting Around (search, navigation)
;;; *** ace-jump-mode
'(use-package ace-jump-mode
  :bind* ("M-j" . ace-jump-mode))

;;; *** avy
(use-package avy
  :bind* ("M-j" . avy-goto-char-timer)
  :config (setq avy-timeout-seconds .3))

;;; *** projectile
(setq helm-projectile-fuzzy-match t)
(use-package projectile
  :commands projectile-global-mode
  :init (projectile-global-mode)
  :config (progn
	    (setq projectile-completion-system 'helm)
	    (diminish 'projectile-mode)))

;;; *** helm-projectile
(use-package helm-projectile
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
    (setq helm-ag-use-grep-ignore-list t)
    ;; ag-truncate is used to tame ag results from minimized javascript files
    (setq helm-ag-base-command "~/.emacs.d/ag-truncate.sh --nocolor --nogroup --smart-case --column")
    (defun helm-find-files-ag (candidate)
      "Default action to grep files from `helm-find-files'.  Replaces helm-find-files-grep."
      (let ((default-directory candidate))
        (helm-projectile-ag)))
    (defalias 'helm-find-files-grep 'helm-find-files-ag)
    (key-chord-define-global "SS" 'helm-projectile-ag)
    (key-chord-define-global "FF" 'helm-projectile-find-file)
    '(defalias 'helm-ff-run-grep 'helm-projectile-ag)
    '(defalias 'helm-projectile-grep 'helm-projectile-ag)))

;;; *** grep
(use-package grep
  :config
  (progn
    (add-to-list 'grep-find-ignored-files "*.pdf")
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
    (add-to-list 'grep-find-ignored-directories "bin")))

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
  (define-key occur-mode-map (kbd "C-x C-q") 'occur-edit-mode))

;;; *** swiper
(use-package swiper
  :bind (("C-s" . swiper-or-isearch-forward)
         ("C-r" . swiper-or-isearch-backward))
  :config
  (progn
    (setq isearch-modes '(shell-mode term-mode Info-mode messages-buffer-mode pdf-view-mode))
    (setq ivy-display-style 'fancy)
    (defun swiper-or-isearch-forward ()
      (interactive)
      (if (member major-mode isearch-modes)
          (call-interactively 'isearch-forward)
        (call-interactively 'swiper-helm)))
    (defun swiper-or-isearch-backward ()
      (interactive)
      (if (member major-mode isearch-modes)
          (call-interactively 'isearch-backward)
        (call-interactively 'swiper-helm)))))

;;; *** swiper
(use-package swiper-helm
  :ensure t
  :config
  (progn
    (setq swiper-helm-display-function 'helm-default-display-buffer)
    (require 'which-func)
    (defun swiper-update-which-func (&rest args)
      (which-func-update-1 (helm-persistent-action-display-window)))
    (advice-add 'swiper--update-sel :after 'swiper-update-which-func )))


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
;;; *** ido
(use-package ido
  :init
  (ido-mode 1)
  :config
  (progn
    (setq ido-use-filename-at-point 'guess)
    (defun my-ffap-guesser (orig-func &rest args)
      (let* ((ffap-url-regexp nil)
             (retval (apply orig-func args)))
        (if (or
             (member retval '("/" "/**" "//"))
             (eq major-mode 'dired-mode))
            nil
          retval)))
    (advice-add 'ffap-guesser :around #'my-ffap-guesser)
    (ido-everywhere 1)
    ;;(flx-ido-mode 1)
    (setq ido-enable-flex-matching t)
    ;; disable ido faces to see flx highlights
                                        ;(setq ido-use-faces nil)
    (setq ido-create-new-buffer 'always)
    (use-package ido-ubiquitous
      :config
      (progn
        (ido-ubiquitous-mode 1)))

    '(use-package idomenu
       :bind ("s-r" . idomenu)
       :config
       (progn
         (defun flatten-index (i prefix)
           (let* ((name (car i))
                  (qname (if (> (length prefix) 0) (concat prefix "." name) name))
                  (d (cdr i)))
             (if (listp d)
                 (cl-reduce
                  'append
                  (mapcar (lambda (subindex) (flatten-index subindex qname)) d))
               (list (cons qname d)))))

         (defadvice idomenu--read (around idomenu--read-flatten
                                          (index-alist &optional prompt guess)
                                          activate )
           "Flatten hierarchical indexes that get passed to this function"
           (let ((index-alist (flatten-index (cons "" index-alist) "")))
             ad-do-it))))

    (defun find-file-sudo ()
      "Find file as root if necessary."
      (interactive)
      (when buffer-file-name
        (unless (file-writable-p buffer-file-name)
          (message "file is %s" buffer-file-name)
          (find-alternate-file (if (file-remote-p buffer-file-name) 
                                   (replace-regexp-in-string "/[^:]*\\\(:.*\\\):" (concat "/ssh\\1|sudo:" (with-parsed-tramp-file-name buffer-file-name nil host) ":") buffer-file-name)
                                 (concat "/sudo:root@localhost:" buffer-file-name))))))
    
    ;; from http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
    '(advice-add 'dired-find-file :after #'find-file-sudo)
    '(advice-add 'ido-find-file :after #'find-file-sudo)
    '(advice-remove 'dired-find-file  #'find-file-sudo)
    '(advice-remove 'ido-find-file  #'find-file-sudo)
    ))

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

;;; *** syntax-subword-mode
;; better word navigation
(use-package syntax-subword
  :config
  (progn
    (global-syntax-subword-mode 1)
    (setq syntax-subword-skip-spaces 'consistent)))
;;; *** helm
;; better word navigation
(use-package helm
  :bind* (("M-x" . helm-M-x ;;execute-extended-command
           )
          ("s-r" . helm-imenu)
          ;("M-y" . helm-show-kill-ring) ;; find something better to bind this to. 
          ("C-h a" . helm-apropos)
          ("C-h SPC" . helm-all-mark-rings)
          ("C-x C-f" . helm-find-files)
          ("s-g" . helm-google-suggest))
  :commands (helm-resume)
  :config (progn
            (helm-mode)
            (custom-set-variables
             '(helm-imenu-fuzzy-match t)
             '(helm-apropos-fuzzy-match t)
             '(helm-autoresize-min-height helm-autoresize-max-height)
             )
            (bind-key "M-r" 'helm-comint-input-ring shell-mode-map )
            (diminish 'helm-mode)
            (defalias 'man 'helm-man-woman)
            (helm-autoresize-mode t))

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
  )

;;; ** Major Modes
;;; *** python-mode
(use-package python-mode
  :ensure t
  :mode "\\.py$"
  :config
  (progn
    (setq py-jython-command "/usr/local/bin/jython")))

;;; *** pdf-view
(use-package pdf-tools
  :ensure t
  :config (pdf-tools-install)
  )

;;; *** js2-mode
(use-package js2-mode
  :ensure t
  :mode "\\.js$"
  :config
  (progn
    (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
    (use-package ac-js2
      :ensure t
      :config
      (progn
        ;(add-hook 'js2-mode-hook 'ac-js2-mode) commented out to try tern
        (setq ac-js2-evaluate-calls t) ;; installation instructions from ac-js2, for auto-complete in browser)
        (require 'jquery-doc)
        (add-hook 'js2-mode-hook 'jquery-doc-setup)
        (defun js2-short-mode-name ()
          (setq mode-name "JS"))
        (add-hook 'js2-mode-hook 'js2-short-mode-name)
        ))

    (use-package js2-refactor
      :ensure t
      :config (progn
                (js2r-add-keybindings-with-prefix "C-j")
                (add-hook 'js2-mode-hook 'js2-refactor-mode)
                (define-key js2-mode-map (kbd "C-k") 'js2r-kill)))))

(use-package tern-auto-complete
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
  :defer t
  ;; Explicitly required in modes that use this.
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

;;; *** php-mode

(defun maio/electric-semicolon ()
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
  :config
  (progn
    ;; The following bookmarks are useful for php debugging
    ;; Reload in PHP Debugger
    ;; javascript:document.cookie = "XDEBUG_SESSION=1";document.location.reload()
    ;; Start PHP Debugger
    ;; javascript:(function(){document.cookie = "XDEBUG_SESSION=1"})()
    ;; Stop PHP Debugger
    ;; javascript:(function(){document.cookie = "XDEBUG_SESSION=; expires=Thu, 01 Jan 1970 00:00:01 GMT;"})()
    
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
         ("\\.jsp\\'" . web-mode))
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
    
    (use-package paredit
      :config
      (progn
        (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
        (diminish 'paredit-mode " ()"))
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
      :config
      (progn
        (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
        (diminish 'eldoc-mode)))))

;;; *** nxml-mode
(use-package nxml-mode
  :mode  (("\\.xml$" . nxml-mode)
          ("\\.xsd$" . nxml-mode))
  :config
  ;; make nxml operate on nested element not just tags
  (setq nxml-sexp-element-flag t))


;;; *** actionscript-mode
(use-package actionscript-mode
  :mode (("\\.as$" . actionscript-mode)
         ("\\.mxml$" . actionscript-mode)))

;;; *** auto-complete
(use-package auto-complete
  :ensure t
  :defer 5
  :config (progn
            (ac-config-default)
            (diminish 'auto-complete-mode)))

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
;;; *** conf-mode
(use-package conf-mode
  :mode "\\.hgrc$"
  :ensure t)
;;; *** ses
(use-package ses-mode
  :mode "\\.ses$"
  :ensure ses
  :init (add-hook 'ses-mode-hook 'linum-mode))
;;; *** calc
(use-package calc
  :config
  (defun calc-call (fun &rest args)
    (string-to-number
     (math-format-value
      (apply
       (intern (concat "calcFunc-" (symbol-name fun)))
       (mapcar
        (lambda (s) (math-read-number-simple (number-to-string s)))
        args))))))
;;; ** File, Window and Buffer Management
;;; *** windmove
(use-package windmove
  :bind (("C-s-<right>" . windmove-right)
         ("C-s-<left>"  . windmove-left)
         ("C-s-<up>"    . windmove-up)
         ("C-s-<down>"  . windmove-down)))

;;; *** buffer-move
(use-package buffer-move
  :ensure t
  :bind (( "C-M-s-<right>" . buf-move-right)
         ( "C-M-s-<left>"  . buf-move-left)
         ( "C-M-s-<up>"    . buf-move-up)
         ( "C-M-s-<down>"  . buf-move-down)))

;;; *** buffer.c
(progn                                  ;buffer.c
  (bind-key* "C-x C-k" 'kill-this-buffer)
  ;; some bindings from- http://www.masteringemacs.org/articles/2014/02/28/my-emacs-keybindings/
  (defun kill-this-buffer ()
    (interactive)
    (kill-buffer (current-buffer)))
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

  
  ;; Settings for backups
  (setq make-backup-files t
        vc-make-backup-files t
        delete-old-versions t
        kept-new-versions 200
        kept-old-versions 0
        backup-by-copying t
        version-control t
        backup-dir  "/Users/rblack/.saves"
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

;;; *** dired
(use-package dired
  :defer t
  :config
  (progn
    (progn ;; from http://stackoverflow.com/questions/19907939/how-can-one-quickly-browse-through-lots-of-files-in-emacs
      ;; little modification to dired-mode that let's you browse through lots of files
      (add-hook 'dired-mode-hook
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

    (defun dired-virtual-vanilla ()
      (interactive)
      (let ((bufname (concat "*Dired " (buffer-name) "*"))
            (line (line-number-at-pos (point))))
        (ignore-errors (kill-buffer bufname))
        (dired (cons bufname (split-string (buffer-string) "\n" t)))
        (goto-line (+ 1 line))
        (dired-move-to-filename)))

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

    
    (add-hook 'dired-mode-hook
              (lambda()
                (define-key dired-mode-map (kbd "P")   'dired-view-corresponding-pdf)))

    (defun dired-make-shell-here ()
      (interactive)
      (shell (let ((host-name
                       (if (file-remote-p default-directory)
                           (tramp-file-name-host
                            (tramp-dissect-file-name default-directory))
                         (system-name))))
               (concat "*shell " host-name "*"))))
    
    (define-key dired-mode-map (kbd "C-!") 'dired-make-shell-here) ;does this need to be in dired-mode-hook like above?
    
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
              (t (error "mark no more than two files")))))
    (bind-key "=" 'mkm/ediff-marked-pair dired-mode-map)))


;;; *** dired-x
(use-package dired-x
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window))
  )

;;; *** window.el customizations
(progn                                  ;window.el customizations
  (bind-key* "M-o" 'other-window)
  (bind-key "C-x 6" 'toggle-window-split)
  (defun other-window-1 (&optional size)
    (when (called-interactively-p 'any)
      (other-window 1)))
  (advice-add 'split-window-below :after #'other-window-1 )
  (advice-add 'split-window-right :after #'other-window-1 )
  (setq scroll-error-top-bottom t)
  (setq scroll-preserve-screen-position t)
  (bind-key "C-S-v" 'scroll-up-line)
  (bind-key "M-V" 'scroll-down-line)
  (bind-key "C-=" 'enlarge-window)

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
     (current-buffer))))

;;; *** savehist
(use-package savehist
  :init
  (progn
    (savehist-mode 1)
    (setq savehist-ignored-variables '(hist)) ;;not sure what hist is, but it's not a list so autosave croaks on it
    (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))))

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
  :config (buffer-flip-mode 1))
;;; *** ns-win (drag n drop)
(use-package ns-win
  :config
  (global-set-key [M-drag-n-drop] 'ns-drag-n-drop))
;;; ** org
(use-package org
  :commands org-mode
  :mode ("\\.org$" . org-mode)
  :bind*
  (( "C-c l" . org-store-link)
   ( "C-c c" . org-capture)
   ( "C-c a" . org-agenda)
   ( "C-c C-x C-i" . org-clock-in)
   ( "C-c C-x C-o" . org-clock-out)
   ( "C-c C-x C-j" . org-clock-goto)
   ;; these come from mac shortcut keys (automator services)
   ;;( "s-O" . org-capture)
   ( "<C-f9>" . org-capture-mail)
   ( "<C-f10>" . org-capture-chrome)
   )
  :config
  (progn

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
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (sh . t)
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
       (js . t)))
    

    (setq org-agenda-custom-commands
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
    (bind-key (kbd "C-c g") 'org-mac-grab-link org-mode-map)
    (bind-key (kbd "C-c s") 'org-table-sum-column org-mode-map)
    (bind-key (kbd "C-s") 'isearch-forward org-mode-map)
    (bind-key (kbd "C-r") 'isearch-backward org-mode-map)
    (add-hook 'org-mode-hook 'visual-line-mode)
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
          '(("w" "Work TODO" entry (file+olp "~/org/life.org" "Work" "Tasks")
             "* TODO %? %i
:PROPERTIES:
:CREATED: %U
:END:")
            ("M" "Work Meeting" entry (file+olp "~/org/life.org" "Work" "Meetings")
             "* %T Meeting: %? %i
:PROPERTIES:
:CREATED: %U
:END:")
            ("m" "Mail" entry (file+olp "~/org/life.org" "Work" "Tasks")
             "* TODO %(org-mac-message-get-links)%? %i
:PROPERTIES:
:CREATED: %U
:END:")
            ("c" "Mail" entry (file+olp "~/org/life.org" "Work" "Tasks")
             "* TODO %(org-mac-chrome-get-frontmost-url)%? %i
:PROPERTIES:
:CREATED: %U
:END:")
            ("l" "Work TODO with link" entry (file+headline "~/org/life.org" "Work")
             "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:
%i\n  %a")
            ("b" "Buy" entry (file+headline "~/org/life.org" "Shopping")
             "* TODO %?
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
            ("t" "General TODO" entry (file+headline "~/org/life.org" "Tasks")
             "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:
%i")
            ("p" "Password" entry (file org-passwords-file)
             "* %^{Title|%(org-passwords-chrome-title)}
:PROPERTIES:
:URL: %^{URL|%(org-passwords-chrome-url)}
:PASSWORD: %^{PASSWORD|%(org-passwords-generate-password-without-symbols nil 15)}
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
    (use-package org-mac-link
      :commands org-mac-grab-link
      :config (message "org-mac-link loaded"))
    ))

;;; ** ox-reveal
(use-package ox-reveal)
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
  (progn
    (setq midnight-hook (quote (clean-buffer-list org-mobile-push xkcd)))
    (midnight-delay-set 'midnight-delay 43200)))

;;; ** exec-path-from-shell
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize) ; set the right PATH variable
  )

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
    (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)))
  
;;; ** Company-Specific Packages
;;; *** jenkins-build
(use-package jenkins-build
  :commands (dfb dbb sfb sbb lfb lbb ncb))

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
  :commands (ocr))

;;; * Misc Functions

(defun get-random-element (list)
  "Returns a random element of LIST."
  (if (not (and (list) (listp list)))
      (nth (random (1- (1+ (length list)))) list)
    (error "Argument to get-random-element not a list or the list is empty")))

(defun switch-to-random-buffer ()
  (switch-to-buffer
   (get-random-element
    (remove-if-not
     (lambda (b)
       (with-current-buffer b
         buffer-file-name)) (buffer-list))) t))

(defun random-cursor-movement ()
  (interactive)
  (save-excursion
    (save-selected-window
      (push-mark)
      (while
          (apply (lambda (operation maxtimes)
                   (loop repeat (1+ (random maxtimes))
                         always
                         (let ((p (point)))
                           (ignore-errors (apply operation nil))
                           (sit-for (+ .1 (/ (log (abs (- p (point)))) 10))))))
                 (case (random 13)
                   (0 '(previous-line 10))
                   (1 '(next-line 10 ))
                   (2 '(forward-char 10 ))
                   (3 '(backward-char 10 ))
                   (4 '(forward-word 10 ))
                   (5 '(backward-word 10 ))
                   (6 '(beginning-of-line 1))
                   (7 '(end-of-line 1))
                   (8 '(scroll-up-command 4))
                   (9 '(scroll-down-command 4))
                   (10 '(forward-sexp 1))
                   (11 '(backward-sexp 1))
                   (12 '(switch-to-random-buffer 1))
                   (t '(ignore 1))
                   ))
        ))))

(defun mandelbrot ()
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
  "Sort lines in region randomly."
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
  "Remove ^M at end of line in the whole buffer.  This is done in ahg-diff-mode
so that extra ^M's are not added when applying hunks with C-c C-a.  Plus it
is a lot more readable without the ^M's getting in the way."
  (interactive)
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat (char-to-string 13) "$")
                                (point-max) t)
        (replace-match "")))))

(defmacro measure-time (&rest body)
  "Measure and return the running time of the code block."
  (declare (indent defun))
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))

(defun func-region (start end func)
  "run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun urlencode-region (start end)
  "urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun urldecode-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))

(defun killdash9/comment-dwim (orig-func &rest args)
  (when (not (region-active-p))
    (beginning-of-line)
    (set-mark (line-end-position))
    (activate-mark))
  (apply orig-func args)
  (c-indent-line-or-region))

(advice-add 'comment-dwim :around #'killdash9/comment-dwim)

(defun open-in-desktop ()
  "Show current file in desktop (OS's file manager)."
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil)) (start-process "" nil "xdg-open" "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ⁖ with nautilus
    ) ))

(defun range (min max)
  (when (<= min max)
    (cons min (range (+ min 1) max))))

(defun save-and-refresh-chrome()
  (interactive)
  (when buffer-file-name
    (save-buffer))
  (do-applescript "tell application \"Google Chrome\" to reload front window's active tab"))

(key-chord-define-global "RR" 'save-and-refresh-chrome)

(require 'cl-lib)

(defvar punctuation-marks '(","
                            "."
                            "'"
                            "&"
                            "\"")
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


;;; * Customization Variables
(load custom-file)

;;; * Secrets file
(load "~/.emacs-secrets.el")

;;; * Load Theme
(load-theme 'tron)

;;; * Local Variables
;; Local Variables:
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: ";;; "
;; End:
    

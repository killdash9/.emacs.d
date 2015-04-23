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
         ("M-<down>" . move-text-down)))

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
    (bind-key "C-<right>" 'sp-slurp-hybrid-sexp smartparens-mode-map)))

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

    ;; Indent after yank
    (dolist (command '(yank yank-pop))
      (eval `(defadvice ,command (after indent-region activate)
               (and (not current-prefix-arg)
                    (member major-mode '(emacs-lisp-mode lisp-mode
                                                         clojure-mode    scheme-mode
                                                         haskell-mode    ruby-mode
                                                         rspec-mode      python-mode
                                                         c-mode          c++-mode
                                                         objc-mode       latex-mode
                                                         plain-tex-mode  java-mode))
                    (let ((mark-even-if-inactive transient-mark-mode))
                      (indent-region (region-beginning) (region-end) nil))))))

    (defun join-region (beg end)
      "Apply join-line over region."
      (interactive "r")
      (if mark-active
          (let ((beg (region-beginning))
                (end (copy-marker (region-end))))
            (goto-char beg)
            (while (< (point) end)
              (join-line 1)))))
    ;; This auto-indents on paste
    (dolist (command '(yank yank-pop))
      (eval `(defadvice ,command (after indent-region activate)
               (and (not current-prefix-arg)
                    (member major-mode '(emacs-lisp-mode lisp-mode
                                                         clojure-mode    scheme-mode
                                                         haskell-mode    ruby-mode
                                                         rspec-mode      python-mode
                                                         c-mode          c++-mode
                                                         objc-mode       latex-mode
                                                         plain-tex-mode  php-mode
                                                         js2-mode))
                    (let ((mark-even-if-inactive transient-mark-mode))
                      (indent-region (region-beginning) (region-end) nil))))))

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
  :commands wgrep-setup)

;;; *** visible-mark
(use-package visible-mark
  :defer 5
  :init
  (defface visible-mark-active ;; put this before (require 'visible-mark)
    '((((type tty) (class mono)))
      (t (:background "magenta"))) "")
  :config (global-visible-mark-mode 1))

;;; *** expand-region
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region)
  :config (progn
            (add-hook 'java-mode-hook 'er/add-cc-mode-expansions)))

;;; *** editfns.c
(progn                                  ;editfns.c
  (put 'narrow-to-region 'disabled nil))

;;; *** broadcast-mode
(use-package broadcast-mode
  :commands (broadcast-mode))

;;; *** ace-jump-zap
(use-package ace-jump-zap
  :ensure t
  :bind
  (("M-z" . ace-jump-zap-up-to-char-dwim)
   ("C-M-z" . ace-jump-zap-to-char-dwim)))
;;; *** key-chord
(use-package key-chord
  :ensure t
  :config
  (progn
    (key-chord-define-global "kk" 'kill-this-buffer)
    (key-chord-define-global "xb" 'ido-switch-buffer)
    (key-chord-define-global "xf" 'ido-find-file)
    (key-chord-define-global "xs" 'save-buffer)
    (key-chord-define-global "r4" 'helm-recentf)
    (key-chord-define-global "hh" 'helm-resume)
    (key-chord-mode 1)))
;;; *** flycheck
(use-package flycheck
  :ensure t)
;;; ** Terminal
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
    (use-package mysql-shell)
    (add-hook
     'shell-mode-hook
     (lambda ()
       (dirtrack-mode t)
       ))))

;;; *** cssh
(use-package cssh
  :ensure t
  :bind ("C-;" . cssh-term-remote-open))

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
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (progn
    '(defun endless/add-PR-fetch ()
      "If refs/pull is not defined on a GH repo, define it."
      (let ((fetch-address
             "+refs/pull/*/head:refs/pull/origin/*"))
        (unless (member
                 fetch-address
                 (magit-get-all "remote" "origin" "fetch"))
          (when (string-match
                 "github" (magit-get "remote" "origin" "url"))
            (magit-git-string
             "config" "--add" "remote.origin.fetch"
             fetch-address)))))
    ;;(add-hook 'magit-mode-hook #'endless/add-PR-fetch)
    (magit-auto-revert-mode 0)
    ))

;;; *** restclient
(use-package restclient
  :mode (("\\.rest$" . restclient-mode)))

;;; *** google-this
(use-package google-this
  :bind ("s-g" . google-this-lucky-search))

;;; *** nodejs-repl
(use-package nodejs-repl
  :commands nodejs-repl)

;;; *** jabber
(use-package jabber
  :ensure t
  :commands jabber-connect-all
  :bind-keymap* ("C-c C-j" . jabber-global-keymap)
  :defer 5
  :config
  (progn
    (setq starttls-extra-arguments (list "--insecure" ))
    ;; Make ctrl-return not submit
    (add-hook 'jabber-chat-mode-hook
              (lambda () (bind-key "<C-return>" (lambda () (interactive) (insert "\n")) jabber-chat-mode-map)))
    (add-hook 'jabber-chat-mode-hook 'visual-line-mode)
    ;; get jabber and dired-x to coexist
    (bind-key "C-x C-j" 'dired-jump)
    (bind-key "C-x 4 C-j" 'dired-jump-other-window)
    (bind-key* "C-c C-j" jabber-global-keymap)
    (jabber-connect-all)))
;;; *** play-sound
(unless (and (fboundp 'play-sound-internal)
             (subrp (symbol-function 'play-sound-internal)))
  (use-package play-sound))

;;; ** Getting Around (search, navigation)
;;; *** ace-jump-mode
(use-package ace-jump-mode
  :bind* ("M-j" . ace-jump-mode))

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
    (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case ")
    (defun helm-find-files-ag (candidate)
      "Default action to grep files from `helm-find-files'.  Replaces helm-find-files-grep."
      (let ((default-directory candidate))
        (helm-projectile-ag)))
    (defalias 'helm-find-files-grep 'helm-find-files-ag)
    '(defalias 'helm-ff-run-grep 'helm-projectile-ag)
    '(defalias 'helm-projectile-grep 'helm-projectile-ag)))

;;; *** grep
(use-package grep
  :config
  (progn
    (add-to-list 'grep-find-ignored-files "*.pdf")
    (add-to-list 'grep-find-ignored-files "pts.js")
    (add-to-list 'grep-find-ignored-files "jquery-*")
    (add-to-list 'grep-find-ignored-files "*.log")
    (add-to-list 'grep-find-ignored-files "*.log.*")
    (add-to-list 'grep-find-ignored-files "#*#")
    (add-to-list 'grep-find-ignored-files "#.orig")
    (add-to-list 'grep-find-ignored-directories "templates_c")
    (add-to-list 'grep-find-ignored-directories "ci_system")
    (add-to-list 'grep-find-ignored-directories "bin5")
    (add-to-list 'grep-find-ignored-directories "bin")))
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

;;; *** swiper
(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)) )


;;; *** cmds.c
(progn                                  ;cmds.c
  (bind-key "s-<right>" 'end-of-line)
  (bind-key "s-<left>" 'beginning-of-line))

;;; *** smex
(use-package smex
  :bind (("M-x" . smex)
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

    (use-package idomenu
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

    '(defun find-file-sudo ()
      "Find file as root if necessary."
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

;;; *** fiplr
'(use-package fiplr
  :ensure t
  :commands (dfo dbo sfo sbo lfo lbo nco)
  :bind (("s-P" . choose-fiplr-directory)
         ("s-p" . fiplr-find-file)
         ("s-F" . dfo)
         ("s-B" . dbo)
         ("s-C" . nco))
  :config
  (progn
    (setq fiplr-ignored-globs
          '(
            (directories (".git" ".svn" ".hg" ".bzr" "bin" "bin5" "docs" "templates_c"))
            (files (".#*" "*~" "*.so" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip" "*.class" "*/"))))


    (defvar fiplr-directory-history nil "History list for fiplr find")
    (setq fiplr-directory-history nil)
                                        ; Project-specific finders
    (defun choose-fiplr-directory (dir)
      (interactive
       (list
        (let* ((file-name-history fiplr-directory-history)
               (history-delete-duplicates t)
               (fiplr-root (fiplr-root))
               (initial
                (if fiplr-directory-history
                    (find-if (lambda(e) (not (equal (expand-file-name e) (expand-file-name fiplr-root)))) fiplr-directory-history)
                  fiplr-root)))
          (car (add-to-history
                'fiplr-directory-history
                (read-file-name-default "Search in directory: " initial initial t nil 'file-directory-p))))))
      (fiplr-find-file-in-directory dir fiplr-ignored-globs))

    (defun dfo ()
      (interactive) (fiplr-find-file-in-directory "/Users/rblack/code/hg/news-dev-frontend" fiplr-ignored-globs))
    (defun dbo ()
      (interactive) (fiplr-find-file-in-directory "/Users/rblack/code/hg/news-dev-backend" fiplr-ignored-globs))
    (defun sfo ()
      (interactive) (fiplr-find-file-in-directory "/Users/rblack/code/hg/news-stage-frontend" fiplr-ignored-globs))
    (defun sbo ()
      (interactive) (fiplr-find-file-in-directory "/Users/rblack/code/hg/news-stage-backend" fiplr-ignored-globs))
    (defun lfo ()
      (interactive) (fiplr-find-file-in-directory "/Users/rblack/code/hg/news-live-frontend" fiplr-ignored-globs))
    (defun lbo ()
      (interactive) (fiplr-find-file-in-directory "/Users/rblack/code/hg/news-live-backend" fiplr-ignored-globs))
    (defun nco ()
      (interactive) (fiplr-find-file-in-directory "/Users/rblack/code/hg/news-config" fiplr-ignored-globs))
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

;;; jenkins-build
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
  :commands (helm-resume
             helm-recentf))

;;; ** Major Modes
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
        (add-hook 'js2-mode-hook 'ac-js2-mode)
        (setq ac-js2-evaluate-calls t) ;; installation instructions from ac-js2, for auto-complete in browser)
        (require 'jquery-doc)
        (add-hook 'js2-mode-hook 'jquery-doc-setup)
        ))

    (use-package js2-refactor
      :ensure t
      :config (js2r-add-keybindings-with-prefix "C-j"))))

;;; *** json-mode
(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)
         ("\\.har\\'" . json-mode)))

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

(use-package php-mode
  :ensure t
  :mode (("\\.inc\\'" . php-mode)
         ("\\.php\\'" . php-mode))
  :config
  (progn

    (defun maio/electric-semicolon ()
      (interactive)
      (end-of-line)
      (when (not (looking-back ";"))
        (insert ";")))
    
    (define-key php-mode-map ";" 'maio/electric-semicolon)
    
    (add-hook 'php-mode-hook 'smartparens-mode)
    (setq php-mode-coding-style 'symfony2)
    (bind-key [(control .)] nil php-mode-map)
    (add-hook 'php-mode-hook 'flycheck-mode)
    ;; we don't want this to overshadow our multi-cursor selection
    (use-package php-eldoc
      :ensure t
      :config
      (add-hook 'php-mode-hook 'php-eldoc-enable))
    '(use-package flymake-php ;; using flycheck now
       :ensure t
       :config
       (add-hook 'php-mode-hook 'flymake-php-load))))

;;; *** geben
(use-package geben
  :commands geben
  :config
  (progn
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
      (c-set-offset 'substatement-open 0))

    
    (add-hook 'java-mode-hook 'better-java-indexing)
    (add-hook 'java-mode-hook 'smartparens-mode)
    (add-hook 'c-mode-hook 'my-c-setup)
    (add-hook 'java-mode-hook 'my-c-setup)
    (define-key c-mode-map ";" 'maio/electric-semicolon)

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

;;; *** vtl
(use-package vtl
  :mode ".*/email.*\\.txt$")         ;opens our email templates as vtl

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
      (add-hook 'after-save-hook 'check-parens nil t))

    (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
    
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
	    (global-auto-complete-mode)
	    (diminish 'auto-complete-mode)))

;;; *** apples-mode
(use-package apples-mode
  :mode "\\.\\(applescri\\|sc\\)pt\\'"
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
  (bind-key* "s-w" 'kill-this-buffer)
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
(progn   ;files.el
  ;; Settings for backups -- move emacs terds into separate dir

  (setq make-backup-files t
        vc-make-backup-files t
        delete-old-versions t
        kept-new-versions 100
        kept-old-versions 0
        backup-by-copying t
        version-control t)
  (setq backup-dir  "~/.saves")
  (if (not (file-exists-p backup-dir))
      (make-directory backup-dir))
  (setq backup-directory-alist `(("." . ,backup-dir)))
  
  (defun force-backup-of-buffer ()
    (setq buffer-backed-up nil))

  (add-hook 'before-save-hook 'force-backup-of-buffer)
  ;; this is what tramp uses
  (setq tramp-backup-directory-alist backup-directory-alist))

;;; *** backup-walker
(use-package backup-walker
  :ensure t
  :commands backup-walker-start)

;;; *** dired
(use-package dired
  :defer t
  :config
  (progn
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
    (setq savehist-ignored-variables '(ido-file-history))
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
  :bind
  (( "C-c l" . org-store-link)
   ( "C-c c" . org-capture)
   ( "C-c a" . org-agenda)
   ( "C-c C-x C-i" . org-clock-in)
   ( "C-c C-x C-o" . org-clock-out)
   ;; these come from mac shortcut keys (automator services)
   ;;( "s-O" . org-capture)
   ( "<C-f9>" . org-capture-mail)
   ( "<C-f10>" . org-capture-chrome)
   )
  :config
  (progn
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
             "* %T Meeting: %?")
            ("m" "Mail" entry (file+olp "~/org/life.org" "Work" "Tasks")
             "* TODO %(org-mac-message-get-links)%?")
            ("c" "Mail" entry (file+olp "~/org/life.org" "Work" "Tasks")
             "* TODO %(org-mac-chrome-get-frontmost-url)%?")
            ("l" "Work TODO with link" entry (file+headline "~/org/life.org" "Work")
             "* TODO %?\n  %i\n  %a")
            ("b" "Buy" entry (file+headline "~/org/life.org" "Shopping")
             "* TODO %?")
            ("r" "Rental Business" entry (file+olp "~/org/life.org" "Rental Business" "Tasks")
             "* TODO %?")
            ("h" "Home TODO" entry (file+olp "~/org/life.org" "Home" "Tasks")
             "* TODO %?\n  %i\n")
            ("s" "SAR TODO" entry (file+olp "~/org/life.org" "SAR" "Tasks")
             "* TODO %?\n  %i\n")
            ("t" "General TODO" entry (file+headline "~/org/life.org" "Tasks")
             "* TODO %?\n  %i\n")
            ("p" "Password" entry (file org-passwords-file)
             "* %^{Title|%(org-passwords-chrome-title)}
:PROPERTIES:
:URL: %^{URL|%(org-passwords-chrome-url)}
:PASSWORD: %^{PASSWORD|%(org-passwords-generate-password-without-symbols nil 15)}
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

;;; ** org-passwords
(use-package org-passwords
  :bind ( "C-*" . org-passwords)
  :demand t
  :config
  (progn
    (setq org-passwords-random-words-dictionary "/usr/share/dict/words")
    (bind-key "C-c u" 'org-passwords-copy-username org-passwords-mode-map)
    (bind-key (kbd "C-c p") 'org-passwords-copy-password org-passwords-mode-map)))

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
  :commands (ocr-dev ocrtext-dev ocr-stage ocrtext-stage ocr-live ocrtext-live))

;;; * Misc Functions
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

(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))

    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))


(defadvice set-mark-command (around push-if-uu (arg) activate)
  (if (and (consp arg) (> (prefix-numeric-value arg) 4))
      (progn
        (unpop-to-mark-command)
        (setq this-command 'unpop-to-mark-command))
    ad-do-it
    ))

(defun killdash9/comment-dwim (orig-func &rest args)
  (when (not (region-active-p))
    (beginning-of-line)
    (set-mark (line-end-position))
    (activate-mark))
  (apply orig-func args)
  (c-indent-line-or-region))

(advice-add 'comment-dwim :around #'killdash9/comment-dwim)

;;; * Customization Variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-and-a-half-executable "/opt/local/bin/ack-5.12")
 '(ahg-hg-command "/usr/local/bin/hg")
 '(ahg-summary-remote t)
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(atari-click-mode t)
 '(backup-by-copying-when-linked t)
 '(blink-cursor-mode t)
 '(buffer-face-mode-face (quote fixed-pitch))
 '(c-default-style
   (quote
    ((java-mode . "strousttrup")
     (awk-mode . "awk")
     (other . "stroustrup"))))
 '(calendar-latitude 40.2444)
 '(calendar-longitude -111.6608)
 '(comint-completion-addsuffix t)
 '(comint-completion-autolist t)
 '(comint-input-ignoredups t)
 '(comint-move-point-for-output t)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(cursor-in-non-selected-windows t)
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "0c3f22acd840eaa072cb23d6c786fa00a69697593ac9d931087b87e072262a61" "6788ec244e2cb7e03ec12264888f3ad956f6373e92c0f964288fb984dd628977" "815956f68af145fca4fca99354b218112ce634225705931c0a8f6cc7c2c821ab" "f4405fefab9d7828fd3e0876a21af95cfae7d03146fb95b6480118325b43e22c" "05861f7ac0f5445539cb45ec2c1c47b4434d2e3c98f1de329d590242a70694ab" "efb876a2714f5a68f60f9db2b55852f039de39043efe788086d391d054908d31" "09a8deda49fcf29d19225b9f04000e8df5bf63907471b8f7a22019b76ccf18fd" "ee0ab0c0064d76662eef47614a587b3316a81418e54090a41b8d9704a7fcfee1" default)))
 '(custom-theme-load-path (quote (custom-theme-directory t "~/.emacs.d/themes")))
 '(debug-on-error nil)
 '(dired-dwim-target t)
 '(dired-listing-switches "-alh")
 '(dirtrack-list (quote ("^\\(.*/.*\\)\\$ " 1)))
 '(display-time-day-and-date t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(epa-file-cache-passphrase-for-symmetric-encryption t)
 '(erc-hide-list (quote ("JOIN" "QUIT")))
 '(explicit-bash-args (quote ("--noediting" "-i" "-l")))
 '(fancy-splash-image "splash.png")
 '(flycheck-php-phpmd-executable
   "/Users/rblack/code/hg/news-dev-frontend/vendor/phpmd/phpmd/src/bin/phpmd")
 '(flycheck-phpmd-rulesets (quote ("cleancode" "design" "unusedcode")))
 '(gnutls-trustfiles
   (quote
    ("/etc/ssl/certs/ca-certificates.crt" "/etc/pki/tls/certs/ca-bundle.crt" "/etc/ssl/ca-bundle.pem" "/usr/ssl/certs/ca-bundle.crt" "/opt/local/share/curl/curl-ca-bundle.crt")))
 '(grep-highlight-matches t)
 '(heartbeat-cursor-mode nil)
 '(helm-M-x-fuzzy-match t)
 '(helm-recentf-fuzzy-match t)
 '(httpd-port 8765)
 '(ido-use-filename-at-point (quote guess))
 '(imenu-auto-rescan t)
 '(jabber-alert-message-hooks
   (quote
    (jabber-message-wave jabber-message-echo jabber-message-scroll)))
 '(jabber-alert-message-wave
   "/Applications/Messages.app/Contents/Resources/Received Message.aiff")
 '(jabber-alert-muc-hooks (quote (jabber-muc-wave jabber-muc-echo jabber-muc-scroll)))
 '(jabber-alert-muc-wave
   "/Applications/Messages.app/Contents/Resources/Received Message.aiff")
 '(jabber-alert-presence-hooks nil)
 '(jabber-auto-reconnect t)
 '(jabber-history-enabled t)
 '(jabber-reconnect-delay 1800)
 '(jabber-use-global-history nil)
 '(litable-result-format "=> %s ")
 '(load-prefer-newer t)
 '(mazemax-char "C")
 '(mew-imap-ssl t)
 '(mode-require-final-newline nil)
 '(multi-term-program-switches "-l")
 '(org-agenda-files (quote ("~/org/life.org" "~/org/flagged.org")))
 '(org-agenda-include-diary t)
 '(org-agenda-prefix-format
   (quote
    ((agenda . " %i %-12:c%?-12t% s%-10 e")
     (timeline . "  % s")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c"))))
 '(org-agenda-restore-windows-after-quit nil)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-window-setup (quote current-window))
 '(org-blank-before-new-entry (quote ((heading) (plain-list-item))))
 '(org-checkbox-hierarchical-statistics nil)
 '(org-completion-use-ido t)
 '(org-emphasis-alist
   (quote
    (("*" font-lock-warning-face)
     ("/" italic)
     ("_" underline)
     ("=" org-verbatim verbatim)
     ("~" org-code verbatim)
     ("+"
      (:strike-through t)))))
 '(org-enforce-todo-dependencies t)
 '(org-export-with-sub-superscripts nil)
 '(org-hide-emphasis-markers t)
 '(org-html-postamble nil)
 '(org-imenu-depth 3)
 '(org-log-done (quote time))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-mac-link)))
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-targets (quote (("~/org/life.org" :level . 2))))
 '(org-refile-use-outline-path t)
 '(org-return-follows-link t)
 '(org-src-fontify-natively t)
 '(org-startup-indented t)
 '(org-support-shift-select t)
 '(package-pinned-packages (quote ((csv-mode . "\"gnu\""))))
 '(paradox-automatically-star t)
 '(paradox-github-token t)
 '(php-manual-path "/Users/rblack/Downloads/php-chunked-xhtml")
 '(projectile-enable-caching t)
 '(projectile-ignored-projects (quote ("~/.emacs.d/")))
 '(safe-local-variable-values
   (quote
    ((org-confirm-babel-evaluate)
     (encoding . utf-8)
     (org-babel-no-eval-on-ctrl-c-ctrl-c . t)
     (org-export-with-toc . t)
     (org-export-with-section-numbers)
     (org-export-with-toc))))
 '(save-place nil nil (saveplace))
 '(send-mail-function (quote mailclient-send-it))
 '(shell-switcher-new-shell-function (quote shell-switcher-make-shell))
 '(show-paren-mode t)
 '(slime-volleyball-enable-sound nil)
 '(soap-debug t)
 '(swiper-completion-method (quote helm))
 '(term-bind-key-alist
   (quote
    (("C-c C-c" . term-interrupt-subjob)
     ("C-s" . isearch-forward)
     ("C-r" . isearch-backward)
     ("C-m" . term-send-raw)
     ("M-<right>" . term-send-forward-word)
     ("M-f" . term-send-forward-word)
     ("M-<left>" . term-send-backward-word)
     ("M-b" . term-send-backward-word)
     ("M-p" . term-send-up)
     ("M-n" . term-send-down)
     ("<C-kp-delete>" . term-send-forward-kill-word)
     ("<C-backspace>" . term-send-backward-kill-word)
     ("M-r" . term-send-reverse-search-history)
     ("M-," . term-send-input)
     ("M-." . comint-dynamic-complete)
     ("s-<left>" . term-send-home)
     ("s-<right>" . term-send-end))))
 '(term-eol-on-send nil)
 '(term-suppress-hard-newline nil)
 '(term-unbind-key-list
   (quote
    ("C-x" "C-c" "C-y" "C-h" "C-f" "C-b" "M-:" "C-x C-k" "M-o" "M-x")))
 '(tls-program
   (quote
    ("openssl s_client -connect %h:%p -no_ssl2 -ign_eof" "gnutls-cli --insecure -p %p %h" "gnutls-cli --insecure -p %p %h --protocols ssl3")))
 '(tool-bar-mode nil)
 '(transient-mark-mode nil)
 '(undo-tree-mode-lighter "")
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(web-mode-code-indent-offset 4)
 '(whitespace-line-column 10000)
 '(yas-also-auto-indent-first-line t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jabber-roster-user-online ((t (:foreground "green3" :slant normal :weight bold))))
 '(litable-result-face ((t (:inherit font-lock-preprocessor-face))))
 '(mazemax-face ((t (:background "gray42" :foreground "gray42" :inverse-video nil))))
 '(mazemax-free-face ((t (:background "gray0"))))
 '(whitespace-empty ((t nil)))
 '(whitespace-indentation ((t nil)))
 '(whitespace-line ((t nil)))
 '(whitespace-space-before-tab ((t nil)))
 '(whitespace-tab ((t (:foreground "darkgray"))))
 '(whitespace-trailing ((t nil))))

;;; * Secrets file
(load "~/.emacs-secrets.el")

;;; * Load Theme
(load-theme 'tron)

;;; * Local Variables
;; Local Variables:
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: ";;; "
;; End:
    

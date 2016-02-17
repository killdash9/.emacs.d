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
 '(async-shell-command-buffer (quote new-buffer))
 '(atari-click-mode t)
 '(backup-by-copying-when-linked t)
 '(blink-cursor-mode t)
 '(buffer-face-mode-face (quote fixed-pitch))
 '(c-basic-offset 4)
 '(c-default-style
   (quote
    ((java-mode . "stroustrup")
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
    ("b987df2743acef44d0ea46d555ff4a5dcf522e0b7791ea0fca62315f034b04b8" "01ccbb2bbd660e98e3977a528bcf905f16cc86d4d881a4df03d3f98a3f611fa1" "46a8646bfe191df614aae7b9209bb3dc72a4259c0f5cacd7b61a03a65d470783" "6aa7714697f9e9f495c78fac77e40cef181e453916d1b19e691fa240116f9ec9" "7bc5c5e58a321ff198d92db4bb377b2e5928f56cc776efca69d45660754c2a3e" "a80f6a85d84170b67749782db7e1aa7c0339caf6d645879ba9924536f87db762" "11b82f1b39397638b5c26f3a6c038992a870b614a1d59455871aa9d2ecb47939" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "0c3f22acd840eaa072cb23d6c786fa00a69697593ac9d931087b87e072262a61" "6788ec244e2cb7e03ec12264888f3ad956f6373e92c0f964288fb984dd628977" "815956f68af145fca4fca99354b218112ce634225705931c0a8f6cc7c2c821ab" "f4405fefab9d7828fd3e0876a21af95cfae7d03146fb95b6480118325b43e22c" "05861f7ac0f5445539cb45ec2c1c47b4434d2e3c98f1de329d590242a70694ab" "efb876a2714f5a68f60f9db2b55852f039de39043efe788086d391d054908d31" "09a8deda49fcf29d19225b9f04000e8df5bf63907471b8f7a22019b76ccf18fd" "ee0ab0c0064d76662eef47614a587b3316a81418e54090a41b8d9704a7fcfee1" default)))
 '(custom-theme-load-path (quote (custom-theme-directory t "~/.emacs.d/themes")) t)
 '(debug-on-error nil)
 '(dired-dwim-target t)
 '(dired-guess-shell-alist-user
   (quote
    (("\\.pdf\\'" "/Applications/Adobe\\ Reader.app/Contents/MacOS/AdobeReader" "pdftotext"))))
 '(dired-listing-switches "-alh")
 '(dirtrack-list (quote ("^\\(.*/.*\\)\\$ " 1)))
 '(display-time-day-and-date t)
 '(eclim-accepted-file-regexps (quote ("\\.java" "\\.groovy")))
 '(eclim-eclipse-dirs (quote ("/Applications/Eclipse.app/Contents/Eclipse/")))
 '(eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/eclim")
 '(eclim-print-debug-messages nil)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(epa-file-cache-passphrase-for-symmetric-encryption t)
 '(erc-hide-list (quote ("JOIN" "QUIT")))
 '(explicit-bash-args (quote ("--noediting" "-i" "-l")))
 '(fancy-splash-image "splash.png")
 '(flycheck-php-phpmd-executable
   "/Users/rblack/code/hg/news-dev-frontend/vendor/phpmd/phpmd/src/bin/phpmd")
 '(flycheck-phpmd-rulesets (quote ("cleancode" "design" "unusedcode")))
 '(global-auto-revert-mode t)
 '(global-eclim-mode t)
 '(global-visible-mark-mode t)
 '(gnutls-trustfiles
   (quote
    ("/etc/ssl/certs/ca-certificates.crt" "/etc/pki/tls/certs/ca-bundle.crt" "/etc/ssl/ca-bundle.pem" "/usr/ssl/certs/ca-bundle.crt" "/opt/local/share/curl/curl-ca-bundle.crt")))
 '(grep-find-template "find . <X> -type f <F> -exec grep <C> -ZnH -e <R> {} +")
 '(grep-highlight-matches t)
 '(heartbeat-cursor-mode nil)
 '(helm-M-x-fuzzy-match t)
 '(helm-apropos-fuzzy-match t)
 '(helm-autoresize-min-height helm-autoresize-max-height)
 '(helm-imenu-fuzzy-match t)
 '(helm-recentf-fuzzy-match t)
 '(httpd-port 8765)
 '(imenu-auto-rescan t)
 '(jabber-alert-message-hooks
   (quote
    (jabber-message-wave jabber-message-echo jabber-message-scroll send-text-message-hook)))
 '(jabber-alert-message-wave
   "/Applications/Messages.app/Contents/Resources/Received Message.aiff")
 '(jabber-alert-muc-hooks
   (quote
    (jabber-muc-wave jabber-muc-echo jabber-muc-scroll send-text-muc-hook)))
 '(jabber-alert-muc-wave
   "/Applications/Messages.app/Contents/Resources/Received Message.aiff")
 '(jabber-alert-presence-hooks nil)
 '(jabber-auto-reconnect t)
 '(jabber-history-enabled t)
 '(jabber-history-muc-enabled t)
 '(jabber-reconnect-delay 1800)
 '(jabber-use-global-history nil)
 '(large-file-warning-threshold 100000000)
 '(line-number-mode t)
 '(litable-result-format "=> %s ")
 '(load-prefer-newer t)
 '(magit-file-buffer-arguments nil)
 '(magit-log-auto-more t)
 '(magit-revision-show-gravatars nil)
 '(magit-status-headers-hook
   (quote
    (magit-insert-diff-filter-header magit-insert-remote-header magit-insert-head-header magit-insert-upstream-header magit-insert-tags-header)))
 '(markdown-command "multimarkdown")
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
 '(org-crypt-disable-auto-save (quote encrypt))
 '(org-edit-src-content-indentation 0)
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
 '(org-src-tab-acts-natively t)
 '(org-startup-indented t)
 '(org-support-shift-select t)
 '(org-use-speed-commands t)
 '(package-pinned-packages (quote ((csv-mode . "\"gnu\""))))
 '(package-selected-packages (quote (use-package)))
 '(paradox-automatically-star t)
 '(paradox-github-token t)
 '(php-manual-path "/Users/rblack/Downloads/php-chunked-xhtml")
 '(projectile-enable-caching t)
 '(projectile-ignored-projects (quote ("~/.emacs.d/")))
 '(read-quoted-char-radix 16)
 '(recentf-max-saved-items 400)
 '(safe-functions (quote (ignore calc-call)))
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
 '(sp-hybrid-kill-excessive-whitespace t)
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
 '(tramp-verbose 9)
 '(transient-mark-mode nil)
 '(undo-tree-mode-lighter "")
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(visible-mark-faces (quote (visible-mark-face1 visible-mark-face2)))
 '(visible-mark-forward-faces (quote (visible-mark-forward-face1)))
 '(visible-mark-forward-max 0)
 '(visible-mark-max 2)
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
 '(show-paren-match ((t (:background "steelblue3" :foreground "black"))))
 '(visible-mark-active ((t (:inherit visible-mark-face1))))
 '(visible-mark-face1 ((t (:background "light salmon" :foreground "black"))))
 '(visible-mark-face1-black ((t (:inherit visible-mark-face1 :foreground "black"))))
 '(visible-mark-face2 ((t (:background "light goldenrod" :foreground "black"))))
 '(visible-mark-face2-black ((t (:inherit visible-mark-face2 :foreground "black"))))
 '(whitespace-empty ((t nil)))
 '(whitespace-indentation ((t nil)))
 '(whitespace-line ((t nil)))
 '(whitespace-space-before-tab ((t nil)))
 '(whitespace-tab ((t (:foreground "darkgray"))))
 '(whitespace-trailing ((t nil))))

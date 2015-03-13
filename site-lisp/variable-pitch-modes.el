(defvar variable-pitch-modes
  '(help-mode
    Info-mode
    jabber-chat-mode
    messages-buffer-mode
    org-mode
    ))

(defun variable-pitch-modes-hook ()
  (when (member major-mode variable-pitch-modes)
    (variable-pitch-mode)))

(add-hook 'after-change-major-mode-hook 'variable-pitch-modes-hook)

(provide 'variable-pitch-modes)
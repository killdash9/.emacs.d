(defvar variable-pitch-modes
  '(help-mode
    Info-mode
    jabber-chat-mode
    messages-buffer-mode
    org-mode
    ))

(defun variable-pitch-modes-hook ()
  (when (member major-mode variable-pitch-modes)
    (variable-pitch-mode 1)))

(add-hook 'after-change-major-mode-hook 'variable-pitch-modes-hook)
;; go through existing buffers
(mapc
 (lambda (buf)
   (with-current-buffer buf
     (variable-pitch-modes-hook)))
 (buffer-list))

(provide 'variable-pitch-modes)
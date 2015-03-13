(define-minor-mode undo-list-mode "A minor mode that displays the undo list in the mode line.  For testing the broadcast plugin"
  nil (:eval (undo-list-format))
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-u") 'undo-list-clear )
    map))

(advice-add  'undo-boundary :after #'log-undo-boundary)
(advice-remove 'undo-boundary #'log-undo-boundary)

(defun undo-list-clear ()
  (interactive)
  (setq buffer-undo-list nil))

(defun log-undo-boundary ()
  (message "Undo boundary"))

(defun undo-list-format ()
  (format " %s" buffer-undo-list))

(provide 'undo-list-mode)
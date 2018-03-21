(define-minor-mode mark-state-mode
  "Display mark state in mode line"
  nil nil nil
  :lighter (:eval (format " %s/%s" transient-mark-mode mark-active))
  :global t)

(provide 'mark-state-mode)
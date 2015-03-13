;;;###autoload
(define-minor-mode kill-ring-mode "A minor mode that displays the kill ring in the mode line.  For testing the broadcast plugin"
  nil (:eval (kill-ring-mode-line)) nil)

(defun kill-ring-mode-line ()
  (concat (kill-ring-format kill-ring) (kill-ring-format kill-ring-yank-pointer)))

(defmacro kill-ring-format (list)
  `(concat " "
           (mapconcat
            (lambda (s) (string-trim
                    (let ((str (replace-regexp-in-string "[\r\n \t]+" " " s)))
                      (if (> (length str) 3) (substring-no-properties str 0 3) str))))
            ,list ",")))

(provide 'kill-ring-mode)
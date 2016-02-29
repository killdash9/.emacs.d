(defun better-shell-idle-p (buf)
  "Returns true if the shell is not running something"
  (= (string-to-number
      (shell-command-to-string
       (concat "pstree "
               (number-to-string (process-id (get-buffer-process buf)))
               "|wc -l"))) 1))

(defun better-shell-shells ()
  "Return a list of shells"
  (remove-if-not
   (lambda (buf)
     (with-current-buffer buf
       (string-equal major-mode 'shell-mode)))
   (buffer-list)))

(defun better-shell-idle-shells ()
  "Returns all the idle shells"
  (let ((current-buffer (current-buffer)))
    (remove-if-not
     (lambda (buf)
       (with-current-buffer buf
         (and
          (better-shell-idle-p buf)
          (not (eq current-buffer buf)))))
     (better-shell-shells))))

(defun better-shell-default-directory (buf)
  "Returns the default directory for the buffer"
  (with-current-buffer buf
    default-directory))

(defun better-shell-for-current-dir ()
  (interactive)
  (let* ((dir default-directory)
         (idle-shell
          (or (car (sort (better-shell-idle-shells)
                         (lambda (s1 s2)
                           (string-equal dir (better-shell-default-directory s1)))))
              ;; make a new shell if there are none
              (shell (generate-new-buffer-name "*shell*")))))

    ;; cd in the shell if needed
    (when (not (string-equal dir (better-shell-default-directory idle-shell)))
      (with-current-buffer idle-shell
        (comint-delete-input)
        (goto-char (point-max))
        (insert (concat "cd " dir))
        (comint-send-input)))
    
    (pop-to-buffer idle-shell)))

(defun better-shell-existing-shell (&optional pop-to-buffer)
  "Next existing shell in the stack"
  (interactive)
  ;; rotate through existing shells
  (let* ((shells (better-shell-shells))
         (buf (nth (mod (+ (or (cl-position (current-buffer) shells) -1) 1)
                        (length shells)) shells)))
    (if pop-to-buffer
        (pop-to-buffer buf nil t)
      (switch-to-buffer buf t))
    (set-transient-map                ; Read next key
     `(keymap (,(elt (this-command-keys-vector) 0) . better-shell-existing-shell)) 
     t (lambda () (switch-to-buffer (current-buffer))))))

(defun better-shell-shell (&optional arg)
  "Switches to next shell in stack.
When called with a prefix arg, finds or creates a shell in the current directory."
  (interactive "p")
  (let ((shells (better-shell-shells)))
    (if (or (null shells) (and arg (= 4 arg)))
        (better-shell-for-current-dir)
      (better-shell-existing-shell t))))

(provide 'better-shell)
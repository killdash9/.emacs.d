(defun random-edit ()
  (interactive)
  (when (equal mode-name "Zone")
    ;; when being invoked as zone, pull in the entire buffer
    ;; and set to the appropriate mode
    (erase-buffer)
    (insert (zone-orig (buffer-substring (point-min)
                                         (min (point-max) (+ (point-min) 100000)))))
    (set-window-start (selected-window)
                      (let ((zone-buf (current-buffer)))
                        (zone-orig
                         (switch-to-buffer (current-buffer))
                         (let ((ws (window-start)))
                           (switch-to-buffer zone-buf) ws))))
    (goto-char (zone-orig (point)))
    (when (zone-orig (derived-mode-p 'prog-mode))
      (funcall (zone-orig major-mode))))
  
  (save-excursion
    (save-selected-window
      (push-mark)
      (while
          (progn
            (random-edit-code)
            (apply (lambda (operation maxtimes)
                     (loop repeat (1+ (random maxtimes))
                           always
                           (let ((p (point)))
                             (ignore-errors (apply operation nil))
                             (sit-for (+ .1 (/ (log (abs (- p (point)))) 10))))))
                   (case (random 12)
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
                                        ;(12 '(switch-to-random-buffer 1))
                     (t '(ignore 1))
                     )))
        ))))

(defun random-edit-type-at (point)
  (let* ((face (get-text-property point 'face))
         (ppss (save-excursion (syntax-ppss point)))
         (syntax (syntax-after point))
         (syntax-class (syntax-class syntax))
         (string (nth 3 ppss))
         (comment (or (= 11 syntax-class) (nth 4 ppss) (or (random-edit-faces-substring face "comment")
                                                           (random-edit-faces-substring face "-doc-face")))))
    (list face string comment )))

(defun random-edit-thing-at (point)
  (ignore-errors
   (save-excursion
     (goto-char point)
     (let* ((type (random-edit-type-at point))
            (beg point)
            (end point)
            text)
       (if (member (char-after point) '(9 10 13 32))
           nil ;; nothing on whitespace
         (cond
          ((nth 1 type) ;; string
           (progn
             (while (and (nth 1 (random-edit-type-at beg))
                         (>= beg (point-min)))
               (setq beg (1- beg)))
             (while (and (nth 1 (random-edit-type-at end))
                         (<= end (point-max)))
               (setq end (1+ end)))
             (setq beg (1+ beg)))
           )
          ((nth 2 type) ;; comment
           (progn
             (while (and (nth 2 (random-edit-type-at beg))
                         (>= beg (point-min)))
               (setq beg (1- beg)))
             (while (and (nth 2 (random-edit-type-at end))
                         (<= end (point-max)))
               (setq end (1+ end)))
             (setq beg (1+ beg)))
           )
          (t                            ; other
          
           (forward-sexp)
           (setq end (point))
           (backward-sexp)
           (setq beg (point))

           ))
         (if (or
              (> (- end beg) 500)       ; no really long setions
              (not (and (>= point beg) (<= point end)))) ; point not in bounds
             nil
           (let* ((s (buffer-substring beg end ))
                  (c (cond
                      ((string-match-p "^[^a-zA-Z]" s) "punct")
                      ((s-uppercase? s) "UPPER")
                      ((s-lowercase? s) "lower")
                      ((s-uppercase? (substring s 0 1)) "Upper")
                      (t "camelCase"))))
            
             (list type beg end s c point))))
       )))
  )

(defun random-edit-faces-substring (face s)
  (when face
    (when (not (listp face))
      (setq face (list face))
      (cl-find-if
       (lambda (f) (s-contains? s (symbol-name f)))
       face)
      
      )))

(defun random-edit-code ()
  (let ((speed 1)
        (thing (random-edit-thing-at (point))))
    (when thing
      (destructuring-bind (type beg end string case orig-point) thing
        (when (not (random-edit-faces-substring (car type) "keyword"))
          (let* (replacement)

            
            (do ((point (random (point-max)) (max (point-min) (mod (1+ point ) (point-max))))
                 (i 0 (1+ i)))
                ((or (> i (buffer-size) )
                     (and (equal type (random-edit-type-at point))
                          (setq replacement (random-edit-thing-at point))
                          (equal case (nth 4 replacement ))
                          (not (equal string (nth 3 replacement)))
                          (not (random-edit-faces-substring (random-edit-type-at (point)) "keyword"))
                          )))
              (setq replacement nil))

            (when replacement
              (let ((r-string (nth 3 replacement)))
                ;;                                        (message "%s -> %s" thing replacement)
                (case (random 4)
                  (0
                   (goto-char beg)
                   (loop
                    for c across string
                    do (delete-char 1)
                    do (sit-for (random-edit-random-hundredths .04 .07 speed ))
                    ))
                  (1
                   (goto-char end)
                   (loop
                    for c across string
                    do (delete-char -1)
                    do (sit-for (random-edit-random-hundredths .04 .07 speed ))
                    ))
                  (2
                   (goto-char beg)
                   (delete-char (length string)))
                  (3
                   (loop
                    initially (goto-char beg)
                    with remaining = (- end beg) and next-end
                    do
                    (setq next-end
                          (save-excursion (forward-word) (point)))
                    (let ((to-delete (min remaining (- next-end (point)))))
                      (delete-char to-delete)
                      (decf remaining to-delete))
                    (sit-for (random-edit-random-hundredths .20 .25 speed))
                    while (> remaining 0)
                    )
                   )
                  )
                (sit-for (random-edit-random-hundredths 0 3 speed))
                (dotimes (i (length r-string))
                  (insert (substring r-string i (1+ i)))
                  (sit-for
                   (if (eq (elt r-string i) ?\n)
                       (random-edit-random-hundredths .08 1.5 speed)
                     (random-edit-random-hundredths .08 .2 speed)))
                  )
                (sit-for (random-edit-random-hundredths 1 4 speed))
                )
              )
            ))))))

(defun random-edit-random-hundredths (start end speed)
  (let ((s (* 100 start))
        (e (* 100 end)))
    (/ (/ (+ s (random (round (- e s)))) 100) speed)))

(provide 'random-edit)
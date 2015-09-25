(defvar stb-board '(1 2 3 4 5 6 7 8 9))
(defvar stb-roll nil)

(defun stb-show-board ()
  (pop-to-buffer (get-buffer-create "*shutthebox*"))
  (read-only-mode 1)
  (buffer-disable-undo)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (mapconcat (lambda (n) (number-to-string n)) stb-board " "))
    (insert (concat "\n\nRoll: " (number-to-string stb-roll)))))

(defun stb-strategy-manual ()
  (read))

(defun stb-strategy-first ()
  (car (stb-get-possible-moves)))

(defun stb-strategy-highest ()
  (car (sort (stb-get-possible-moves) (lambda (x y) (> (apply 'max x) (apply 'max y))))))

(defun stb-strategy-lowest ()
  (car (sort (stb-get-possible-moves) (lambda (x y) (< (apply 'max x) (apply 'max y))))))

(defun stb-strategy-maximize-options ()
  (car (sort (stb-get-possible-moves) (lambda (x y) (>
                                                (stb-options-if-move stb-board x)
                                                (stb-options-if-move stb-board y))))))

(defun stb-strategy-minimize-options ()
  (car (sort (stb-get-possible-moves) (lambda (x y) (<
                                                (stb-options-if-move stb-board x)
                                                (stb-options-if-move stb-board y))))))

(defun stb-strategy-maximize-probability ()
  (car (sort (stb-get-possible-moves) (lambda (x y) (>
                                                (stb-options-if-move stb-board x)
                                                (stb-options-if-move stb-board y))))))

(defun stb-options-if-move (board move)
  (stb-option-count (stb-board-if-move board move)))

(defun stb-board-if-move (board move)
  (sort (cl-set-difference board move) '<))

(defun stb-option-count (board)
  (length (remove-if-not (lambda (roll) (stb-get-possible-moves-for roll board) ) '(2 3 4 5 6 7 8 9 10 11 12))))

(defun stb-probability-if-move (board move)
  (stb-probability (stb-board-if-move board move)))

(defun stb-probability (board)
  (apply '+ (mapcar
             'stb-roll-probability
             (remove-if-not (lambda (roll) (stb-get-possible-moves-for roll board) ) '(2 3 4 5 6 7 8 9 10 11 12)))))

(defun stb-roll-probability (roll)
  (plist-get
   '(
     2 2.78
       3 5.56
       4 8.33
       5 11.11
       6 13.89
       7 16.67
       8 13.89
       9 11.11
       10 8.33
       11 5.56
       12 2.78
       )
   roll))

(defvar stb-all-strategies
  '(
    ;;stb-strategy-first
    stb-strategy-highest
    ;;stb-strategy-lowest
    stb-strategy-maximize-options
    ;;stb-strategy-minimize-options
    stb-strategy-maximize-probability
    ))

(defun stb-test-all-strategies (times )
  (mapcar (lambda (x) (let ((score (stb-test-strategy x times))) (cons x (/ score times)))) stb-all-strategies))

(defun stb-test-strategy (strategy times)
  (loop repeat times
        sum (stb-play strategy)))


(defun stb-play (strategy)
  (interactive (list 'stb-strategy-manual))
  (setq stb-board '(1 2 3 4 5 6 7 8 9))
  (stb-dice-roll)
  (when (eq strategy 'stb-strategy-manual) (stb-show-board))
  (while (stb-has-move)
    (when (eq strategy 'stb-strategy-manual) (stb-suggest-moves))
    (stb-make-move (stb-get-valid-move strategy))
    (stb-dice-roll)
    (when (eq strategy 'stb-strategy-manual) (stb-show-board))(stb-show-board))
  (when (eq strategy 'stb-strategy-manual) (stb-end-game))
  '(message "%s %s" strategy (stb-get-score))
  (stb-get-score))

(defun stb-suggest-moves ()
  (message "%s" (mapcar (lambda (strategy) (cons strategy (funcall strategy))) stb-all-strategies)))

(defun stb-end-game ()
  '(message "Game over."))

(defun stb-get-score ()
  (string-to-number (mapconcat (lambda (x) (number-to-string x)) stb-board "")))

(defun stb-make-move (move)
  (setq stb-board (stb-board-if-move stb-board move)))

(defun stb-get-valid-move (strategy)
  (let ((move (sort (funcall strategy) '<)))
    (while (not (member move (stb-get-possible-moves)))
      (message "Invalid move")
      (setq move (sort (funcall strategy) '<)))
    move
    ))

(defun stb-has-move ()
  (stb-get-possible-moves))

(defun stb-get-possible-moves ()
  (stb-get-possible-moves-for stb-roll stb-board))

(defun stb-get-possible-moves-for (roll board)
  (remove-if-not (lambda (x) (= roll (apply '+ x))) (stb-get-all-moves board)))

(defun stb-get-all-moves (board)
  (when board
    (let ((moves (stb-get-all-moves (cdr board))))
      (append
       (list (list (car board)))
       (mapcar (lambda (x) (cons (car board) x)) moves)
       moves))))

(defun stb-dice-roll ()
  (setq stb-roll (+ (stb-get-die-roll) (stb-get-die-roll))))

(defun stb-get-die-roll ()
  (+ 1 (random 6)))
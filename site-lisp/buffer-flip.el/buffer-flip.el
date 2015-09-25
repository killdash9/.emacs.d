(require 'key-chord)

(defvar buffer-flip-mode-map '(keymap))

(define-minor-mode buffer-flip-mode
  "Lets you flip through buffers \"ALT-TAB in Windows\" style,
keeping the most recently used buffers on the top of the stack.
By default, the key sequence to flip through buffers is \"u8\".
Pressing \"u\" is like holding down ALT.  Pressing 8 is like
pressing tab.  To begin cycling through the buffers, press u and
then 8 in rapid succession.  Repeatedly pressing 8 will switch
through the buffer stack returned by `buffer-list'.  Pressing
* (shift-8) will cycle in the opposite direction.  SPC will bring
you back to buffer you were in before cycling.  Pressing \"u8\"
mid-cycling will place the current buffer on the top of the stack
and restart the cycling.  Once you begin doing something in a
buffer you have flipped to, it becomes the top buffer in the
stack.  So, pressing \"u8\" repeatedly will toggle between the
top two buffers.  Pressing \"u88\" will go to the third buffer
from the top of the stack, and so on.  You can customize these
keys with `buffer-flip-key-sequence'"
  :global t :keymap buffer-flip-mode-map
  (when buffer-flip-mode
    (unless key-chord-mode
      (when (yes-or-no-p "key-chord-mode must be enabled for buffer-flip-mode to work.  Enable key-chord-mode?")
        (key-chord-mode 1)))))

(defun buffer-flip-set-keys (symbol value)
  (when (not (and (= 3 (length value)) (or (stringp value))))
    (user-error "This must be a three character string"))
  (set-default symbol value)
  (setcdr buffer-flip-mode-map nil) ;; empty the mode map to clear out previous bindings
  (key-chord-define buffer-flip-mode-map (substring value 0 2) 'buffer-flip))

(defcustom buffer-flip-key-sequence "u8*"
  "This is the key sequence to invoke buffer flipping.  The first
character functions as the ALT in ALT-TAB, and the second
character functions as the TAB.  The third character functions as
SHIFT-TAB.  This would typically be the shifted version of the
second character.  These may not be modifier keys, and because of
a restriction in key-chord, these must be characters between 32
and 126.  Choose a key combination not likely to be used in
succession in normal editing."
  :set 'buffer-flip-set-keys :type '(string))

(defun buffer-flip ()
  "Begins the process of flipping through buffers.
See `buffer-flip-mode' for more information."
  (interactive)
  (lexical-let*
      ((buffer-list (buffer-list))
       (index 0)
       (cycle
        (lambda () (interactive)
          ;; switch to the next non-minibuffer buffer that is not in another window
          (loop with buf
                do (setq index
                         (mod (+ index
                                 (if (find (elt buffer-flip-key-sequence 2)
                                           (this-command-keys-vector)) -1 1))
                              (length buffer-list)))
                do (setq buf (nth index buffer-list))
                while (or (get-buffer-window buf) (minibufferp buf))
                finally (switch-to-buffer buf t)))))
    (funcall cycle)
    (set-transient-map `(keymap (,(elt buffer-flip-key-sequence 1) . ,cycle)
                                (,(elt buffer-flip-key-sequence 2) . ,cycle)
                                (32 . ,(lambda () (interactive) (switch-to-buffer (car buffer-list))))
                                )
                       t (lambda () (switch-to-buffer (current-buffer))))))

(provide 'buffer-flip)
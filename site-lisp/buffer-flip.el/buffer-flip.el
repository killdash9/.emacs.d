(require 'key-chord)

(defvar buffer-flip-mode-map (make-sparse-keymap))

(define-minor-mode buffer-flip-mode
  "Lets you flip through buffers \"ALT-TAB in Windows\" style, keeping the most
recently used buffers on the top of the stack.  By default, the key sequence to 
flip through buffers is \"u8\".  Pressing \"u\" is like holding down ALT.   
Pressing 8 is like pressing tab.  To begin cycling through the buffers, press 
u and then 8 in rapid succession.  Repeatedly pressing 8 will switch through the
buffer stack returned by `buffer-list'.  Releasing \"u\" and pressing \"u8\" 
mid-cycling will place the current buffer on the top of the stack and restart the 
cycling.  Once you begin doing something in a buffer you have flipped to, it 
becomes the top buffer in the stack.  So, pressing \"u8\" repeatedly will toggle
between the top two buffers.  Pressing \"u88\" will go to the third buffer from 
the top of the stack, and so on.  You can customize u8 to some other key pair
by customizing `buffer-flip-key-sequence'"
  :global t :keymap buffer-flip-mode-map
  (when buffer-flip-mode
    (unless key-chord-mode
      (when (yes-or-no-p "key-chord-mode must also be enabled for buffer-flip-mode to work.  Enable key-chord-mode?")
        (key-chord-mode 1)))))

(defun buffer-flip-set-keys (symbol value)
  (when (not (and (= 2 (length value)) (or (stringp value)(vectorp value))))
    (user-error "This must be a two character string or vector"))
  (set-default symbol value)
  (setcdr buffer-flip-mode-map nil) ;; empty the mode map to clear out previous bindings
  (key-chord-define buffer-flip-mode-map value 'buffer-flip))

(defcustom buffer-flip-key-sequence "u8"
  "This is the key sequence to invoke buffer flipping.  The first character
functions as the ALT in ALT-TAB, and the second character functions as the 
TAB.  These may not be modifier keys, and because of a restriction in key-chord,
these must be characters between 32 and 126.  Choose a key combination not 
likely to be used in succession in normal editing."
  :set 'buffer-flip-set-keys :type '(string))

(defun buffer-flip ()
  (interactive)
  (lexical-let*
      ((buffer-list (buffer-list))
       (cycle (lambda () (interactive)
                ;; switch to the next non-minibuffer buffer that is not in another window
                (let ((buf))
                  (while (or (not buf) (get-buffer-window buf) (minibufferp buf))
                    (setq buf (car buffer-list))
                    (setq buffer-list (append (cdr buffer-list) (list buf))))
                  (switch-to-buffer buf t)))))
    (funcall cycle)
    (set-transient-map `(keymap (,(elt buffer-flip-key-sequence 1) . ,cycle))
                       t (lambda () (switch-to-buffer (current-buffer))))))

(provide 'buffer-flip)
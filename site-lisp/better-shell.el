;;; better-shell.el --- Better shell management
;; Copyright (C) 2015 Russell Black

;; Author: Russell Black (killdash9@github)
;; Keywords: convenience
;; URL: https://github.com/killdash9/better-shell.el
;; Created: 1st Mar 2016
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Quickly pop to a shell, optionally place it in the current
;; directory, reusing shells where possible.

;;; Code:
(eval-when-compile (require 'cl))
(require 'comint)
(defun better-shell-idle-p (buf)
  "Return t if the shell in BUF is not running something.
Since we can't tell if a remote shell is running something,
return nil for all remote shells."
  (and
   (not (with-current-buffer buf
          (file-remote-p default-directory)))
   (= (string-to-number
       (shell-command-to-string
        (concat "pstree "
                (number-to-string (process-id (get-buffer-process buf)))
                "|wc -l"))) 1)))

(defun better-shell-shells ()
  "Return a list of buffers running shells."
  (cl-remove-if-not
   (lambda (buf)
     (and
      (get-buffer-process buf)
      (with-current-buffer buf
        (string-equal major-mode 'shell-mode))))
   (buffer-list)))

(defun better-shell-idle-shells (remote-host)
  "Return all the buffers with idle shells on REMOTE-HOST.
If REMOTE-HOST is nil, returns a list of non-remote shells."
  (let ((current-buffer (current-buffer)))
    (cl-remove-if-not
     (lambda (buf)
       (with-current-buffer buf
         (and
          (string-equal (file-remote-p default-directory) remote-host)
          (better-shell-idle-p buf)
          (not (eq current-buffer buf)))))
     (better-shell-shells))))

(defun better-shell-default-directory (buf)
  "Return the default directory for BUF."
  (with-current-buffer buf
    default-directory))

(defun better-shell-for-current-dir ()
  "Find or create a shell for the current directory."
  (let* ((dir default-directory)
         (idle-shell
          (or (car (sort (better-shell-idle-shells
                          (file-remote-p default-directory))
                         (lambda (s1 s2)
                           (string-equal dir (better-shell-default-directory s1)))))
              ;; make a new shell if there are none
              (shell (generate-new-buffer-name (if (file-remote-p dir) (format "*shell%s*" (file-remote-p dir)) "*shell*"))))))

    ;; cd in the shell if needed
    (when (not (string-equal dir (better-shell-default-directory idle-shell)))
      (with-current-buffer idle-shell
        (comint-delete-input)
        (goto-char (point-max))
        (insert (concat "cd " dir))
        (comint-send-input)))
    
    (pop-to-buffer idle-shell)))

(defun better-shell-existing-shell (&optional pop-to-buffer)
  "Next existing shell in the stack.
If POP-TO-BUFFER is non-nil, pop to the shell.  Otherwise, switch
to it."
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
  "Pop to an appropriate shell.
When called with a prefix ARG, finds or creates a shell in the
current directory.  Otherwise, pop to the most recently used
shell."
  (interactive "p")
  (let ((shells (better-shell-shells)))
    (if (or (null shells) (and arg (= 4 arg)))
        (better-shell-for-current-dir)
      (better-shell-existing-shell t))))

(provide 'better-shell)
;;; better-shell ends here

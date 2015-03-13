(defun my-replace-regexp (s r)
  (while (re-search-forward s nil t)
    (replace-match r nil nil)))


(defun get-scribd-text (scribd-url)
  (switch-to-buffer
   (url-retrieve-synchronously scribd-url))
  (rename-buffer "*scribd file*" t)
  (beginning-of-buffer)
  (search-forward "docManager.assetPrefix = \"")
  (forward-char)
  (let ((asset-prefix (thing-at-point 'word)))
    (beginning-of-buffer)
    (let ((last (point)))
      (while (search-forward (concat "/" asset-prefix "/pages/") nil t)
        (search-backward "\"")
        (forward-char)
        (kill-region last (point))
        (search-forward "\"")
        (backward-char)
        (insert "\n")
        (setq last (point)))
      (end-of-buffer)
      (kill-region last (point))
      (beginning-of-buffer)
      (let ((url))
        (while 
            (setq url (thing-at-point 'filename))
          (message url)
          (kill-line)
          (insert 
           (with-current-buffer
               (url-retrieve-synchronously url)
             (beginning-of-buffer)
             (search-forward "([\"")
             (delete-region 1 (point))
             (end-of-buffer)
             (delete-backward-char 4)
             (let ((s (buffer-string)))
               (kill-this-buffer)
               s)
             ))
          (forward-char)))
      (beginning-of-buffer)
      (my-replace-regexp "<span class=a" "\n<span class=a")
      (beginning-of-buffer)
      (my-replace-regexp "\\\\\"" "\"")
      (beginning-of-buffer)
      (my-replace-regexp "<[^>]*>" "")
      (beginning-of-buffer)
      (my-replace-regexp "\\\\n" "\n")
      (beginning-of-buffer)
      (my-replace-regexp "\n\n\n*" "\n\n")
      (beginning-of-buffer)
      (my-replace-regexp "&nbsp;" " ")
      (beginning-of-buffer)
      (my-replace-regexp "&gt;" ">")
      (beginning-of-buffer)
      (my-replace-regexp "&amp;" "&")
      (beginning-of-buffer)
      (my-replace-regexp "&lt;" "<")
      (beginning-of-buffer)
      )))

(get-scribd-text "http://www.scribd.com/doc/56303459/Sabrix-SAPSabrixTechnicalTraining")
;;  $('.absimg').css('opacity',1);$('.page-blur-promo').detach();$('.text_layer').css('color','black').css('text_shadow')
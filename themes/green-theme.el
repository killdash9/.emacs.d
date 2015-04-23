(deftheme green "Green Theme")

(custom-theme-set-faces
 'green
 '(default ((t ( :background "black" :foreground "#0A0"  :height 120 :family "Monaco" ))))
 '(variable-pitch ((t ( :height 160 :family "Papyrus"))))
 '(cursor ((t ( :background "#0A0" ))))
 '(fringe ((nil nil)))
 '(widget-field ((nil nil)))
 `(mode-line ((nil (:box "#0A0" :inverse-video t))))
 `(mode-line-inactive ((nil (:box "#0A0")))))

(provide-theme 'green)
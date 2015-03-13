(deftheme papyrus "Papyrus Theme")

(custom-theme-set-faces
 'papyrus
 '(default ((t ( :background "#EEDFA6" :foreground "black"  :height 120 :family "Monaco" ))))
 '(variable-pitch ((t ( :height 160 :family "Papyrus"))))
 '(cursor ((t ( :background "#000000" ))))
 '(fringe ((nil nil)))
 `(mode-line ((nil (:box "black" :inverse-video t))))
 `(mode-line-inactive ((nil (:box "black")))))

(provide-theme 'papyrus)
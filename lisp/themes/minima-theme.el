(deftheme minima "A clean, dark theme optimized for extended coding sessions with high contrast and minimal distractions.")

(let ((background "#181A1F")
      (foreground "#E2E2E3")
      (cursor-color "#FC5D7C")
      (comment-color "#7F8490")
      (keyword-color "#FC5D7C")
      (builtin-color "#FC5D7C")
      (string-color "#E7C664")
      (type-color "#76CCE0")
      (constant-color "#FC5D7C")
      (variable-color "#9CDCFE")
      (function-name-color "#B39DF3")
      (region-color "#363D4C")
      (warning-color "#E7C664")
      (mode-line-bg "#181A1F")
      (mode-line-fg "#e2e2e5")
      (mode-line-inactive-bg "#181A1F")
      (mode-line-inactive-fg "#7f7f7f"))

  (custom-theme-set-faces
   'minima
   ;; Background and foreground
   `(default ((t (:foreground ,foreground :background ,background))))
   `(cursor ((t (:background ,cursor-color))))

   ;; Highlighting faces
   `(highlight ((t (:background ,mode-line-bg))))
   `(region ((t (:background ,mode-line-bg :foreground ,foreground))))

   ;; Font lock faces for syntax highlighting
   `(font-lock-builtin-face ((t (:foreground ,builtin-color))))
   `(font-lock-comment-face ((t (:foreground ,comment-color :italic t))))
   `(font-lock-negation-char-face ((t (:foreground ,keyword-color))))
   `(font-lock-reference-face ((t (:foreground ,builtin-color))))
   `(font-lock-constant-face ((t (:foreground ,constant-color))))
   `(font-lock-doc-face ((t (:foreground ,comment-color))))
   `(font-lock-function-name-face ((t (:foreground ,function-name-color))))
   `(font-lock-keyword-face ((t (:foreground ,keyword-color :bold t))))
   `(font-lock-string-face ((t (:foreground ,string-color))))
   `(font-lock-type-face ((t (:foreground ,type-color))))
   `(font-lock-variable-name-face ((t (:foreground ,variable-color))))
   `(font-lock-warning-face ((t (:foreground ,warning-color :bold t))))
   
   ;; UI Elements
   `(mode-line ((t (:foreground ,mode-line-fg :background ,mode-line-bg :box (:line-width -1 :style released-button)))))
   `(mode-line-buffer-id ((t (:bold t :foreground ,foreground))))
   `(mode-line-inactive ((t (:background ,mode-line-inactive-bg :foreground ,mode-line-inactive-fg))))
   `(vertical-border ((t (:foreground ,mode-line-bg))))
   `(minibuffer-prompt ((t (:bold t :foreground ,keyword-color))))
   `(link ((t (:underline t :foreground ,type-color))))
  )
)

(provide-theme 'minima)

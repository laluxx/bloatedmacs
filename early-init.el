;; i do manual garbage collection for speed
(setq gc-cons-threshold most-positive-fixnum)
(defvar emacs-start-time (current-time))

(setq package-enable-at-startup nil)

;; emacs --shut-the-fuck-up || -stfu
(defun display-startup-echo-area-message ()
  (message ""))
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)


;; TODO Load the last theme and change this color when exiting emacs
;; (add-to-list 'default-frame-alist '(background-color . "#212121"))
(add-to-list 'default-frame-alist '(background-color . "#171717"))


;; TODO Font manager 'fontaine'
(set-face-attribute 'default nil
                    :family "JetBrains Mono Nerd Font"
                    :weight 'extrabold
                    :height 110)

;; (set-face-attribute 'default nil
;;                     :family "JetBrains Mono"
;;                     :weight 'medium
;;                     :height 110)

;; (set-face-attribute 'default nil
;;                     :family "Fantasque SansM NerdFont"
;;                     :weight 'bold
;;                     :height 130)


(setq frame-resize-pixelwise t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

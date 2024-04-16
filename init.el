;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; TODO C-S-n and C-S-p should behave like visual-line mode in vim
;; TODO pressing 'c' when a region is active should 'comment-region'

;; KEYBINDS
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(defun laluxx/insert-or-evaluate-region ()
  "Insert the character 'e' if no active region, otherwise evaluate the region."
  (interactive)
  (if (use-region-p)
      (call-interactively 'eval-region)
    (insert "e")))

(global-set-key (kbd "e") 'laluxx/insert-or-evaluate-region)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; UI
;; Disable confirmation for loading themes
(setq custom-safe-themes t)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; (doom-themes-visual-bell-config)
    ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

 (load-theme 'doom-one t)

(use-package kaolin-themes
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package hl-todo
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))
    
(use-package olivetti
  :ensure t)


;; COMPLETION
(savehist-mode)

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :ensure t
  :init
  (all-the-icons-completion-mode))


(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin
  
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :ensure t
  :init
  (global-corfu-mode))

;; (setq-local corfu-auto        t
;;             corfu-auto-delay  0 ;; TOO SMALL - NOT RECOMMENDED
;;             corfu-auto-prefix 1 ;; TOO SMALL - NOT RECOMMENDED
;;             completion-styles '(basic))
;; (use-package corfu-map)

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-blend-background t)
  (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(use-package consult
  :ensure t)

(use-package which-key
  :ensure t
  :init
  (which-key-mode))


(use-package helpful
  :ensure t
  :init
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command))


;; EDITING
(setq-default truncate-lines t)
(electric-pair-mode)

(defun laluxx/move-to-beginning-if-shorter-than-column (original-func &rest args)
  "Move cursor to the beginning of the line if the new line after executing ORIGINAL-FUNC is shorter than the current column position."
  (let ((current-column (current-column)))  ; Store the current horizontal position
    ;; Execute the original function to move the cursor (next-line or previous-line).
    (apply original-func args)
    ;; After moving, check the length of the new line.
    (when (derived-mode-p 'text-mode 'prog-mode)
      (let ((new-length (- (line-end-position) (line-beginning-position))))
        ;; If the new line is shorter than the current column position, move the cursor to the beginning of that line.
        (when (< new-length current-column)
          (beginning-of-line)
          (setq this-command 'beginning-of-line))))))  ; Force Emacs to reset the desired column

(advice-add 'next-line :around #'laluxx/move-to-beginning-if-shorter-than-column)
(advice-add 'previous-line :around #'laluxx/move-to-beginning-if-shorter-than-column)



;; WINDOW MANAGMENT
(defun swap-with-next-window ()
  "Swap the current window with the next one."
  (interactive)
  (let* ((current (selected-window))
         (next (next-window))
         (current-buf (window-buffer current))
         (next-buf (window-buffer next))
         (current-pos (window-start current))
         (next-pos (window-start next)))
    (unless (eq current next)
      (set-window-buffer current next-buf)
      (set-window-buffer next current-buf)
      (set-window-start current next-pos)
      (set-window-start next current-pos)
      (select-window next))))

(defun swap-with-prev-window ()
  "Swap the current window with the previous one."
  (interactive)
  (let* ((current (selected-window))
         (prev (previous-window))
         (current-buf (window-buffer current))
         (prev-buf (window-buffer prev))
         (current-pos (window-start current))
         (prev-pos (window-start prev)))
    (unless (eq current prev)
      (set-window-buffer current prev-buf)
      (set-window-buffer prev current-buf)
      (set-window-start current prev-pos)
      (set-window-start prev current-pos)
      (select-window prev))))


(defun smart-enlarge-window-horizontally ()
  "Enlarge the window horizontally in a direction dependent on its position."
  (interactive)
  (if (window-at-side-p (selected-window) 'right)
      (shrink-window-horizontally 5)
    (enlarge-window-horizontally 5)))

;; TODO if there is only one window call mark-paragraph instead
(defun smart-shrink-window-horizontally ()
  "Shrink the window horizontally in a direction dependent on its position."
  (interactive)
  (if (window-at-side-p (selected-window) 'right)
      (enlarge-window-horizontally 5)
    (shrink-window-horizontally 5)))

(global-set-key (kbd "M-j") 'other-window)
(global-set-key (kbd "M-k") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "M-q") 'delete-window)
(global-set-key (kbd "M-J") 'swap-with-next-window)
(global-set-key (kbd "M-K") 'swap-with-prev-window)
(global-set-key (kbd "M-l") 'smart-enlarge-window-horizontally)
(global-set-key (kbd "M-h") 'smart-shrink-window-horizontally)




(find-file "~/.config/emacs/early-init.el")
(split-window-right)
(find-file "~/.config/emacs/init.el")


(run-with-idle-timer
 1 nil
 (lambda ()
   (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
     (message "Emacs started in %.2fs" (- elapsed 1)))))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(kind-icon olivetti helpful which-key all-the-icons-completion marginalia consult kaolin-themes moody rainbow-delimiters smarteparens smart-parens vertico doom-themes))
 '(safe-local-variable-values '((git-commit-major-mode . git-commit-elisp-text-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

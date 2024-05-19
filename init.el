;; (profiler-start 'cpu)

;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (profiler-stop)
;;             (profiler-report)))

;; TODO customize the non focused modeline 
;; TODO 'C-S-s'


;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; TODO 'C-backspace' should work with came case like ded

;; TODO 'popper' make popup take just enough space for the text to fit
;; TODO 'C-S-n' and 'C-S-p' should behave like visual-line mode in vim and always go to the beginning of the line
;; TODO Remember the last mode the 'scratch-buffer' was is and load it at startup
;; TODO Remember the last theme used, and load it at startup also extract the 'bg' color for the 'early-ini.el'
;; TODO 'elisp-outline-mode' make it automatically collapse heading when entering, make ;;; heading work and make heading look good
;; TODO 'C-h-e' should toggle


(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
        '(("http://nullprogram.com/feed/" blog emacs)
          "http://www.50ply.com/atom.xml"  ; no autotagging
          ("http://nedroid.com/feed/" webcomic))))


;;;; KEYBINDS

;; LSP
(global-set-key (kbd "C-S-K") 'lsp-ui-doc-show)
(global-set-key (kbd "C-c h") 'consult-flycheck)

;; TODO if point is ontop of an image
;; call instead image-mouse-decrease-size
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-M-j") 'eval-last-sexp)
(global-set-key (kbd "C-c p") 'beginning-of-buffer)
(global-set-key (kbd "C-c n") 'end-of-buffer)
(global-set-key (kbd "C-c C-d") 'helpful-at-point)

(global-set-key (kbd "C-x C-b") 'kill-buffer)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-h i") 'info-display-manual)

(global-set-key (kbd "C-x w c") 'delete-window)
(global-set-key (kbd "C-x w w") 'other-window)
(global-set-key (kbd "C-x w s") 'split-window-below)
(global-set-key (kbd "C-x w v") 'split-window-right)

(global-set-key (kbd "C-h t") 'laluxx/consult-dark-themes)
(global-set-key (kbd "C-h F") 'describe-face)
(global-set-key (kbd "C-h V") 'set-variable)

(global-set-key (kbd "C-x C-S-j") 'dired-jump-other-window)
(global-set-key (kbd "C-x c") 'compile)

(global-set-key (kbd "C-S-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "C-S-d") 'kill-word)
(global-set-key (kbd "C-e") 'laluxx/mwim-end)
(global-set-key (kbd "C-a") 'laluxx/mwim-beginning)
(global-set-key (kbd "C-S-y") 'laluxx/copy-line)
(global-set-key (kbd "C-y") 'laluxx/yank-line)
(global-set-key (kbd "C-k") 'laluxx/kill-line-or-kill-region)
(global-set-key (kbd "e") 'laluxx/insert-or-evaluate-region)
(global-set-key (kbd "y") 'laluxx/insert-or-copy-region)
(global-set-key (kbd "C-d") 'laluxx/delete-char-or-kill-region)
(global-set-key (kbd "M-n") 'laluxx/drag-down-or-forward-paragraph)
(global-set-key (kbd "M-p") 'laluxx/drag-up-or-backward-paragraph)
(global-set-key (kbd "M-N") 'laluxx/forward-paragraph-select)
(global-set-key (kbd "M-P") 'laluxx/backward-paragraph-select)
(global-set-key (kbd "M-H") 'mark-paragraph)
(global-set-key (kbd "M-O") 'laluxx/open-above)
(global-set-key (kbd "M-o") 'laluxx/open-below)

(global-set-key (kbd "C-c i") 'consult-imenu)


(use-package general
  :ensure t
  :config
  (define-prefix-command 'Find nil)
  (define-key global-map (kbd "C-x f") 'Find)

  (define-prefix-command 'Insert nil)
  (define-key global-map (kbd "C-x i") 'Insert)

  (define-prefix-command 'Search nil)
  (define-key global-map (kbd "C-x s") 'Search)

  (define-prefix-command 'Quit nil)
  (define-key global-map (kbd "C-x q") 'Quit)

  (define-prefix-command 'Theme nil)
  (define-key global-map (kbd "C-c t") 'Theme)

  (define-prefix-command 'Buffer nil)
  (define-key global-map (kbd "C-c b") 'Buffer)

  (general-define-key
   :keymaps 'Buffer
   "p" 'previous-buffer
   "n" 'next-buffer)

  (general-define-key
   :keymaps 'Quit
   "r" 'restart-emacs
   "q" 'save-buffers-kill-terminal)

  (general-define-key
   :keymaps 'Search
   "i" 'consult-imenu)

  (general-define-key
   :keymaps 'Theme
   ;; "t" 'toggle-theme-between-doom-material-dark-and-doom-badger
   "t" 'laluxx/toggle-theme-pair
   "d" 'laluxx/consult-dark-themes
   "l" 'laluxx/consult-light-themes
   "u" 'laluxx/consult-ugly-themes)

  (general-define-key
   :keymaps 'Insert
   "c" 'insert-char
   "f" 'insert-file)

  (general-define-key
   :keymaps 'Find
   "r" 'consult-recent-file
   "h" 'laluxx/find-header
   "H" 'laluxx/find-header-split
   "e" 'consult-flycheck
   "t" 'laluxx/find-todo
   "n" 'laluxx/find-note
   "p" 'laluxx/find-package-source-code
   "m" 'consult-man
   "i" 'consult-find
   "g" 'find-grep
   "G" 'consult-ripgrep
   "f" 'find-file
   "j" 'laluxx/file-jump
   "l" 'consult-line
   "c" (lambda () (interactive) (find-file "~/.config/emacs/init.el"))))


(defun laluxx/file-jump ()
  "Prompt to open a file interactively.
if not aborted,open it in a new window split vertically."
  (interactive)
  (let ((file (read-file-name "File jump: ")))
    (when file
      (split-window-right)
      (windmove-right)
      (find-file file))))


;;;; GIT
(use-package magit
  :ensure t
  :commands magit-status
  :config
  (global-set-key (kbd "C-h g") #'magit-status))


;;;; COMPILATION
(setq compilation-scroll-output t)
(setq compilation-always-kill t)



;;;; IELM
(setq ielm-header "")
(setq ielm-prompt "λ🢒 ")

(add-hook 'ielm-mode-hook (lambda ()
                            (define-key ielm-map (kbd "C-l")
                              (lambda ()
                                (interactive)
                                (recenter-top-bottom 0)))))



;; (use-package gptel
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "C-c RET") #'gptel-send)
;;   (global-set-key (kbd "C-c m") #'gptel-menu))

;; (defun simulate-keypress-sequence ()
;;   (interactive)
;;   (execute-kbd-macro (kbd "C-c m"))
;;   (sit-for 0.5)
;;   (execute-kbd-macro "m")
;;   (sit-for 0.5)
;;   (execute-kbd-macro "e")
;;   (sit-for 0.5)
;;   (execute-kbd-macro (kbd "RET")))

;; (global-set-key (kbd "C-c j") #'simulate-keypress-sequence)



;;;; PDF
;; (use-package pdf-tools
;;   :ensure t
;;   :config
;;   (pdf-tools-install)
;;   (setq pdf-view-midnight-colors '("#FFFFFF" . "#212121"))
;;   (add-hook 'pdf-view-mode-hook (lambda ()
;;                                   (pdf-view-midnight-minor-mode 1))))


;;;; UI

;; POINTER

;; TODO background and selection
(defun laluxx/pointer-color-update ()
  "Update the cursor color based on the foreground color of the character at point."
  (let* ((pos (point))
         ;; Attempt to find the face using overlays first, then text properties.
         (face (or (car (face-at-point nil t))  ; Get face from overlays/text properties.
                   'default))  ; Fallback to default if no face is found.
         (fg-color (face-attribute face :foreground nil t))
         ;; Fallback cursor color if fg is unspecified or face attribute fails.
         (cursor-color (if (or (not fg-color) (string= fg-color "unspecified"))
                           "red"
                         fg-color)))
    ;; Set the cursor color if it differs from the current one to minimize updates.
    (unless (equal cursor-color (frame-parameter nil 'cursor-color))
      (set-cursor-color cursor-color))))

(add-hook 'post-command-hook 'laluxx/pointer-color-update)


;; IMPROVE
;; (defun my/cursor-color-update ()
;;   "Update the cursor color based on the foreground color of the character at point or the region background color."
;;   (let* ((pos (point))
;;          ;; Attempt to find the face using overlays first, then text properties.
;;          (face (or (car (face-at-point nil t))  ; Get face from overlays/text properties.
;;                    'default))  ; Fallback to default if no face is found.
;;          (fg-color (face-attribute face :foreground nil t))
;;          ;; Use the background color of the 'region' face as the fallback color.
;;          (region-bg-color (face-attribute 'region :background nil t))
;;          ;; Determine cursor color, fallback to region background color if fg is unspecified.
;;          (cursor-color (if (or (not fg-color) (string= fg-color "unspecified"))
;;                            region-bg-color
;;                          fg-color)))
;;     ;; Set the cursor color if it differs from the current one to minimize updates.
;;     (unless (equal cursor-color (frame-parameter nil 'cursor-color))
;;       (set-cursor-color cursor-color))))

;; (add-hook 'post-command-hook 'my/cursor-color-update)



(defun set-minibuffer-cursor-style ()
  (setq cursor-type 'bar))

(add-hook 'minibuffer-setup-hook 'set-minibuffer-cursor-style)

(defun my/set-cursor-style ()
  "Set cursor style based on the number of lines in the selection, excluding the minibuffer."
  (unless (window-minibuffer-p)
    (setq cursor-type (if (use-region-p)
                          (if (= (line-number-at-pos (region-beginning))
                                 (line-number-at-pos (region-end)))
                              'bar  ; Selection within a single line
                            'box) ; Selection spans multiple lines
                        'box))))  ; No active selection

(add-hook 'post-command-hook 'my/set-cursor-style)




(use-package diminish
  :ensure t)

(use-package iscroll
  :ensure t)


(use-package olivetti
  :ensure t
  :config
  (global-set-key (kbd "C-<") #'olivetti-expand)
  (global-set-key (kbd "C->") #'olivetti-shrink))


;; INFO

(defun my-info-mode-setup ()
  "Set up my preferences for Info mode."
  (olivetti-mode 1)
  (olivetti-set-width 82))

(add-hook 'Info-mode-hook 'my-info-mode-setup)


(defun open-info-root-menu-and-bury ()
  "Open Info and jump directly to the root menu."
  (interactive)
  (info "dir")
  (call-interactively 'Info-menu))



;; READONLY
;; TODO change cursor color to warning and add npfb keybinds
(defun toggle-blink-cursor-based-on-read-only ()
  "Disable blinking cursor in read-only buffers."
  (if buffer-read-only
      (blink-cursor-mode 0)  ; Disable cursor blinking
    (blink-cursor-mode 1)))  ; Enable cursor blinking

(add-hook 'find-file-hook 'toggle-blink-cursor-based-on-read-only)
(add-hook 'read-only-mode-hook 'toggle-blink-cursor-based-on-read-only)


;; Optimizations
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)


(setq custom-safe-themes t)
(setq use-dialog-box nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq confirm-kill-processes nil)


;; Title
(setq frame-title-format '("Minimacs - %b")
      icon-title-format frame-title-format)

;; SCROLLING
(setq mouse-wheel-scroll-amount '(2 ((shift) . 5))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don"t accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse



;; TODO Do this only in a buffer alist '(eww)
;; (setq jit-lock-defer-time 0)
;; (pixel-scroll-mode)
;; (setq pixel-dead-time 0) ; Never go back to the old scrolling behaviour.
;; (setq pixel-resolution-fine-flag t) ; Scroll by number of pixels instead of lines (t = frame-char-height pixels).
;; (setq mouse-wheel-scroll-amount '(1)) ; Distance in pixel-resolution to scroll each mouse wheel event.
;; (setq mouse-wheel-progressive-speed nil) ; Progressive speed is too fast for me.



(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000
      auto-window-vscroll nil
      scroll-preserve-screen-position t)

(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)


;; LINE NUMBERS
(defvar laluxx/line-number-threshold 50
  "Minimum number of lines in a buffer to enable line numbers.")

(defun my-enable-line-numbers-based-on-size ()
  "Enable `display-line-numbers-mode' only if the buffer has more than `laluxx/line-number-threshold' lines."
  (when (> (count-lines (point-min) (point-max)) laluxx/line-number-threshold)
    (display-line-numbers-mode 1)))

;; Add the function to prog-mode-hook
(add-hook 'prog-mode-hook 'my-enable-line-numbers-based-on-size)


;; Make certain buffers grossly incandescent
;; TODO Per theme configuration
;; (use-package solaire-mode
;;   :ensure t
;;   :config
;;   (solaire-global-mode))


(use-package theme-magic
  :ensure t)

(use-package ligature
  :ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  (global-ligature-mode t))



;; PRETTIFY SYMBOLS
(global-prettify-symbols-mode 1)

;; (defun prettify-set ()
;;   (setq prettify-symbols-alist
;;         '(("lambda" . "λ")
;;           ("|>"     . "▶") ;; ▷
;;           ("<|"     . "◀") ;; ◁
;;           ("<="     . "≤")
;;           (">="     . "≥")
;;           ;; ("->>"    . "↠")
;;           ;; ("->"     . "→")
;;           ;; ("<-"     . "←")
;;           ;; ("=>"     . "⇒")
;;           )))
;; (add-hook 'prog-mode-hook 'prettify-set)




;; Pulse current line
;; (use-package pulse
;;   :ensure t
;;   :custom-face
;;   (pulse-highlight-start-face ((t (:inherit region :background unspecified))))
;;   (pulse-highlight-face ((t (:inherit region :background unspecified :extend t))))
;;   :hook (((dumb-jump-after-jump imenu-after-jump) . my-recenter-and-pulse)
;;          ((bookmark-after-jump magit-diff-visit-file next-error) . my-recenter-and-pulse-line))
;;   :init
;;   (with-no-warnings
;;     (defun my-pulse-momentary-line (&rest _)
;;       "Pulse the current line."
;;       (pulse-momentary-highlight-one-line (point)))

;;     (defun my-pulse-momentary (&rest _)
;;       "Pulse the region or the current line."
;;       (if (fboundp 'xref-pulse-momentarily)
;;           (xref-pulse-momentarily)
;;         (my-pulse-momentary-line)))

;;     (defun my-recenter-and-pulse(&rest _)
;;       "Recenter and pulse the region or the current line."
;;       (recenter)
;;       (my-pulse-momentary))

;;     (defun my-recenter-and-pulse-line (&rest _)
;;       "Recenter and pulse the current line."
;;       (recenter)
;;       (my-pulse-momentary-line))

;;     (dolist (cmd '(recenter-top-bottom
;;                    other-window switch-to-buffer
;;                    aw-select toggle-window-split
;;                    windmove-do-window-select
;;                    pager-page-down pager-page-up
;;                    treemacs-select-window
;;                    symbol-overlay-basic-jump))
;;       (advice-add cmd :after #'my-pulse-momentary-line))

;;     (dolist (cmd '(pop-to-mark-command
;;                    pop-global-mark
;;                    goto-last-change))
;;       (advice-add cmd :after #'my-recenter-and-pulse))))

;; Pulse modified region
;; (use-package goggles
;;   :ensure t
;;   :diminish
;;   :hook ((prog-mode text-mode) . goggles-mode))



(use-package rainbow-mode
  :diminish
  :ensure t
  :diminish
  :hook org-mode prog-mode)


(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))



(use-package ewal
  :ensure t
  :init
  (setq ewal-use-built-in-always-p nil
	ewal-use-built-in-on-failure-p t
	ewal-built-in-palette "sexy-material"))


(use-package ewal-doom-themes
  :ensure t
  :init
  (setq ewal-use-built-in-always-p nil
	ewal-use-built-in-on-failure-p t
	ewal-built-in-palette "sexy-material"))

;; WHITESPACES
;; (setq whitespace-style '(face empty trailing))
;; (setq whitespace-line-column 80)
;; (global-whitespace-mode 1)

;; (eval-after-load 'whitespace
;;   '(diminish 'global-whitespace-mode))


(use-package hl-todo
  :ensure t
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
	  ("IMPORTANT"  success bold)
	  ("DEPRECATED" font-lock-doc-face bold)
	  ("LATER" font-lock-doc-face bold))))

;; DEPRECATED 
;; (use-package moody
;;   :ensure t
;;   :config
;;   (setq x-underline-at-descent-line t)
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode)
;;   (moody-replace-eldoc-minibuffer-message-function))

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode)
  (setq doom-modeline-height 35
        doom-modeline-bar-width 0
        doom-modeline-persp-name t
        doom-modeline-persp-icon t))

(use-package spacious-padding
  :ensure t
  :config
  (general-after-init(spacious-padding-mode)))


;;;; THEME

(use-package catppuccin-theme
  :ensure t)

(use-package timu-caribbean-theme
  :ensure t)

(use-package spacemacs-theme
  :ensure t)


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

 ;; (load-theme 'doom-material-dark t)
 (load-theme 'doom-material-dark t)

(use-package kaolin-themes
  :ensure t)

(use-package ef-themes
  :ensure t)


(defvar dark-themes '(doom-badger doom-pine doom-laserwave doom-one doom-1337 doom-nord doom-dark+ doom-henna doom-opera doom-rouge doom-xcode
				  doom-snazzy doom-Iosvkem doom-dracula doom-gruvbox doom-horizon doom-lantern doom-molokai doom-peacock doom-vibrant
				  doom-zenburn doom-ayu-dark doom-manegarm doom-material doom-miramare doom-old-hope doom-ephemeral doom-moonlight doom-palenight
				  doom-sourcerer doom-spacegrey doom-ayu-mirage doom-plain-dark doom-acario-dark doom-city-lights doom-fairy-floss doom-monokai-pro
				  doom-nord-aurora doom-tokyo-night doom-wilmersdorf doom-bluloco-dark doom-feather-dark doom-oceanic-next doom-oksolar-dark
				  doom-material-dark doom-solarized-dark doom-tomorrow-night doom-challenger-deep doom-monokai-classic doom-monokai-machine
				  doom-monokai-octagon doom-outrun-electric doom-monokai-spectrum doom-shades-of-purple doom-monokai-ristretto doom-solarized-dark-high-contrast
				  ef-duo-dark ef-bio ef-dark ef-rosa ef-night ef-autumn ef-cherie ef-winter ef-elea-dark ef-symbiosis ef-trio-dark ef-maris-dark ef-melissa-dark
				  ef-tritanopia-dark ef-deuteranopia-dark kaolin-bubblegum kaolin-dark kaolin-eclipse kaolin-ocean kaolin-shiva kaolin-aurora kaolin-galaxy
				  kaolin-temple kaolin-blossom kaolin-mono-dark kaolin-valley-dark modus-vivendi ewal-doom-one ewal-doom-vibrant timu-caribbean spacemacs-dark)
  "List of dark color themes.")

(defvar light-themes '(doom-plain doom-ayu-light doom-earl-grey doom-flatwhite doom-one-light doom-nord-light doom-opera-light doom-acario-light doom-homage-white doom-tomorrow-day
				  doom-bluloco-light doom-feather-light doom-gruvbox-light doom-oksolar-light doom-solarized-light ef-day ef-frost ef-light ef-cyprus ef-kassio
				  ef-spring ef-summer ef-arbutus ef-duo-light ef-elea-light ef-trio-light ef-maris-light ef-melissa-light ef-tritanopia-light ef-deuteranopia-light
				  kaolin-light kaolin-breeze kaolin-mono-light kaolin-valley-light adwaita modus-operandi spacemacs-light)
  "List of light color themes.")

(defvar ugly-themes '(doom-nova doom-meltbus doom-ir-black doom-homage-black manoj-dark light-blue misterioso tango-dark tsdh-light wheatgrass whiteboard deeper-blue leuven-dark)
  "List of themes i don't like.")


(defun laluxx/consult-dark-themes ()
  "Select and load a theme from the list of dark themes with a preview, restoring the original theme if aborted."
  (interactive)
  (let* ((original-theme (car custom-enabled-themes))
	 (saved-theme original-theme)
	 (avail-themes dark-themes))
    (consult--read
     (mapcar #'symbol-name avail-themes)
     :prompt "Select dark theme: "
     :require-match t
     :category 'theme
     :history 'consult--theme-history
     :lookup (lambda (selected &rest _)
	       (setq selected (and selected (intern-soft selected)))
	       (or (and selected (car (memq selected avail-themes)))
		   saved-theme))
     :state (lambda (action theme)
	      (pcase action
		('return (unless (equal theme saved-theme)
			   (consult-theme (or theme saved-theme))))
		((and 'preview (guard theme))
		 (unless (equal theme (car custom-enabled-themes))
		   (mapc #'disable-theme custom-enabled-themes)
		   (load-theme theme t))))))
     :default (symbol-name (or saved-theme 'default)))
    ;; This check restores the original theme if no theme was selected
    (unless (or (equal (car custom-enabled-themes) original-theme)
		(member (car custom-enabled-themes) avail-themes))
      (mapc #'disable-theme custom-enabled-themes)
      (when original-theme
	(load-theme original-theme t))))


(defun laluxx/consult-light-themes ()
  "Select and load a theme from the list of light themes with a preview, restoring the original theme if aborted."
  (interactive)
  (let* ((original-theme (car custom-enabled-themes))
	 (saved-theme original-theme)
	 (avail-themes light-themes))
    (consult--read
     (mapcar #'symbol-name avail-themes)
     :prompt "Select light theme: "
     :require-match t
     :category 'theme
     :history 'consult--theme-history
     :lookup (lambda (selected &rest _)
	       (setq selected (and selected (intern-soft selected)))
	       (or (and selected (car (memq selected avail-themes)))
		   saved-theme))
     :state (lambda (action theme)
	      (pcase action
		('return (unless (equal theme saved-theme)
			   (consult-theme (or theme saved-theme))))
		((and 'preview (guard theme))
		 (unless (equal theme (car custom-enabled-themes))
		   (mapc #'disable-theme custom-enabled-themes)
		   (load-theme theme t))))))
     :default (symbol-name (or saved-theme 'default)))
    ;; This check restores the original theme if no theme was selected
    (unless (or (equal (car custom-enabled-themes) original-theme)
		(member (car custom-enabled-themes) avail-themes))
      (mapc #'disable-theme custom-enabled-themes)
      (when original-theme
	(load-theme original-theme t))))

(defun laluxx/consult-ugly-themes ()
  "Select and load a theme from the list of ugly themes with a preview, restoring the original theme if aborted."
  (interactive)
  (let* ((original-theme (car custom-enabled-themes))
	 (saved-theme original-theme)
	 (avail-themes ugly-themes))
    (consult--read
     (mapcar #'symbol-name avail-themes)
     :prompt "Select ugly theme: "
     :require-match t
     :category 'theme
     :history 'consult--theme-history
     :lookup (lambda (selected &rest _)
	       (setq selected (and selected (intern-soft selected)))
	       (or (and selected (car (memq selected avail-themes)))
		   saved-theme))
     :state (lambda (action theme)
	      (pcase action
		('return (unless (equal theme saved-theme)
			   (consult-theme (or theme saved-theme))))
		((and 'preview (guard theme))
		 (unless (equal theme (car custom-enabled-themes))
		   (mapc #'disable-theme custom-enabled-themes)
		   (load-theme theme t))))))
     :default (symbol-name (or saved-theme 'default)))
    ;; This check restores the original theme if no theme was selected
    (unless (or (equal (car custom-enabled-themes) original-theme)
		(member (car custom-enabled-themes) avail-themes))
      (mapc #'disable-theme custom-enabled-themes)
      (when original-theme
	(load-theme original-theme t))))


;; MINIMA THEMES

(add-to-list 'custom-theme-load-path "~/.config/emacs/lisp/themes/")

;; TODO if also-toggle-xorg-colors call from-emacs
;; TODO start based on "dark" or "light"
(defvar opposite-theme-pairs
  '((doom-one . doom-one-light)
    (doom-badger . doom-material-dark)
    (doom-nord . doom-nord-light)
    (doom-dracula . doom-solarized-light)
    (doom-solarized-dark . doom-solarized-light))
  "Pairs of dark and light themes.")

(defun laluxx/toggle-theme-pair ()
  "Toggle between paired themes defined in `opposite-theme-pairs`."
  (interactive)
  (let* ((current-theme (car custom-enabled-themes))
         (pair (or (assoc current-theme opposite-theme-pairs) (rassoc current-theme opposite-theme-pairs)))
         (next-theme (if (eq current-theme (car pair)) (cdr pair) (car pair))))
    (when pair
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme next-theme t)
      (message "Switched to theme: %s" next-theme))))


;;;; COMPLETION
;;(setq history-length 25)
(savehist-mode)

(setq recentf-auto-cleanup 'never) ;; Disable automatic cleanup

(defun suppress-messages (orig-fun &rest args)
  "Suppress messages from ORIG-FUN called with ARGS."
  (let ((inhibit-message t))
    (apply orig-fun args)))

(advice-add 'recentf-load-list :around #'suppress-messages)

(recentf-mode 1)

;; Default Completion
(setq completion-auto-wrap t
      completion-auto-help 'always
      completion-show-help nil
      completions-max-height 10)

;; (use-package jinx
;;   :diminish
;;   :ensure t
;;   :hook (emacs-startup . global-jinx-mode)
;;   :bind (("M-#" . jinx-correct)
;;          ("C-M-#" . jinx-languages)))


;; TODO 'eshell' is not a popup when spawned
(use-package popper
  :ensure t
  :config
  (global-set-key (kbd "C-;") #'popper-toggle)
  (global-set-key (kbd "C-'") #'popper-cycle)
  (global-set-key (kbd "M-;") #'popper-toggle-type)
  ;; (setq popper-display-function #'display-buffer-in-child-frame) ; TODO
  (setq popper-window-height 15)
  (setq popper-reference-buffers
        '(Custom-mode
          compilation-mode
          messages-mode
          help-mode
          occur-mode
          eshell-mode
          "^\\*Warnings\\*"
          "^\\*Compile-Log\\*"
          "^\\*HS-Error\\*"
          "^\\*shell\\*"
          "^\\*Messages\\*"
          "^\\*Backtrace\\*"
          "^\\*evil-registers\\*"
          "^\\*ielm\\*"
          "^\\*TeX Help\\*"
          "^\\*Shell Command Output\\*"
          "^\\*Async Shell Command\\*"
          "^\\*Completions\\*"
          "^\\*Apropos"
          "Calc:"
          "[Oo]utput\\*"))
  (popper-mode))

;; NOTE This might not be needed
(defun my/popper-hide-modeline ()
  (setq-local mode-line-format nil))
(add-hook 'popper-open-popup-hook 'my/popper-hide-modeline)

(defun laluxx/hide-modeline-for-popups ()
  "Hide the modeline in specific buffers."
  (when (or (string= (buffer-name) "*Warnings*")
            (string= (buffer-name) "*Compile-Log*")
            (string= (buffer-name) "*ielm*")
            (string= (buffer-name) "*shell*")
            (string= (buffer-name) "*eshell*")
            (string= (buffer-name) "*HS-Error*")
            (string= (buffer-name) "*Help*")
            (string= (buffer-name) "*Apropos*")
            (string= (buffer-name) "*Disabled Command*")
            (string= (buffer-name) "*Backtrace*")
            (string= (buffer-name) "*compilation*")
            (string= (buffer-name) "*Shell Command Output*")
           )
    (setq mode-line-format nil)))

(add-hook 'after-change-major-mode-hook 'laluxx/hide-modeline-for-popups)

;; Why do i have to do it like this 
(with-current-buffer "*Messages*"
  (setq mode-line-format nil))


;;;; MINIBUFFER
;; TODO it doesnt really work
(defun my-toggle-variable-via-embark ()
  "Toggle a variable directly using Embark's toggle variable function, applying it to the currently highlighted candidate in Vertico."
  (interactive)
  (let* ((candidates (embark--vertico-candidates))
         (type (car candidates))
         (candidate (vertico--candidate)))
    (when (and (eq type 'variable) candidate (boundp (intern candidate)))
      (embark-toggle-variable (intern candidate)))))


;; (defun my-set-variable-via-embark ()
;;   "Set a variable directly using Embark's insert variable value function, applying it to the currently highlighted candidate in Vertico."
;;   (interactive)
;;   (let* ((candidates (embark--vertico-candidates))
;;          (type (car candidates))
;;          (candidate (vertico--candidate)))
;;     (when (and (eq type 'variable) candidate (boundp (intern candidate)))
;;       (embark-insert-variable-value (intern candidate)))))

(with-eval-after-load 'vertico
  ;; Bind the custom Embark functions to C-t and C-s within Vertico's context
  (define-key vertico-map (kbd "C-t") #'my-toggle-variable-via-embark)
  ;; (define-key vertico-map (kbd "C-s") #'my-set-variable-via-embark)
  )

(use-package embark
  :ensure t
  :bind
  ("C-." . embark-act)
  ("C-," . embark-become))

(use-package embark-consult
  :ensure t)

(use-package vertico
  :ensure t
  :config
  (vertico-multiform-mode 1)
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20)))
  (vertico-mode))

(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq enable-recursive-minibuffers t)



(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :ensure t
  :init
  (all-the-icons-completion-mode))


(use-package corfu
  ;; Optional customization's
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin

  :ensure t
  :init
  (global-corfu-mode))

(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer."
  (when (local-variable-p 'completion-at-point-functions)
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
		corfu-popupinfo-delay nil)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

;; (setq-local corfu-auto        t
;;             corfu-auto-delay  0 ;; TOO SMALL - NOT RECOMMENDED
;;             corfu-auto-prefix 1 ;; TOO SMALL - NOT RECOMMENDED
;;             completion-styles '(basic))
;; (use-package corfu-map)

(setq tab-always-indent 'complete)


;; (use-package cape
;;   :init
;;   ;; Add to the global default value of `completion-at-point-functions' which is
;;   ;; used by `completion-at-point'.  The order of the functions matters, the
;;   ;; first function returning a result wins.  Note that the list of buffer-local
;;   ;; completion functions takes precedence over the global list.
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   (add-to-list 'completion-at-point-functions #'cape-elisp-block)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-history)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-tex)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-dict)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-line)
;; )

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-blend-background t)
  (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package consult
  :ensure t
  :init
  (global-set-key (kbd "C-x b") #'consult-buffer))

;; (use-package consult-eglot
;;   :ensure t)


(defun laluxx/find-todo ()
  "Search for the term 'TODO' in the current buffer."
  (interactive)
  (consult-line "TODO"))

(defun laluxx/find-note ()
  "Search for the term 'NOTE' in the current buffer."
  (interactive)
  (consult-line "NOTE"))



(use-package which-key
  :diminish
  :ensure t
  :init
  (which-key-mode))

(use-package helpful
  :ensure t
  :init
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  :hook (helpful-mode . my-helpful-olivetti-setup))

(defun my-helpful-olivetti-setup ()
  "Enable Olivetti mode and set the preferred width."
  (olivetti-mode 1)
  (olivetti-set-width 84))


;;;; SHELL

;; TODO 'C-c-C-j' dwim open on the bottom of the current buffer only
;; if you are in a vertical split
(setq shell-file-name "/bin/bash")
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Don't ask for confirmation when killing a running process
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

(add-hook 'shell-mode-hook
          (lambda ()
            (local-set-key (kbd "<up>") 'comint-previous-input)
            (local-set-key (kbd "<down>") 'comint-next-input)
            (local-set-key (kbd "C-l") 'comint-clear-buffer)))


(defun toggle-shell ()
  (interactive)
  (if (get-buffer "*shell*")
      (if (equal (current-buffer) (get-buffer "*shell*"))
          (bury-buffer)
        (shell))
    (shell)))

(global-set-key (kbd "C-c C-j") 'toggle-shell)








;;;; ESHELL
(use-package eshell
  :ensure t
  :defines eshell-prompt-function
  :bind (:map eshell-mode-map
	 ([remap recenter-top-bottom] . eshell/clear))
  :config
  (setq eshell-banner-message "")  ; Suppress the default welcome message
  (add-hook 'eshell-mode-hook 'eshell/clear)  ; Clear eshell on startup

  ;; Override eshell-cmpl-mode-map for M-TAB
  (with-eval-after-load 'em-cmpl
    (define-key eshell-cmpl-mode-map (kbd "M-TAB") 'eyebrowse-last-window-config))

  (with-no-warnings
    (defun eshell/clear ()
      "Clear the eshell buffer."
      (interactive)
      (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))


    (defun eshell/emacs (&rest args)
      "Open a file (ARGS) in Emacs.  Some habits die hard."
      (if (null args)
	  ;; If I just ran "emacs", I probably expect to be launching
	  ;; Emacs, which is rather silly since I'm already in Emacs.
	  ;; So just pretend to do what I ask.
	  (bury-buffer)
	;; We have to expand the file names or else naming a directory in an
	;; argument causes later arguments to be looked for in that directory,
	;; not the starting directory
	(mapc #'find-file (mapcar #'expand-file-name (flatten-tree (reverse args))))))
    (defalias 'eshell/e #'eshell/emacs)
    (defalias 'eshell/ec #'eshell/emacs)

    (defun eshell/ebc (&rest args)
      "Compile a file (ARGS) in Emacs. Use `compile' to do background make."
      (if (eshell-interactive-output-p)
	  (let ((compilation-process-setup-function
		 (list 'lambda nil
		       (list 'setq 'process-environment
			     (list 'quote (eshell-copy-environment))))))
	    (compile (eshell-flatten-and-stringify args))
	    (pop-to-buffer compilation-last-buffer))
	(throw 'eshell-replace-command
	       (let ((l (eshell-stringify-list (flatten-tree args))))
		 (eshell-parse-command (car l) (cdr l))))))
    (put 'eshell/ebc 'eshell-no-numeric-conversions t)

    (defun eshell-view-file (file)
      "View FILE.  A version of `view-file' which properly rets the eshell prompt."
      (interactive "fView file: ")
      (unless (file-exists-p file) (error "%s does not exist" file))
      (let ((buffer (find-file-noselect file)))
	(if (eq (get (buffer-local-value 'major-mode buffer) 'mode-class)
		'special)
	    (progn
	      (switch-to-buffer buffer)
	      (message "Not using View mode because the major mode is special"))
	  (let ((undo-window (list (window-buffer) (window-start)
				   (+ (window-point)
				      (length (funcall eshell-prompt-function))))))
	    (switch-to-buffer buffer)
	    (view-mode-enter (cons (selected-window) (cons nil undo-window))
			     'kill-buffer)))))

    (defun eshell/less (&rest args)
      "Invoke `view-file' on a file (ARGS).

\"less +42 foo\" will go to line 42 in the buffer for foo."
      (while args
	(if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
	    (let* ((line (string-to-number (match-string 1 (pop args))))
		   (file (pop args)))
	      (eshell-view-file file)
	      (forward-line line))
	  (eshell-view-file (pop args)))))
    (defalias 'eshell/more #'eshell/less))

  ;;  Display extra information for prompt
  (use-package eshell-prompt-extras
    :ensure t
    :after esh-opt
    :defines eshell-highlight-prompt
    :autoload (epe-theme-lambda epe-theme-dakrone epe-theme-pipeline)
    :init (setq eshell-highlight-prompt nil
		eshell-prompt-function #'epe-theme-lambda))

  ;; `eldoc' support
  (use-package esh-help
    :ensure t
    :init (setup-esh-help-eldoc))

  ;; `cd' to frequent directory in `eshell'
  (use-package eshell-z
    :ensure t
    :hook (eshell-mode . (lambda () (require 'eshell-z)))))


(defun laluxx/eshell ()
  "Simulate the press of M-3."
  (interactive)
  (execute-kbd-macro (kbd "M-3")))


(defun eshell-color-word (word face)
  "Color a WORD in eshell with the specified FACE."
  (save-excursion
    (goto-char eshell-last-output-start)
    (while (re-search-forward (concat "\\b" word "\\b") eshell-last-output-end t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face face))))


(defvar eshell-word-faces '()
  "List of (word . face) pairs to color in eshell.")

(defun eshell-apply-coloring ()
  "Apply coloring to words in eshell based on `eshell-word-faces`."
  (mapc (lambda (pair)
          (eshell-color-word (car pair) (cdr pair)))
        eshell-word-faces))

(defun eshell-add-color-word (word face)
  "Add a word and face pair to `eshell-word-faces` and update eshell hook."
  (add-to-list 'eshell-word-faces (cons word face))
  (add-hook 'eshell-output-filter-functions 'eshell-apply-coloring))


;; TODO why does this fuck up the prompt ?
;; (eshell-add-color-word "INFO" 'success)
;; (eshell-add-color-word "ERROR" 'error)
;; (eshell-add-color-word "DEBUG" 'font-lock-comment-face)


;;;; LANGUAGES

;; LISP

(setq inferior-lisp-program "clisp")


;; LISP Configuration with enhanced SLY interaction
(use-package sly
  :ensure t
  :config
  (setq sly-command-switch-to-existing-lisp 'always)
  (setq inferior-lisp-program "clisp"))

;; TODO add a bool value to switch to the sly buffer on save
(defun my-lisp-load-on-save ()
  "Enhanced save function for Lisp files. It checks for any active SLY buffer, starts SLY if necessary, then loads the file only if SLY was started by this function."
  (add-hook 'after-save-hook
            (lambda ()
              (let ((source-buffer (current-buffer))
                    (sly-buffer-prefix "*sly-mrepl for clisp*")
                    (sly-started nil))
                ;; Check if any SLY buffer exists and start SLY if none exists
                (unless (seq-some (lambda (buffer)
                                    (string-prefix-p sly-buffer-prefix (buffer-name buffer)))
                                  (buffer-list))
                  (sly)
                  (setq sly-started t)) ; Mark that SLY was started by this action
                ;; Wait briefly to ensure SLY has started if needed
                (when sly-started
                  (sleep-for 0.5)) ; Adjust timing if necessary
                ;; If SLY was started by this function, switch back and load the file
                (if sly-started
                    (progn
                      (switch-to-buffer-other-window source-buffer)
                      (sly-load-file (buffer-file-name)))
                  ;; Otherwise just load the file without switching windows
                  (sly-load-file (buffer-file-name))))) ; Load the current file into Lisp
            nil 'local))


(add-hook 'lisp-mode-hook 'my-lisp-load-on-save)


;; SCHEME
(setq scheme-program-name "guile")

(use-package geiser
  :ensure t)

(use-package geiser-guile
  :ensure t)


;; TREE SITTER
;; TODO it print alot of garbage
;; (use-package tree-sitter
;;   :ensure t)

;; (use-package tree-sitter-langs
;;   :ensure t)



(use-package haskell-mode
  :ensure t
  :config
  (setq haskell-process-show-debug-tips nil))

(defun my-haskell-load-on-save ()
  "When saving, load the current Haskell file interactively."
  (add-hook 'after-save-hook
            (lambda ()
              (call-interactively 'haskell-process-load-file))
            nil 'local))

(add-hook 'haskell-mode-hook 'my-haskell-load-on-save)

(defun setup-haskell-interactive-keys ()
  "Set up custom keybindings for the Haskell interactive mode."
  (local-set-key (kbd "<up>") 'haskell-interactive-mode-history-previous)
  (local-set-key (kbd "<down>") 'haskell-interactive-mode-history-next)
  (local-set-key (kbd "C-l") 'haskell-interactive-mode-clear))

(add-hook 'haskell-interactive-mode-hook 'setup-haskell-interactive-keys)







;; OCAML
(use-package tuareg
  :ensure t)

(use-package dune
  :ensure t)

;; TODO those 2 dont compile on arch
;; (use-package utop
;;   :ensure t)

;; (use-package merlin
;;   :ensure t)




;; JSON

(use-package json-mode
  :ensure t)



;; ELISP
(defun laluxx/setup-emacs-lisp-keys ()
  "Setup keybindings for `emacs-lisp-mode' and related modes."
  (define-key emacs-lisp-mode-map (kbd "M-TAB") 'eyebrowse-last-window-config)
  (define-key lisp-interaction-mode-map (kbd "M-TAB") 'eyebrowse-last-window-config))


(add-hook 'emacs-lisp-mode-hook 'laluxx/setup-emacs-lisp-keys)
(add-hook 'lisp-interaction-mode-hook 'laluxx/setup-emacs-lisp-keys)

;; C
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

;; (add-to-list 'auto-mode-alist '("\\.vert\\'" . c-mode))
;; (add-to-list 'auto-mode-alist '("\\.frag\\'" . c-mode))

(use-package glsl-mode
  :ensure t)





;; TODO enabled lang packages when a .lua file is opened 
;; LUA
;; (use-package lua-mode
;;   :ensure t)

;; ZIG
;; (use-package zig-mode
;;   :ensure t)

;; (use-package lsp-haskell
;;   :ensure t)

(use-package lsp-mode
  :ensure t
  :hook ((c-mode . lsp)
         (c++-mode . lsp))
  :config
  (setq lsp-idle-delay 0.1) ; clangd is fast
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :ensure t
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package consult-lsp
  :ensure t)

;; (use-package indent-bars
;;   :load-path "~/.config/emacs/lisp/indent-bars")


;;;; ORG MODE

(use-package org-bars
  :load-path "~/.config/emacs/lisp/org-bars"
  :config
  (setq org-bars-stars '(:empty "◉"
                                :invisible "◉"
                                :visible "▼"))
  :hook (org-mode . org-bars-mode))


;; NOTE This require a shit ton of python dependencies
;; Install them all or it will not work.
;; (use-package eaf
;;   :load-path "~/.config/emacs/lisp/emacs-application-framework"
;;   :custom
;;   ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (browse-url-browser-function 'eaf-open-browser)
;;   :config
;;   (require 'eaf-browser)
;;   ;; (require 'eaf-pdf-viewer)
;;   (require 'eaf-image-viewer)
;;   (defalias 'browse-web #'eaf-open-browser))


(use-package flycheck
  :ensure t)

(use-package consult-flycheck
  :ensure t)


(global-set-key (kbd "C-c C-g") 'xref-find-definitions)
(global-set-key (kbd "C-x C-c") 'quick-calc)


(use-package shut-up
  :ensure t)

(use-package suggest
  :ensure t)



;; RUST
(use-package rustic
  :ensure t )


;;;; BACKUP
(setq make-backup-files nil)


;;;; DIRED
(setq dired-listing-switches
      "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")

;; (use-package dirvish
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "C-x C-j") 'dirvish-dwim)
;;   (global-set-key (kbd "C-c C-h") 'dirvish-side)
;;   (setq dirvish-attributes '(nerd-icons file-size collapse subtree-state)) ;; vc-state  git-msg file-time
;;   (setq dired-omit-files "^\\...+$")  ; This regex matches both '.' and '..'
;;   (setq dirvish-use-mode-line nil
;;         dirvish-use-header-line nil)
;;   (setq dirvish-path-separators (list "  " "  " "  "))
;;   (add-hook 'dired-mode-hook #'dired-omit-mode)
;;   (dirvish-override-dired-mode))

;; TODO This is not the correct hook
(defun enable-diredfl-for-dirvish-preview ()
  "Enable `diredfl-mode` in Dirvish preview buffers."
  (when (string-match-p "\\*Dirvish-preview-.*\\*" (buffer-name))
    (diredfl-mode 1)))

(add-hook 'after-change-major-mode-hook 'enable-diredfl-for-dirvish-preview)

(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode) ; enable this only if you dont use dirvish
  )

;; NOTE Use this only if you don't use dirvish
(use-package nerd-icons-dired
  :ensure t
  :diminish
  :custom-face
  (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
  :hook (dired-mode . nerd-icons-dired-mode))



(defvar auto-create-directory-enabled t
  "When non-nil, Emacs will automatically create non-existing directories when opening files.")

(defun create-dir-if-not-exists ()
  "Create the parent directory of the file to be opened if it does not already exist and if `auto-create-directory-enabled' is true."
  (when auto-create-directory-enabled
    (let ((dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

(add-hook 'find-file-hook 'create-dir-if-not-exists)

(defun toggle-auto-create-directory ()
  "Toggle the automatic creation of non-existing directories when opening files."
  (interactive)
  (setq auto-create-directory-enabled (not auto-create-directory-enabled))
  (message "Auto-create directory is now %s"
           (if auto-create-directory-enabled "enabled" "disabled")))


(defun dired-copy-file-contents-to-clipboard ()
  "Copy the contents of the file at point in Dired to the clipboard."
  (interactive)
  (let* ((filename (dired-get-file-for-visit))
         (buffer-existed (get-buffer filename))
         (buffer (find-file-noselect filename)))
    (with-current-buffer buffer
      (clipboard-kill-ring-save (point-min) (point-max)))
    (unless buffer-existed
      (kill-buffer buffer))
    (message "Copied %s" filename)))



(use-package wdired
  :ensure t)

(defun my-dired-mode-setup ()
  "Custom keybindings and settings for `dired-mode`."
  (define-key dired-mode-map (kbd "b") 'dired-up-directory)
  (define-key dired-mode-map (kbd "j") 'dired-next-line)
  (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  (define-key dired-mode-map (kbd "h") 'dired-up-directory)
  (define-key dired-mode-map (kbd "l") 'dired-find-file)
  (define-key dired-mode-map (kbd "C-s") 'find-file)
  (define-key dired-mode-map (kbd "TAB") 'dirvish-subtree-toggle)
  (define-key dired-mode-map (kbd "y") 'dired-copy-file-contents-to-clipboard)
  (define-key dired-mode-map (kbd "o") 'dired-maybe-insert-subdir)
  (define-key dired-mode-map (kbd "i") 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "F") 'find-file)
  (auto-revert-mode 1)
  )

(add-hook 'dired-mode-hook 'my-dired-mode-setup)


;;;; ISEARCH
;; TODO while typing buffer wrapping
;; (defun isearch-repeat-forward+ ()
;;   (interactive)
;;   (unless isearch-forward
;;     (goto-char isearch-other-end))
;;   (isearch-repeat-forward)
;;   (unless isearch-success
;;     (isearch-repeat-forward)))

;; (defun isearch-repeat-backward+ ()
;;   (interactive)
;;   (when (and isearch-forward isearch-other-end)
;;     (goto-char isearch-other-end))
;;   (isearch-repeat-backward)
;;   (unless isearch-success
;;     (isearch-repeat-backward)))

;; (define-key isearch-mode-map (kbd "C-S") 'isearch-repeat-forward+)
;; (define-key isearch-mode-map (kbd "C-r") 'isearch-repeat-backward+)

(global-set-key (kbd "C-r") 'isearch-forward)
(global-set-key (kbd "C-s") 'consult-line)



;;;; EDITING
(setq-default truncate-lines t)
(electric-pair-mode)
(global-auto-revert-mode 1)
(delete-selection-mode t)
(save-place-mode t)


(defun laluxx/copy-buffer ()
  "Copy the entire buffer to the kill ring."
  (interactive)
  (kill-new (buffer-string)))

(global-set-key (kbd "C-c C-b") 'laluxx/copy-buffer)

(use-package evil
  :ensure t)

(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode))


;; TODO stop blink and line cursor
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "M-S-<down>") 'mc/mark-next-like-this)
  (global-set-key (kbd "M-S-<up>") 'mc/mark-previous-like-this)
  )


(use-package drag-stuff
  :ensure t
  :bind (("M-<left>" . drag-stuff-left)
         ("M-<right>" . drag-stuff-right)
         ("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))


;; (use-package goto-last-change
;;   :ensure t)


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


(global-set-key (kbd "C-S-o") (lambda ()
			      (interactive)
			      (duplicate-line)
			      (next-line)))



(defun laluxx/mwim-beginning ()
  "Move point to the first non-whitespace character on this line.
If the point is already there, move to the beginning of the line.
Extend the selection if shift is held."
  (interactive "^")
  (let ((origin (point)))
    (back-to-indentation)
    (when (= origin (point))
      (move-beginning-of-line 1))))

(defun laluxx/mwim-end ()
  "Move point to the end of the text on this line.
If the point is already there, move to the end of the line.
Extend the selection if shift is held."
  (interactive "^")
  (let ((origin (point)))
    (end-of-line-text)
    (when (= origin (point))
      (move-end-of-line 1))))


;; TODO
(defun laluxx/backward-kills-word ()
  "Kills the previous segment of a camelCase word or a whole word if not camelCase."
  (interactive)
  (let ((case-fold-search nil)  ; Ensure case sensitivity
        (start (point)))
    (if (and (looking-back "[A-Z][a-z]+" nil)  ; If there's a lowercase following uppercase
             (looking-back "\\([A-Z][a-z]+\\)[A-Z]" nil)) ; And it's part of a camelCase
        (progn
          (backward-char 1)
          (if (looking-back "[A-Z][a-z]+" nil)
              (kill-region (match-beginning 0) start)  ; Kill to the beginning of the segment
            (backward-kill-word 1)))
      (backward-kill-word 1))))  ; Fallback to killing a whole word

(global-set-key (kbd "C-c C-k") 'laluxx/backward-kills-word)




(defun laluxx/smart-eval-sexp ()
  "Evaluate the s-expression based on the position of the point.
If the point is on an opening or closing parenthesis, evaluate the enclosed s-expression.
Otherwise, evaluate the last s-expression before the point."
  (interactive)
  (if (or (eq (char-after) ?\()
          (eq (char-before) ?\)))
      (progn
        (save-excursion
          (unless (eq (char-after) ?\()
            (backward-up-list))
          (eval-last-sexp nil)))
    (eval-last-sexp nil)))

(global-set-key (kbd "C-M-j") 'laluxx/smart-eval-sexp)


(defun end-of-line-text ()
  "Move point to the last non-whitespace character on this line."
  (end-of-line)
  (skip-syntax-backward " " (line-beginning-position)))


(defvar copied-line nil "Flag to indicate if a line has been copied.")

(defun laluxx/copy-line ()
  "Copy the current line to the clipboard and set `copied-line` to t."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (kill-ring-save (point) (line-end-position)))
  (setq copied-line t)
  (message "Line copied!"))

;; ;; TODO in some places it doesn't make any sense to indent like in 'Makefiles'
(defun laluxx/yank-line ()
  "Yank the copied line to the next line if `copied-line` is true
   and the current line is not empty, otherwise perform a regular yank.
   Then indent the yanked region."
  (interactive)
  (let ((start (point)))  ; Remember the point position before yanking.
    (if copied-line
        (if (save-excursion
              (beginning-of-line)
              (/= (point) (line-end-position)))
            (progn
              (end-of-line)
              (newline)
              (yank)
              (indent-region start (point)))  ; Indent the region after yanking.
          (progn
            (yank)
            (indent-region start (point))))  ; Indent even if the line was empty.
      (progn
        (yank)
        (indent-region start (point))))))  ; Always indent for a regular yank.






(defun laluxx/kill-line-or-kill-region ()
  "'kill-line' if no active region, otherwise 'kill-region'."
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-region)
    (kill-line)))


(defun laluxx/insert-or-comment-region ()
  "Insert the character 'c' if no active region, otherwise comment the region."
  (interactive)
  (if (use-region-p)
      (call-interactively 'comment-dwim)
    (insert "c")))

(global-set-key (kbd "c") 'laluxx/insert-or-comment-region)


;; COPY
(defun laluxx/copy-append-region (start end)
  "Append selected region to the clipboard contents."
  (interactive "r")
  (let ((selection (buffer-substring start end)))
    (if (not (use-region-p))
        (message "No region selected!")
      (with-temp-buffer
        (insert (if (gui-get-selection 'CLIPBOARD) (gui-get-selection 'CLIPBOARD) ""))
        (insert selection)
        (gui-set-selection 'CLIPBOARD (buffer-string)))
      (message "Region appended to clipboard!"))))

(defun laluxx/insert-or-copy-region ()
  "Insert the character 'y' if no active region, otherwise copy the region and reset `copied-line`."
  (interactive)
  (if (use-region-p)
      (progn
	(call-interactively 'kill-ring-save)
	(setq copied-line nil))
    (insert "y")))

(defun laluxx/delete-char-or-kill-region ()
  "Call 'delete-char' if no active region, otherwise 'kill-region'."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (delete-char 1)))

(defun laluxx/drag-down-or-forward-paragraph ()
  "If a region is active 'drag-stuff-down' else 'forward-paragraph'."
  (interactive)
  (if (use-region-p)
      (call-interactively 'drag-stuff-down)
    (forward-paragraph)))

(defun laluxx/drag-up-or-backward-paragraph ()
  "If a region is active 'drag-stuff-up' else 'backward-paragraph'."
  (interactive)
  (if (use-region-p)
      (call-interactively 'drag-stuff-up)
    (backward-paragraph)))

(defun laluxx/forward-paragraph-select ()
  "Move forward a paragraph and extend the selection."
  (interactive)
  (unless (use-region-p)
    (push-mark))
  (forward-paragraph)
  (activate-mark))

(defun laluxx/backward-paragraph-select ()
  "Move backward a paragraph and extend the selection."
  (interactive)
  (unless (use-region-p)
    (push-mark))
  (backward-paragraph)
  (activate-mark))


;; 4 seconds just to load iedit ?
(use-package iedit
  :ensure t
  :defer t
  :config
  (defun laluxx/iedit-forward-word()
    "Activate iedit-mode and go to the end of the current word."
    (interactive)
    (iedit-mode)
    (forward-word))

  (defun laluxx/iedit-backward-word()
    "Activate iedit-mode and go to the end of the current word."
    (interactive)
    (iedit-mode)
    (backward-word))

  (global-set-key (kbd "M-I") 'laluxx/iedit-backward-word)
  (global-set-key (kbd "M-i") 'laluxx/iedit-forward-word)
  )


(defun laluxx/open-above ()
  "Open a new line above the current line and position the cursor at its beginning."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-for-tab-command))

(defun laluxx/open-below ()
  "Open a new line below the current line and position the cursor at its beginning."
  (interactive)
  (end-of-line)
  (newline-and-indent))


;;;; FUNCTIONS



;; SELECTION
;; TODO better mouse selection
;; drag-down -> visual-line
;; drag-in-line -> normal one

(defun mark-line (&optional arg allow-extend)
  "Set mark ARG lines away from point.
The place mark goes is the same place \\[end-of-line] would
move to with the same argument.
Interactively, if this command is repeated
or (in Transient Mark mode) if the mark is active,
it marks the next ARG lines after the ones already marked."
  (interactive "P\np")
  (cond ((and allow-extend
              (or (and (eq last-command this-command) (mark t))
                  (region-active-p)))
         (setq arg (if arg (prefix-numeric-value arg)
                     (if (< (mark) (point)) -1 1)))
         (set-mark
          (save-excursion
            (goto-char (mark))
            (forward-line arg)
            (point))))
        (t
         (push-mark
          (save-excursion
            (end-of-line (prefix-numeric-value arg))
            (point))
          nil t))))

(defun laluxx/center-word-in-buffer (word)
  "Center a word horizontally in the current buffer."
  (interactive "sEnter word to center: ")
  (let* ((win-width (window-width))
         (padding (/ (- win-width (length word)) 2))
         (padding-str (make-string (max 0 padding) ?\s)))
    (save-excursion
      (move-to-column padding t)
      (insert word))))

(defun laluxx/center-message (word)
  "Center a word horizontally in the middle of the screen."
  (interactive "sEnter word to center: ")
  (let* ((win-width (window-width))
         (padding (/ (- win-width (length word)) 2))
         (padding-str (make-string (max 0 padding) ?\s)))
    (message "%s%s" padding-str word)))





(defun yay ()
  "Search for an Arch package and install it using yay."
  (interactive)
  (let* ((packages (split-string (shell-command-to-string "yay -Ssq") "\n" t))
         (package (completing-read "Install package: " packages nil t)))
    (when (and package (not (string= package "")))
      (async-shell-command (format "yay -S --noconfirm %s" package) "*yay-install-output*"))))



;; BUFFERS
(defun laluxx/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))


;; EVAL

;; TODO copy that appends to the clipboard

(defun laluxx/insert-or-evaluate-region ()
  "Insert the character 'e' if no active region, otherwise evaluate the region and then deactivate the region."
  (interactive)
  (if (use-region-p)
      (progn
	    (call-interactively 'laluxx/smart-eval-region)
	    (deactivate-mark))  ; Deselect the region correctly without signaling quit
    (insert "e")))

(defun laluxx/smart-eval-region (start end)
  "Evaluate the region as if it wasn't commented.
The region is from START to END."
  (interactive "r")
  (let ((content (buffer-substring start end)))
    ;; Uncomment the content temporarily
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (comment-forward (point-max))
        (uncomment-region (line-beginning-position) (line-end-position)))
      ;; Evaluate the uncommented content
      (eval-buffer)))
  (message "DONE"))  ; Moved the message here to ensure it prints after evaluation




;; TODO 'C-M-S-h' backward-mark-defun
;; TODO one backward should do -1 a mark-defun
;; (global-set-key (kbd "C-M-H") (lambda () (interactive) (mark-defun -1)))



;; EDITING

(defun mark-line (&optional arg allow-extend)
  "Set mark ARG lines away from point.
The place mark goes is the same place \\[end-of-line] would
move to with the same argument.
Interactively, if this command is repeated
or (in Transient Mark mode) if the mark is active,
it marks the next ARG lines after the ones already marked."
  (interactive "P\np")
  (cond ((and allow-extend
              (or (and (eq last-command this-command) (mark t))
                  (region-active-p)))
         (setq arg (if arg (prefix-numeric-value arg)
                     (if (< (mark) (point)) -1 1)))
         (set-mark
          (save-excursion
            (goto-char (mark))
            (forward-line arg)
            (point))))
        (t
         (push-mark
          (save-excursion
            (end-of-line (prefix-numeric-value arg))
            (point))
          nil t))))

(global-set-key (kbd "C-S-l") 'mark-line)

(defun mark-word-and-move ()
  "Mark the current word, activate the region, and move the cursor to the end of the selection."
  (interactive)
  (push-mark (point) t t)   ; Save the current position and activate the mark
  (forward-word)            ; Move the point to the end of the word
  (exchange-point-and-mark) ; Swap the positions of the point and mark
  )

(global-set-key (kbd "C-w") 'mark-word)



(defun laluxx/insert-lambda ()
  (interactive)
  (insert "λ"))

(global-set-key (kbd "C-c l") 'laluxx/insert-lambda)


(defun laluxx/kill-strings-in-region (start end)
  "Kill texts inside double quotes in the specified region."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\"\\([^\"]*\\)\"" end t)
      (replace-match "\"\"" nil nil))))

(global-set-key (kbd "C-c s") 'laluxx/kill-strings-in-region)

;; C
(defun laluxx/copy-project ()
  "Concatenate the content of all .c and .h files in the current directory and copy to the clipboard."
  (interactive)
  (let ((files (directory-files "." t "\\.[ch]$"))
        (content ""))
    (dolist (file files)
      (setq content (concat content (when (file-readable-p file)
                                       (with-temp-buffer
                                         (insert-file-contents file)
                                         (buffer-string)))
                             "\n\n"))) ; Add extra newlines between files for readability
    (unless (string= content "")
      (kill-new content)
      (message "Copied content of .c and .h files to clipboard."))))


(defun laluxx/find-header ()
  "Toggle between a C source file and its corresponding header file."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (extension (file-name-extension current-file))
         (base-name (file-name-sans-extension current-file))
         target-file)
    ;; Determine the target file based on the extension of the current file
    (setq target-file
          (cond ((string= extension "c") (concat base-name ".h"))
                ((string= extension "h") (concat base-name ".c"))
                (t (error "Not a C or Header file: %s" current-file))))
    ;; Check if the target file exists and open it
    (if (file-exists-p target-file)
        (find-file target-file)
      (message "File does not exist: %s" target-file))))

(defun laluxx/find-header-split ()
  "Toggle between a C source file and its corresponding header file in a new split window."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (extension (file-name-extension current-file))
         (base-name (file-name-sans-extension current-file))
         (target-file (cond ((string= extension "c") (concat base-name ".h"))
                            ((string= extension "h") (concat base-name ".c"))
                            (t (error "Not a C or Header file: %s" current-file)))))
    ;; Check if the target file exists
    (if (file-exists-p target-file)
        (progn
          ;; Split the window and find the file in the new window
          (split-window-right) ;; or `split-window-below` to split horizontally
          (other-window 1)
          (find-file target-file))
      (message "File does not exist: %s" target-file))))



(defun laluxx/find-package-source-code ()
  "NOTE Work only with 'elpa' opens a .el file corresponding to the extended 'word' under the cursor in the ~/.config/emacs/elpa/ directory in a new window."
  (interactive)
  (save-excursion
    (let* ((start (progn (skip-chars-backward "^ \t\n") (point)))
	   (end (progn (skip-chars-forward "^ \t\n") (point)))
	   (package-name (buffer-substring-no-properties start end))
	   (elpa-dir "~/.config/emacs/elpa/")
	   (directories (directory-files elpa-dir t "\\`[^.].*")) ; ignore hidden dirs
	   matching-dir file-path found)

      ;; Find the first directory that starts with the package name and has a version
      (dolist (dir directories found)
	(when (string-match-p (format "\\`%s-.*" (regexp-quote package-name)) (file-name-nondirectory dir))
	  (setq matching-dir dir)
	  (setq found t)))

      (when matching-dir
	;; Assuming the main .el file has the same name as the package
	(setq file-path (concat matching-dir "/" package-name ".el"))

	;; Check if the .el file exists or fallback to any .el file
	(unless (file-exists-p file-path)
	  (let ((el-files (directory-files matching-dir t "\\.el\\'")))
	    (when el-files
	      (setq file-path (car el-files)))))

	(if (file-exists-p file-path)
	    (find-file-other-window file-path)  ; Open the file in a new window if it exists
	  (message "Elisp file does not exist: %s" file-path)))
      (unless found
	(message "No directory starts with: %s" package-name)))))

(defun laluxx/clean-emacs-config ()
  "Delete all directories inside ~/.config/emacs/ not named 'scripts' or 'lisp'
   and all files not named 'init.el' or 'early-init.el'."
  (interactive)
  (let ((root-dir "~/.config/emacs/"))
    ;; Ensure the directory exists
    (when (file-directory-p root-dir)
      ;; Loop over the directory contents
      (dolist (entry (directory-files root-dir t "\\`[^.]"))
        (cond
         ;; Check if entry is a directory
         ((file-directory-p entry)
          (unless (member (file-name-nondirectory entry) '("scripts" "lisp"))
            (delete-directory entry t)))
         ;; Check for file conditions
         ((file-regular-p entry)
          (unless (member (file-name-nondirectory entry) '("init.el" "early-init.el"))
            (delete-file entry))))))))

(defun laluxx/recompile-emacs ()
  "Restart and recompile emacs"
  (interactive)
  (laluxx/clean-emacs-config)
  (restart-emacs))


;;;; WINDOW MANAGMENT
(defun swap-with-prev-window ()
  "Swap the current window with the previous one,
   open 'scratch-buffer' if only one window."
  (interactive)
  (if (= (length (window-list)) 1)
      (progn
	(split-window-below)
	(other-window 1)
	(scratch-buffer))
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
      (select-window prev)))))

(defun swap-with-next-window ()
  "Swap the current window with the next one,
   open 'eshell' if only one window."
  (interactive)
  (if (= (length (window-list)) 1)
      (progn
	(split-window-below)
	(other-window 1)
	(eshell))
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
      (select-window next)))))


(defun smart-split-down ()
  "Switch to the next window, or if only one window, split horizontally and focus below."
  (interactive)
  (if (= (length (window-list)) 1)
      (progn
	(split-window-below)
	(windmove-down))
    (other-window 1)))

(defun smart-split-up ()
  "Switch to the previous window, or if only one window, split horizontally and focus above."
  (interactive)
  (if (= (length (window-list)) 1)
      (progn
	(split-window-below))
    (other-window -1)))

(defun smart-split-left ()
  "If there is only one window, split vertically and focus left. Otherwise, shrink window horizontally."
  (interactive)
  (if (= (length (window-list)) 1)
      (progn
	(split-window-right))
    (if (window-at-side-p (selected-window) 'right)
	(enlarge-window-horizontally 5)
      (shrink-window-horizontally 5))))

(defun smart-split-right ()
  "If there is only one window, split vertically and focus right. Otherwise, enlarge window horizontally."
  (interactive)
  (if (= (length (window-list)) 1)
      (progn
	(split-window-right)
	(windmove-right))
    (if (window-at-side-p (selected-window) 'right)
	(shrink-window-horizontally 5)
      (enlarge-window-horizontally 5))))

(defun smart-delete-window ()
  "Delete the current window, or kill the buffer if it's the only window."
  (interactive)
  (if (> (length (window-list)) 1)
      (delete-window)
    (kill-buffer (current-buffer))))

(global-set-key (kbd "M-j") 'smart-split-down)
(global-set-key (kbd "M-k") 'smart-split-up)
(global-set-key (kbd "M-h") 'smart-split-left)
(global-set-key (kbd "M-l") 'smart-split-right)
(global-set-key (kbd "M-J") 'swap-with-next-window)
(global-set-key (kbd "M-K") 'swap-with-prev-window)
(global-set-key (kbd "M-q") 'smart-delete-window)


(use-package eyebrowse
  :ensure t
  :config
  (setq eyebrowse-tagged-slot-format "%t")
  (eyebrowse-mode t)
  (defvar workspace-3-eshell-initialized nil
    "Flag to check if eshell has been initialized in workspace 3.")

  (defun switch-to-workspace-3-and-maybe-start-eshell ()
    "Close any open eshell in the current workspace, then switch to workspace 3,
   and start eshell if it hasn't been started yet, closing other windows."
    (interactive)
    ;; Close eshell windows in the current workspace
    (dolist (win (window-list))
      (when (eq 'eshell-mode (buffer-local-value 'major-mode (window-buffer win)))
	(delete-window win)))

    (eyebrowse-switch-to-window-config 3)
    ;; Start eshell if not already initialized and manage windows
    (unless workspace-3-eshell-initialized
      (setq workspace-3-eshell-initialized t)
      (eyebrowse-rename-window-config 3 "λ")
      (eshell)
      (delete-other-windows))

    ;; Ensure only eshell windows are open in this workspace
    (let ((only-eshell t))
      (walk-windows (lambda (w)
		      (unless (eq 'eshell-mode (buffer-local-value 'major-mode (window-buffer w)))
			(setq only-eshell nil)))
		    nil t)  ; t indicates to walk windows in the current frame only
      (unless only-eshell
	(mapc (lambda (w)
		(unless (eq 'eshell-mode (buffer-local-value 'major-mode (window-buffer w)))
		  (delete-window w)))
	      (window-list nil 'no-minibuf)))))  ; Avoid minibuffer windows

  (global-set-key (kbd "M-1") (lambda () (interactive) (eyebrowse-switch-to-window-config 1)))
  (global-set-key (kbd "M-2") (lambda () (interactive) (eyebrowse-switch-to-window-config 2)))
  (global-set-key (kbd "M-3") 'switch-to-workspace-3-and-maybe-start-eshell)
  (global-set-key (kbd "M-4") (lambda () (interactive) (eyebrowse-switch-to-window-config 4)))
  (global-set-key (kbd "M-5") (lambda () (interactive) (eyebrowse-switch-to-window-config 5)))
  (global-set-key (kbd "M-6") (lambda () (interactive) (eyebrowse-switch-to-window-config 6)))
  (global-set-key (kbd "M-7") (lambda () (interactive) (eyebrowse-switch-to-window-config 7)))
  (global-set-key (kbd "M-8") (lambda () (interactive) (eyebrowse-switch-to-window-config 8)))
  (global-set-key (kbd "M-9") (lambda () (interactive) (eyebrowse-switch-to-window-config 9)))
  (global-set-key (kbd "M-TAB") 'eyebrowse-last-window-config))


;; MONOCLE

(defvar laluxx/window-configuration nil
  "Current window configuration.
Intended for use by `laluxx/window-monocle'.")

(define-minor-mode laluxx/window-single-toggle
  "Toggle between multiple windows and single window.
This is the equivalent of maximizing a window. Tiling window
managers such as DWM, BSPWM refer to this state as 'monocle'."
  :lighter " [M]"
  :global nil
  (if (one-window-p)
      (when laluxx/window-configuration
        (set-window-configuration laluxx/window-configuration))
    (setq laluxx/window-configuration (current-window-configuration))
    (delete-other-windows)))

(global-set-key (kbd "M-SPC") 'laluxx/window-single-toggle)





;; EXWM TODO

;; (defun efs/exwm-update-class ()
;;   (exwm-workspace-rename-buffer exwm-class-name))

;; (use-package exwm
;;   :config
;;   ;; Set the default number of workspaces
;;   (setq exwm-workspace-number 5)

;;   ;; When window "class" updates, use it to set the buffer name
;;   (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

;;   ;; Rebind CapsLock to Ctrl
;;   (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")

;;   ;; Set the screen resolution (update this to be the correct resolution for your screen!)
;;   (require 'exwm-randr)
;;   (exwm-randr-enable)
;;   ;; (start-process-shell-command "xrandr" nil "xrandr --output Virtual-1 --primary --mode 2048x1152 --pos 0x0 --rotate normal")

;;   ;; Load the system tray before exwm-init
;;   (require 'exwm-systemtray)
;;   (exwm-systemtray-enable)

;;   ;; These keys should always pass through to Emacs
;;   (setq exwm-input-prefix-keys
;;     '(?\C-x
;;       ?\C-u
;;       ?\C-h
;;       ?\M-x
;;       ?\M-`
;;       ?\M-&
;;       ?\M-:
;;       ?\C-\M-j  ;; Buffer list
;;       ?\C-\ ))  ;; Ctrl+Space

;;   ;; Ctrl+Q will enable the next key to be sent directly
;;   (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

;;   ;; Set up global key bindings.  These always work, no matter the input state!
;;   ;; Keep in mind that changing this list after EXWM initializes has no effect.
;;   (setq exwm-input-global-keys
;;         `(
;;           ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
;;           ([?\s-r] . exwm-reset)

;;           ;; Move between windows
;;           ([s-left] . windmove-left)
;;           ([s-right] . windmove-right)
;;           ([s-up] . windmove-up)
;;           ([s-down] . windmove-down)

;;           ;; Launch applications via shell command
;;           ([?\s-&] . (lambda (command)
;;                        (interactive (list (read-shell-command "$ ")))
;;                        (start-process-shell-command command nil command)))

;;           ;; Switch workspace
;;           ([?\s-w] . exwm-workspace-switch)
;;           ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

;;           ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
;;           ,@(mapcar (lambda (i)
;;                       `(,(kbd (format "s-%d" i)) .
;;                         (lambda ()
;;                           (interactive)
;;                           (exwm-workspace-switch-create ,i))))
;;                     (number-sequence 0 9))))

;;   (exwm-enable))

;;;; SCRATCH BUFFER
(defun save-scratch-buffer-to-file ()
  "Save the contents of *scratch* buffer to a specific file, ensuring directory exists."
  (let ((scratch-file "~/.config/emacs/scratch"))
    (unless (file-exists-p (file-name-directory scratch-file))
      (make-directory (file-name-directory scratch-file) t))
    (with-current-buffer "*scratch*"
      (write-region (point-min) (point-max) scratch-file))))


(defun load-scratch-buffer-from-file ()
  "Load the contents of *scratch* buffer from a specific file."
  (let ((scratch-file "~/.config/emacs/scratch"))
    (when (file-exists-p scratch-file)
      (with-current-buffer "*scratch*"
	(erase-buffer)
	(insert-file-contents scratch-file)
	(text-scale-set 0)))))

(add-hook 'kill-emacs-hook 'save-scratch-buffer-to-file)
(add-hook 'emacs-startup-hook 'load-scratch-buffer-from-file)


;;;; GOOGLE THIS
(defun laluxx/google-this (query)
  "Search QUERY using Firefox."
  (interactive "sGoogle: ")
  (shell-command (concat "xdg-open 'https://www.google.com/search?q="
                         (url-hexify-string query) "'")))

(global-set-key (kbd "C-x g") 'laluxx/google-this)


;;;; ELISP OUTLINE
(define-derived-mode elisp-outline-mode emacs-lisp-mode "Elisp Outline"
  "Major mode for Elisp files with enhanced outline capabilities based on comment headers.")

(defun elisp-outline-setup ()
  (setq-local outline-regexp "^;;;+\\s-")  ;; Recognize 3 or more semicolons followed by a space
  (setq-local outline-level
              (lambda ()
                (- (string-width (match-string 0)) 2)))  ;; The outline level is the number of semicolons minus two
  (outline-minor-mode 1))  ;; Enable outline-minor-mode within elisp-outline-mode

(add-hook 'elisp-outline-mode-hook 'elisp-outline-setup)  ;; Add the setup function to the mode's hook

(defun outline-next-heading-same-level ()
  (interactive)
  (outline-next-heading)
  (while (and (not (bobp)) (> (funcall outline-level) 2))
    (outline-next-heading)))

(defun outline-previous-heading-same-level ()
  (interactive)
  (outline-previous-heading)
  (while (and (not (eobp)) (> (funcall outline-level) 2))
    (outline-previous-heading)))

(defun outline-toggle-children-improved ()
  (interactive)
  (outline-toggle-children)
  (save-excursion
    (let ((level (funcall outline-level)))
      (outline-next-heading)
      (while (and (not (eobp)) (> (funcall outline-level) level))
        (outline-toggle-children)
        (outline-next-heading)))))


(defvar elisp-outline-all-collapsed nil
  "State tracking whether all top-level headings are currently collapsed.")

(defun elisp-outline-toggle-all-top-level ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if elisp-outline-all-collapsed
        (while (re-search-forward "^;;;;\\s-" nil t)
          (outline-show-subtree))
      (goto-char (point-min))
      (while (re-search-forward "^;;;;\\s-" nil t)
        (outline-hide-subtree)))
    (setq elisp-outline-all-collapsed (not elisp-outline-all-collapsed))))

(define-key elisp-outline-mode-map (kbd "TAB") 'outline-toggle-children-improved)
(define-key elisp-outline-mode-map (kbd "<backtab>") 'elisp-outline-toggle-all-top-level)
(define-key elisp-outline-mode-map (kbd "C-c C-n") 'outline-next-heading-same-level)
(define-key elisp-outline-mode-map (kbd "C-c C-p") 'outline-previous-heading-same-level)
(define-key elisp-outline-mode-map (kbd "C-c C-u") 'outline-up-heading)



;;;; EWW


(defun my-eww-mode-setup ()
  "Set up EWW with custom settings."
  (olivetti-mode 1)
  (olivetti-set-width 120))

(add-hook 'eww-mode-hook 'my-eww-mode-setup)


;;;; PROFILER

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
   '(all-the-icons-completion catppuccin-theme consult-flycheck
                              consult-lsp corfu diminish diredfl
                              doom-modeline drag-stuff dune ef-themes
                              elfeed embark-consult esh-help
                              eshell-prompt-extras eshell-z evil
                              ewal-doom-themes eyebrowse geiser-guile
                              general glsl-mode haskell-mode helpful
                              hl-todo iedit iscroll json-mode
                              kaolin-themes kind-icon ligature lsp-ui
                              magit marginalia multiple-cursors
                              nerd-icons-dired olivetti orderless
                              page-break-lines popper
                              rainbow-delimiters rainbow-mode rustic
                              shut-up sly smooth-scrolling
                              spacemacs-theme spacious-padding suggest
                              theme-magic timu-caribbean-theme tuareg
                              vertico which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t :background "#212121")))
 '(header-line ((t :box (:line-width 4 :color "grey20" :style nil))))
 '(header-line-highlight ((t :box (:color "#C5C8C6"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "#212121")))
 '(mode-line ((t :box (:line-width 6 :color "grey75" :style nil))))
 '(mode-line-active ((t :box (:line-width 6 :color "grey75" :style nil))))
 '(mode-line-highlight ((t :box (:color "#C5C8C6"))))
 '(mode-line-inactive ((t :box (:line-width 6 :color "grey30" :style nil))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "grey85" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "grey75" :style nil))))
 '(tab-line-tab ((t)))
 '(tab-line-tab-active ((t)))
 '(tab-line-tab-inactive ((t)))
 '(vertical-border ((t :background "#212121" :foreground "#212121")))
 '(window-divider ((t (:background "#212121" :foreground "#212121"))))
 '(window-divider-first-pixel ((t (:background "#212121" :foreground "#212121"))))
 '(window-divider-last-pixel ((t (:background "#212121" :foreground "#212121")))))

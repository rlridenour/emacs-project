;;; init.el -*- lexical-binding: t; -*-

(setq user-full-name "Randy Ridenour")
(setq user-mail-address "rlridenour@fastmail.com")

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(recentf-mode)
  (setopt recentf-max-menu-items 1000
  recentf-max-saved-items 1000)

(global-set-key (kbd "C-x c") #'save-buffers-kill-emacs)

(use-package modus-themes
  :ensure
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
	  modus-themes-mixed-fonts t
	  modus-themes-variable-pitch-ui t
	  modus-themes-italic-constructs t
	  modus-themes-bold-constructs t)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
	  modus-themes-preset-overrides-faint)

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi t)
  :bind
  ("<f9>" . #'modus-themes-rotate))

;; Main typeface
(set-face-attribute 'default nil :family "SF Mono" :height 160 :weight 'medium)
;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "SF Pro Text" :height 1.0 :weight 'medium)
;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "SF Mono" :height 1.0 :weight 'medium)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(use-package ace-window
    :ensure
    :config
(setq aw-dispatch-always t)
    :bind
    (("M-O" . #'ace-window)
     ("M-o" . #'rlr/quick-window-jump)))

(defun rlr/quick-window-jump ()
"If only one window, switch to previous buffer, otherwise call ace-window."
    (interactive)
    (let* ((window-list (window-list nil 'no-mini)))
      (if (< (length window-list) 3)
	  ;; If only one window, switch to previous buffer. If only two, jump directly to other window.
	  (if (one-window-p)
	  (switch-to-buffer nil)
	(other-window 1))
	(ace-window t))))

(use-package vertico
  :ensure
  :demand
  :custom (vertico-cycle t)
  :config
  (setf (car vertico-multiline) "\n") ;; don't replace newlines
  (vertico-mode)
  ;; (setq vertico-multiform-commands
  ;;  '((consult-line
  ;;       posframe
  ;;       (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
  ;;       (vertico-posframe-border-width . 10)
  ;;       ;; NOTE: This is useful when emacs is used in both in X and
  ;;       ;; terminal, for posframe do not work well in terminal, so
  ;;       ;; vertico-buffer-mode will be used as fallback at the
  ;;       ;; moment.
  ;;       (vertico-posframe-fallback-mode . vertico-buffer-mode))
  ;;      (t posframe)))
  (vertico-multiform-mode 1)
  (setq vertico-multiform-categories
	  '((file grid)
	    ;; (jinx grid (vertico-grid-annotate . 20))
	    ;; (citar buffer)
	    )
	  )
  (setq vertico-cycle t) ;; enable cycling for 'vertico-next' and 'vertico-prev'
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  :bind
  (:map vertico-map
	      ;; keybindings to cycle through vertico results.
	      ("C-h" . #'+minibuffer-up-dir)
	      ("<backspace>" . #'vertico-directory-delete-char)
	      ("RET" . #'vertico-directory-enter)))

(use-package orderless
  :ensure
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure
  :demand
  :bind
  (("C-x b" . #'consult-buffer)
   ("s-f" . #'consult-line)
  ("s-r" . #'consult-buffer)
  ("M-y" . #'consult-yank-pop)))

(defun rlr/consult-rg ()
  "Function for `consult-ripgrep' with the `universal-argument'."
  (interactive)
  (consult-ripgrep (list 4)))

(defun rlr/consult-fd ()
  "Function for `consult-find' with the `universal-argument'."
  (interactive)
  (consult-find (list 4)))

(use-package org-auto-tangle
  :ensure t
  :hook
  (org-mode . org-auto-tangle-mode))

(use-package yasnippet
  :ensure t
  :custom
  (yas-snippet-dirs '("~/.config/emacs/snippets"))
  :config
  (yas-global-mode))

(use-package major-mode-hydra
  :ensure
  :commands
  (pretty-hydra-define)
  :bind
  ("s-m" . #'major-mode-hydra))

(pretty-hydra-define hydra-toggle
       (:color teal :quit-key "q" :title "Toggle")
       (" "
	(("a" abbrev-mode "abbrev" :toggle t)
	 ("b" toggle-debug-on-error "debug" (default value 'debug-on-error))
	 ("d" global-devil-mode "devil" :toggle t)
	 ("e" evil-mode "evil" :toggle t)
	 ("i" aggressive-indent-mode "indent" :toggle t)
	 ("f" auto-fill-mode "fill" :toggle t)
	 ("l" display-line-numbers-mode "linum" :toggle t)
	 ("m" variable-pitch-mode "variable-pitch" :toggle t)
	 ("p" electric-pair-mode "electric-pair" :toggle t)
	 ("t" toggle-truncate-lines "truncate" :toggle t)
	 ("s" whitespace-mode "whitespace" :toggle t))
	" "
	(("c" cdlatex-mode "cdlatex" :toggle t)
	 ("o" olivetti-mode "olivetti" :toggle t)
	 ("r" read-only-mode "read-only" :toggle t)
	 ("v" view-mode "view" :toggle t)
	 ("W" wc-mode "word-count" :toggle t)
	 ("S" auto-save-visited-mode "auto-save" :toggle t)
	 ("C" cua-selection-mode "rectangle" :toggle t))))

(pretty-hydra-define hydra-buffer
       (:color teal :quit-key "q" :title "Buffers and Files")
       ("Open"
	(("b" ibuffer "ibuffer")
	 ("m" consult-bookmark "bookmark")
	 ("w" consult-buffer-other-window "other window")
	 ("f" consult-buffer-other-frame "other frame")
	 ("d" crux-recentf-find-directory "recent directory")
	 ("a" crux-open-with "open in default app"))
	"Actions"
	(("D" crux-delete-file-and-buffer "delete file")
	 ("R" crux-rename-file-and-buffer "rename file")
	 ("K" rlr/kill-other-buffers "kill other buffers")
	 ("N" nuke-all-buffers "Kill all buffers")
	 ("c" crux-cleanup-buffer-or-region "fix indentation"))
	"Misc"
	(("t" crux-visit-term-buffer "ansi-term")
	 ("T" iterm-goto-filedir-or-home "iTerm2")
	 ("i" crux-find-user-init-file "init.el")
	 ("s" crux-find-shell-init-file "fish config"))
	))

(pretty-hydra-define hydra-locate
       (:color teal :quit-key "q" title: "Search")
       ("Buffer"
	(("c" pulsar-highlight-dwim "find cursor")
	 ("h" consult-org-heading "org heading")
	 ("l" consult-goto-line "goto-line")
	 ("i" consult-imenu "imenu")
	 ("m" consult-mark "mark")
	 ("o" consult-outline "outline"))
	"Global"
	(("M" consult-global-mark "global-mark")
	 ("n" consult-notes "notes")
	 ("r" consult-ripgrep "ripgrep")
	 ("d" rlr/consult-rg "rg from dir")
	 ("f" rlr/consult-fd "find from dir"))
	"Files"
	(("e" rr/open-init-file "Emacs init")
	 ("s" goto-shell-init "Fish functions"))
	))

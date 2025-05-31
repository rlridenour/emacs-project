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

(use-package transient
  :ensure)

(use-package hl-todo
  :ensure)

(use-package magit
  :ensure
  :custom
  (magit-diff-refine-hunk 'all)
  :config
  (transient-bind-q-to-quit)
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

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

(pretty-hydra-define hydra-window
       (:color teal :quit-key "q" title: "Windows")
       ("Windows"
	(("w" other-window "cycle windows" :exit nil)
	 ("a" ace-window "ace window")
	 ("m" minimize-window "minimize window")
	 ("s" transpose-windows "swap windows")
	 ("S" shrink-window-if-larger-than-buffer "shrink to fit")
	 ("b" balance-windows "balance windows")
	 ("t" toggle-window-split "toggle split")
	 ("T" enlarge-window" grow taller" :exit nil)
	 ("G" enlarge-window-horizontally "grow wider" :exit nil)
	 ("o" delete-other-windows "kill other windows"))
	"Frames"
	(("M" iconify-frame "minimize frame")
	 ("d" delete-other-frames "delete other frames")
	 ("D" delete-frame "delete this frame")
	 ("i" make-frame-invisible "invisible frame")
	 ("f" toggle-frame-fullscreen "fullscreen")
	 ("n" make-frame-command "new frame"))
	"Writeroom"
	(("W" writeroom-mode "toggle writeroom")
	 ("M" writeroom-toggle-mode-line "toggle modeline"))))

(pretty-hydra-define hydra-new
  (:color teal :quit-key "q" title: "New")
  ("Frame"
	(("f" make-frame-command "new frame"))
	"Denote"
	(("c" org-capture "capture")
	 ("n" denote "note")
	 ("v" denote-menu-list-notes "view notes")
	 ("j" denote-journal-extras-new-or-existing-entry "journal"))
	"Writing"
	(("b" rlrt-new-post "blog post")
	 ("a" rlrt-new-article "article"))
	"Teaching"
	(("l" rlrt-new-lecture "lecture")
	 ("h" rlrt-new-handout "handout")
	 ("s" rlrt-new-syllabus "syllabus"))
	))

(pretty-hydra-define hydra-logic
  (:color pink :quit-key "0" :title "Logic")
  ("Operators"
	(
	 ;; ("1" (rr/insert-unicode "NOT SIGN") "¬")
	 ("1" (rr/insert-unicode "TILDE OPERATOR") "∼")
	 ;; ("2" (rr/insert-unicode "AMPERSAND") "&")
	 ("2" (rr/insert-unicode "BULLET") "•")
	 ("3" (rr/insert-unicode "LOGICAL OR") "v")
	 ("4" (rr/insert-unicode "SUPERSET OF") "⊃")
	 ;; ("4" (rr/insert-unicode "RIGHTWARDS ARROW") "→")
	 ("5" (rr/insert-unicode "IDENTICAL TO") "≡")
	 ;; ("5" (rr/insert-unicode "LEFT RIGHT ARROW") "↔")
	 ("6" (rr/insert-unicode "THERE EXISTS") "∃")
	 ("7" (rr/insert-unicode "FOR ALL") "∀")
	 ("8" (rr/insert-unicode "WHITE MEDIUM SQUARE") "□")
	 ("9" (rr/insert-unicode "LOZENGE") "◊")
	 ("`" (rr/insert-unicode "NOT EQUAL TO") "≠"))
	"Space"
	(("?" (rr/insert-unicode "MEDIUM MATHEMATICAL SPACE") "Narrow space"))
	"Quit"
	(("0" quit-window "quit" :color blue))
	))

(pretty-hydra-define hydra-math
  (:color pink :quit-key "?" :title "Math")
  ("Operators"
	(("1" (rr/insert-unicode "NOT SIGN") "¬")
	 ("2" (rr/insert-unicode "AMPERSAND") "&")
	 ("3" (rr/insert-unicode "LOGICAL OR") "v")
	 ("4" (rr/insert-unicode "RIGHTWARDS ARROW") "→")
	 ("5" (rr/insert-unicode "LEFT RIGHT ARROW") "↔")
	 ("6" (rr/insert-unicode "THERE EXISTS") "∃")
	 ("7" (rr/insert-unicode "FOR ALL") "∀")
	 ("8" (rr/insert-unicode "WHITE MEDIUM SQUARE") "□")
	 ("9" (rr/insert-unicode "LOZENGE") "◊"))
	"Sets"
	(("R" (rr/insert-unicode "DOUBLE-STRUCK CAPITAL R") "ℝ real")
	 ("N" (rr/insert-unicode "DOUBLE-STRUCK CAPITAL N") "ℕ natural")
	 ("Z" (rr/insert-unicode "DOUBLE-STRUCK CAPITAL Z") "ℤ integer")
	 ("Q" (rr/insert-unicode "DOUBLE-STRUCK CAPITAL Q") "ℚ rational")
	 ("Q" (rr/insert-unicode "DOUBLE-STRUCK CAPITAL Q") "ℚ rational")
	 ("Q" (rr/insert-unicode "DOUBLE-STRUCK CAPITAL Q") "ℚ rational")
	 )
	"Space"
	(("?" (rr/insert-unicode "MEDIUM MATHEMATICAL SPACE") "Narrow space"))
	"Quit"
	(("?" quit-window "quit" :color blue))
	))

(pretty-hydra-define hydra-hydras
  (:color teal :quit-key "q" :title "Hydras")
  ("System"
	(("t" hydra-toggle/body)
	 ("b" hydra-buffer/body)
	 ("h" hydra-hugo/body)
	 ("p" powerthesaurus-hydra/body))
	"Unicode"
	(("l" hydra-logic/body "logic")
	 ("m" hydra-math/body))))

(major-mode-hydra-define dashboard-mode
       (:quit-key "q")
       ("Open"
	(("m" consult-bookmark "bookmarks")
	 ("a" consult-org-agenda "consult-agenda")
	 ("t" (find-file "/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/tasks.org") "open tasks")
	 ("b" (find-file "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/bookmarks.org") "web bookmarks"))))

(major-mode-hydra-define org-agenda-mode
       (:quit-key "q")
       ("Open"
	(
	 ("a" consult-org-agenda "consult-agenda")
	 ("b" consult-bookmark "bookmarks")
	 ("m" mu4e "rlr/read-mail-news")
	 ("t" (find-file "/Users/rlridenour/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/tasks.org") "open tasks")
	 ("w" (find-file "/Users/rlridenour/Library/Mobile Documents/com~apple~CloudDocs/org/bookmarks.org") "web bookmarks"))
	"Classes"
	(("1" (dired "~/icloud/teaching/intro/lectures") "Intro")
	 ("2" (dired "~/icloud/teaching/medieval/lectures") "Medieval")
	 ("3" (dired "~/icloud/teaching/logic/lectures") "Logic")
	 ("4" (dired "~/icloud/teaching/language/lectures") "Language")
	 )
	"Actions"
	(("s" rlr/save-web-page-as-org-file "Save Safari page as Org")
	 ("e" rlr/open-safari-page-in-eww "Open Safari page in EWW"))
	))

(major-mode-hydra-define eww-mode
       (:quit-key "q")
       ("A"
	(
	 ;; ("G" eww "Eww Open Browser")
	 ("g" eww-reload "Eww Reload")
	 ("6" eww-open-in-new-buffer "Open in new buffer")
	 ("l" eww-back-url "Back Url")
	 ("r" eww-forward-url "Forward Url")
	 ("N" eww-next-url "Next Url")
	 ("P" eww-previous-url "Previous Url")
	 ("u" eww-up-url "Up Url")
	 ("&" eww-browse-with-external-browser "Open in External Browser")
	 ("d" eww-download "Download")
	 ("w" eww-copy-page-url "Copy Url Page")
	 );end theme
	"B"
	(
	 ("T" rlr/eww-toggle-images "Toggle Image Display")
	 (">" shr-next-link "Shr Next Link")
	 ("<" shr-previous-link "Shr Previous Link")
	 ("n" scroll-down-command "Scroll Down")
	 ("C" url-cookie-list "Url Cookie List")
	 ("v" eww-view-source "View Source")
	 ("R" eww-readable "Make Readable")
	 ("H" eww-list-histories "List History")
	 ("E" eww-set-character-encoding "Character Encoding")
	 ("s" eww-switch-to-buffer "Switch to Buffer")
	 ("S" eww-list-buffers "List Buffers")
	 );end highlighting

	"C"
	(
	 ("1" rrnet "randyridenour.net")
	 ("2" sep "SEP")
	 ("F" eww-toggle-fonts "Toggle Fonts")
	 ("D" eww-toggle-paragraph-direction "Toggle Paragraph Direction")
	 ("c" eww-toggle-colors "Toggle Colors")
	 ("b" eww-add-bookmark "Add Bookmark")
	 ("B" eww-list-bookmarks "List Bookmarks")
	 ("=" eww-next-bookmark "Next Bookmark")
	 ("-" eww-previous-bookmark "Previous Bookmark")
	 ("O" jao-eww-to-org "Make Org Version")
	 ("<SPC>" nil "Quit" :color pink)
	 );end other
	))

(major-mode-hydra-define markdown-mode
       (:quit-key "q")
       ("Format"
	(("h" markdown-insert-header-dwim "header")
	 ("l" markdown-insert-link "link")
	 ("u" markdown-insert-uri "url")
	 ("f" markdown-insert-footnote "footnote")
	 ("w" markdown-insert-wiki-link "wiki")
	 ("r" markdown-insert-reference-link-dwim "r-link")
	 ("n" markdown-cleanup-list-numbers "clean-lists")
	 ("c" markdown-complete-buffer "complete"))))

(major-mode-hydra-define LaTeX-mode
       (:quit-key "q")
       ("Bibtex"
	(("r" citar-insert-citation "citation"))
	"LaTeXmk"
	(("m" rlr/tex-mkpdf "PDFLaTeX")
	 ("l" rlr/tex-mklua "LuaLaTeX")
	 ("w" rlr/tex-mktc "watch PDFLaTeX")
	 ("L" rlr/tex-mklua "watch LuaLaTeX")
	 ("c" tex-clean "clean aux")
	 ("C" tex-clean-all "clean all")
	 ("n" latex-word-count "word count"))))

(major-mode-hydra-define org-mode
       (:quit-key "q")
       ("Export"
	(("m" rlr/org-mkpdf "Make PDF with PDFLaTeX")
	 ("p" rlr/org-open-pdf "View PDF")
	 ("h" make-html "HTML")
	 ("l" rlr/org-mklua "Make PDF with LuaLaTeX")
	 ("el" org-latex-export-to-latex "Org to LaTeX")
	 ("eb" org-beamer-export-to-pdf "Org to Beamer-PDF")
	 ("eB" org-beamer-export-to-latex "Org to Beamer-LaTeX")
	 ("s" lecture-slides "Lecture slides")
	 ("n" lecture-notes "Lecture notes")
	 ("ep" present "Present slides")
	 ("ec" canvas-copy "Copy HTML for Canvas")
	 ("es" canvas-notes "HTML Canvas notes")
	 ("eS" make-syllabus "Syllabus")
	 ("eh" make-handout "Handout")
	 ("c" tex-clean "clean aux")
	 ("C" tex-clean-all "clean all"))
	"Edit"
	(("a" org-appear-mode :toggle t)
	 ("dd" org-deadline "deadline")
	 ("ds" org-schedule "schedule")
	 ("r" org-refile "refile")
	 ("du" rlr/org-date "update date stamp")
	 ;; ("fn" org-footnote-new "insert footnote")
	 ("ff" org-footnote-action "edit footnote")
	 ("fc" citar-insert-citation "citation")
	 ("il" org-mac-link-safari-insert-frontmost-url "insert safari link")
	 ("y" yankpad-set-category "set yankpad"))
	"View"
	(("vi" consult-org-heading "iMenu")
	 ("vu" org-toggle-pretty-entities "org-pretty")
	 ("vI" org-toggle-inline-images "Inline images"))
	"Blog"
	(("bn" rlrt-new-post "New draft")
	 ("bt" orgblog-add-tag "Add tag")
	 ("bi" orgblog-insert-image "Insert image")
	 ("bp" orgblog-publish-draft "Publish draft")
	 ("bb" orgblog-build "Build site")
	 ("bs" orgblog-serve "Serve site")
	 ("bd" orgblog-push "Push to Github"))
	"Notes"
	(("1" denote-link "link to note"))))

(major-mode-hydra-define dired-mode
       (:quit-key "q")
       ("New"
	(("a" rlrt-new-article "article")
	 ("l" rlrt-new-lecture "lecture")
	 ("h" rlrt-new-handout "handout")
	 ("s" rlrt-new-syllabus "syllabus"))
	"Tools"
	(("d" crux-open-with "Open in default program")
	 ("." dired-omit-mode "Show hidden files")
	 ("p" diredp-copy-abs-filenames-as-kill "Copy filename and path")
	 ("n" dired-toggle-read-only "edit Filenames"))
	"Blog"
	(("bn" rlrt-new-post "New draft")
	 ("bb" orgblog-build "Build Site")
	 ("bs" orgblog-serve "Serve Site")
	 ("bd" orgblog-push "Push to Github"))))

(major-mode-hydra-define css-mode
       (:quit-key "q")
       ("Blog"
	(("bn" rlrt-new-post "New draft")
	 ("bb" orgblog-build "Build Site")
	 ("bs" orgblog-serve "Serve Site")
	 ("bd" orgblog-push "Push to Github"))))

(major-mode-hydra-define denote-menu-mode
       (:quit-key "q")
       ("Tools"
	(("f" denote-menu-filter "Filter by regex")
	 ("k" denote-menu-filter-by-keyword "Filter by keyword")
	 ("c" denote-menu-clear-filters "Clear filters")
	 ("d" denote-menu-export-to-dired "Dired"))))

(major-mode-hydra-define mu4e-main-mode
       (:quit-key "q")
       ("Message"
	(
	 ("n" mu4e-compose-mail "New")
	 ("e" mu4e-view-save-attachments "Save attachments")
	 )))

(major-mode-hydra-define mu4e-headers-mode
       (:quit-key "q")
       ("Message"
	(
	 ("n" mu4e-compose-mail "New")
	 ("r" mu4e-compose-reply "Reply")
	 ("a"  mu4e-compose-wide-reply "Reply All")
	 )))

(major-mode-hydra-define mu4e-view-mode
       (:quit-key "q")
       ("Message"
	(
	 ("n" mu4e-compose-mail "New")
	 ("r" mu4e-compose-reply "Reply")
	 ("a"  mu4e-compose-wide-reply "Reply All")
	 )
	"Browser"
	(
	 ("bd" rlr/browser-default "System default")
	 ("bq" rlr/browser-qutebrowser "Qutebrowser")
	 ("be" rlr/browser-eww "EWW")
	 )))

(major-mode-hydra-define mu4e-compose-mode
       (:quit-key "q")
       ("Compose with Org"
	(
	 ("o" org-mime-edit-mail-in-org-mode "Edit in org")
	 ("r" org-mime-htmlize "Org to HTML")
	 )))

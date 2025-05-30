;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Disable GUI
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; Disable bell sound
(setq ring-bell-function 'ignore)

;; Don't recenter when scrolling off screen
(setq scroll-conservatively 10
	scroll-margin 0)

;; Use empty scratch buffers
(setq-default initial-scratch-message nil)

(setq use-dialog-box nil)
(setq use-file-dialog nil)

;; Hide the startup screen
(setq inhibit-startup-screen t)

;; Increase font size
(set-face-attribute 'default nil :height 160)

;; Make frame fit the screen
(setq frame-resize-pixelwise t)
(setq-default frame-inhibit-implied-resize t)

;; Start with maximized screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Show path of file in frame title
(setq frame-title-format
	'(buffer-file-name (:eval (abbreviate-file-name buffer-file-name))
			   (dired-directory dired-directory
					    "%b")))

;; Bring new frame to front
(defun rlr/bring-emacs-to-front ()
  "Using applescript, force the Emacs frame to be activated."
  (when (eq system-type 'darwin)
    (start-process "bring-emacs-to-front" nil
		     "osascript"
		     "-e"
		     "tell application \"Emacs\" to activate")))
(add-hook 'server-after-make-frame-hook #'rlr/bring-emacs-to-front)


(defun rlr/focus-new-client-frame ()
  (select-frame-set-input-focus (selected-frame)))
(add-hook 'server-after-make-frame-hook #'rlr/focus-new-client-frame)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

;;; early-init.el ends here

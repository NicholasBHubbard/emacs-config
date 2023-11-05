;;;; init.el -*- lexical-binding: t; -*-

(setq debug-on-error t)
(add-hook 'after-init-hook #'(lambda () (setq debug-on-error nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     DEFAULT PACKAGES                      ;;
;; All non-default packages can assume that default packages ;;
;; are already loaded. The same is not true for deafult      ;;
;; packages, which means that default package definition     ;;
;; order may be relevant. The end of the default package     ;;
;; definitions is marked by a comment similar to this one.   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; STRAIGHT

(defvar bootstrap-version)
(let ((bootstrap-file
	   (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	  (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
	(with-current-buffer
		(url-retrieve-synchronously
		 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
		 'silent 'inhibit-cookies)
	  (goto-char (point-max))
	  (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;; ENV VARS

;; (use-package exec-path-from-shell
;;   :straight t
;;   :config
;;   (exec-path-from-shell-initialize))

;;; THEME

(use-package alect-themes
  :straight t
  :config
  (load-theme 'alect-black t))

(set-face-attribute 'region nil :background "#737")

;; ;; dark mode
;; (invert-face 'default)

;;; GENERAL

(use-package general
  :straight t
  :config
  (general-auto-unbind-keys)

  (general-create-definer file-key-def
	:states '(normal visual)
	:prefix "SPC f")
  (general-define-key
   :states '(normal visual)
   "SPC f" '(nil :wk "file-prefix"))

  (general-create-definer buffer-key-def
	:states '(normal visual)
	:prefix "SPC b")
  (general-define-key
   :states '(normal visual)
   "SPC b" '(nil :wk "buffer-prefix"))

  (general-create-definer window-key-def
	:states '(normal visual)
	:prefix "SPC w")
  (general-define-key
   :states '(normal visual)
   "SPC w" '(nil :wk "window-prefix"))

  (general-create-definer text-editing-key-def
	:states '(normal visual)
	:prefix "SPC SPC")
  (general-define-key
   :states '(normal visual)
   "SPC SPC" '(nil :wk "text-editing-prefix"))

  (general-create-definer org-key-def
	:states '(normal visual)
	:keymaps 'org-mode-map
	:prefix "SPC o")
  (general-define-key
   :states '(normal visual)
   :keymaps 'org-mode-map
   "SPC o" '(nil :wk "org-prefix"))

  (general-create-definer misc-key-def
	:states '(normal visual)
	:prefix "SPC z")
  (general-define-key
   :states '(normal visual)
   "SPC z" '(nil :wk "misc-prefix")))

;;; KEY CHORD

(use-package key-chord
  :straight t
  :custom
  (key-chord-two-keys-delay 0.15)
  (key-chord-safety-interval-forward 0)
  (key-chord-safety-interval-backward 0)
  :config
  (key-chord-mode 1))

;;; MARGINALIA

(use-package marginalia
  :straight t
  :config
  (marginalia-mode 1))

;;; BLACKOUT

(use-package blackout
  :straight t)

;;; HYDRA

(use-package hydra
  :straight t
  :config
  (general-define-key
   :keymaps 'hydra-base-map
   "q"   'hydra-keyboard-quit
   "C-g" 'hydra-keyboard-quit))

(use-package pretty-hydra
  :straight t)

;;; UNDO FU

(use-package undo-fu
  :straight t
  :custom
  (undo-fu-allow-undo-in-region t))

;;; EVIL

(use-package evil
  :straight t
  :init
  (setq evil-want-minibuffer t)
  (setq evil-want-Y-yank-to-eol t)
  :custom
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-fu)
  (evil-mode-line-format nil)
  :config
  (evil-mode 1)
  (advice-add 'evil-use-register :after '(lambda (&rest _args) (message "Using register: %s" (char-to-string evil-this-register))))
  ;; I will rebind these to something more useful
  (general-define-key
   :states '(normal motion visual insert)
   "C-f" nil
   "C-r" nil
   "M-j" nil
   "M-k" nil)
  (general-define-key
   "M-J" 'evil-scroll-line-up
   "M-m" 'evil-set-marker
   "M-K" 'evil-scroll-line-down)
  (general-define-key
   :states '(normal visual motion)
   "SPC s" 'save-buffer
   "RET"   nil
   "M-."   nil
   "M-,"   nil
   "C-."   nil
   "C-,"   nil
   "r"     'evil-use-register
   "x"     'delete-forward-char
   "X"     'delete-backward-char
   "U"     'evil-redo
   "J"     nil
   "K"     'evil-join
   "q"     'previous-buffer
   "Q"     'my/safe-kill-buffer
   "SPC m" 'evil-execute-last-recorded-macro
   "m"     'evil-record-macro
   "M"     'evil-execute-macro)
  (general-define-key
   :states '(insert visual replace)
   (general-chord "jk") 'evil-normal-state
   (general-chord "kj") 'evil-normal-state)
  (general-define-key
   :keymaps 'evil-ex-completion-map
   "M-p" 'next-complete-history-element
   "M-n" 'previous-complete-history-element))

;;; EVIL COLLECTION

(use-package evil-collection
  :straight t
  :custom
  (evil-collection-want-unimpaired-p nil))

;;; PASS

(setq epa-pinentry-mode 'loopback)
(use-package pass
  :straight t)

(use-package auth-source-pass
  :straight (auth-source-pass :type built-in)
  :config
  (auth-source-pass-enable)
  (let ((root-password (password-store-get "root-password")))
	(defun my/string-contains-root-password (str)
	  (when root-password (string-match-p (regexp-quote root-password) str))))
  :custom
  (auth-source-debug t)
  (auth-source-do-cache nil)
  (auth-sources '(password-store)))

(use-package password-cache
  :straight (password-cache :type built-in)
  :custom
  (password-cache-expiry nil))

;;; ACE WINDOW

(use-package ace-window
  :straight t
  :custom
  (aw-dispatch-always nil)
  (aw-dispatch-when-more-than 2)
  (aw-minibuffer-flag t)
  :config
  (defmacro my/with-aw (fn &rest args)
	"Expands to an interactive lambda that applies FN to ARGS from
an ace-window selected window then switch back to the original window."
	`(lambda ()
	   (interactive)
	   (let* ((orig-win (selected-window))
			  (aw-dispatch-when-more-than 1)
			  (aw (aw-select "Ace Window")))
		 (when (and (window-live-p aw) (not (window-dedicated-p aw)))
		   (select-window aw)
		   (apply ,fn ,args)
		   (select-window orig-win)))))
  (general-define-key "M-w" 'ace-window)
  (window-key-def
	"w" 'ace-window
	"V" 'split-window-horizontally
	"Z" 'split-window-vertically
	"D" 'delete-window
	"X" 'kill-buffer-and-window
	"s" 'ace-swap-window
	"v" `(,(my/with-aw 'split-window-horizontally) :wk "ACE split-vert")
	"z" `(,(my/with-aw 'split-window-vertically) :wk "ACE split-horiz")
	"d" `(,(my/with-aw 'delete-window) :wk "ACE delete-win")
	"x" `(,(my/with-aw 'kill-buffer-and-window) :wk "ACE delete-win-and-buf"))
  (buffer-key-def
	"D" 'kill-this-buffer
	"d" `(,(my/with-aw 'kill-this-buffer) :wk "ACE kill-this-buffer")))

;;; EMBARK

(use-package embark
  :straight t
  :config
  (eval-when-compile
	(defmacro my/embark-ace-action (fn)
	  `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
		 (interactive)
		 (with-demoted-errors "%s"
		   (require 'ace-window)
		   (let ((aw-dispatch-always t)
				 (orig-dir default-directory))
			 (aw-switch-to-window (aw-select nil))
			 (let ((default-directory orig-dir))
			   (call-interactively (symbol-function ',fn))))))))

  (general-define-key
   :keymaps 'embark-file-map
   "o" (my/embark-ace-action find-file))

  (general-define-key
   :keymaps 'embark-buffer-map
   "o" (my/embark-ace-action switch-to-buffer))

  (general-define-key
   "M-e" 'embark-act
   "M-E" 'embark-become))

(use-package embark-consult
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; PRESCIENT

(use-package prescient
  :straight t
  :custom
  (prescient-save-file "~/.emacs.d/.prescient")
  (prescient-sort-full-matches-first t)
  :config
  (prescient-persist-mode 1))

;;; VERTICO

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode 1)
  (use-package vertico-directory :straight nil :after vertico :ensure nil)
  :custom
  (vertico-count 15)
  (vertico-scroll-margin 0)
  :config
  (general-define-key
   :keymaps 'vertico-map
   "M-j" 'vertico-next
   "M-k" 'vertico-previous)

  (general-define-key
   :keymaps 'vertico-map
   :states 'normal
   "~" #'(lambda () (interactive) (delete-minibuffer-contents) (insert-char ?~) (insert-char ?/) (evil-insert-state))
   "/" #'(lambda () (interactive) (delete-minibuffer-contents) (insert-char ?/) (evil-insert-state)))

  (defun backward-kill-sentence (&optional arg)
    "Kill back from point to start of sentence.
With ARG, repeat, or kill forward to Nth end of sentence if
negative ARG -N."
    (interactive "p")
    (kill-region (point) (progn (backward-sentence arg) (point))))

  (general-define-key
   :keymaps 'vertico-map
   :states '(normal insert)
   "M-DEL" 'vertico-directory-delete-word)

  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
			  (lambda (orig cand prefix suffix index _start)
				(setq cand (funcall orig cand prefix suffix index _start))
				(concat
				 (if (= vertico--index index)
					 (propertize "» " 'face 'vertico-current)
				   "  ")
				 cand)))
  :custom-face
  (vertico-current ((t (:foreground "green" :background "default")))))

(use-package vertico-prescient
  :straight t
  :config
  (vertico-prescient-mode 1))

;;; IVY

;; (use-package ivy
;;   :straight t
;;   :blackout
;;   :custom
;;   (ivy-fixed-height-minibuffer t)
;;   :config
;;   (ivy-mode 1)
;;   (general-define-key
;;    :keymaps 'ivy-minibuffer-map
;;    "M-j" 'ivy-next-line
;;    "M-k" 'ivy-previous-line)
;;   :custom-face
;;   (ivy-current-match ((t (:foreground "green" :background "default"))))
;;   (ivy-minibuffer-match-face-1 ((t (:foreground "yellow" :background "default"))))
;;   (ivy-minibuffer-match-face-2 ((t :inherit ivy-minibuffer-match-face-1)) face-defface-spec)
;;   (ivy-minibuffer-match-face-3 ((t :inherit ivy-minibuffer-match-face-1)) face-defface-spec)
;;   (ivy-minibuffer-match-face-4 ((t :inherit ivy-minibuffer-match-face-1)) face-defface-spec))

;; (use-package ivy-prescient
;;   :straight t
;;   :config
;;   (ivy-prescient-mode 1))

;;; COUNSEL

;; (use-package counsel
;;   :straight t)

;;; CONSULT

(use-package consult
  :straight t
  :custom
  (consult-buffer-sources '(consult--source-buffer consult--source-recent-file))
  :config
  (general-define-key
   "M-o" 'consult-buffer)
  (misc-key-def
	"p" 'consult-yank-from-kill-ring
	"L" 'consult-locate
	"f" 'consult-find))

;;; YASNIPPET

;; (use-package yasnippet
;;   :straight t
;;   :blackout yas-minor-mode
;;   :config
;;   (use-package yasnippet-snippets
;;     :straight t)
;;   (general-define-key
;;    :keymaps 'yas-minor-mode-map
;;    "M-TAB" 'yas-insert-snippet))

;;; MULTI COMPILE

;; (use-package multi-compile
;;   :straight t
;;   :custom
;;   (multi-compile-default-directory-function #'projectile-project-root)
;;   :config
;;   (evil-set-initial-state 'compilation-mode 'normal))

;;; PROJECTILE

(use-package projectile
  :straight t
  :blackout
  :config
  (use-package consult-projectile
	:straight t
	:custom
	(consult-projectile-use-projectile-switch-project t))
  :custom
  (projectile-project-search-path '("~/p"))
  (projectile-track-known-projects-automatically nil)
  (projectile-enable-caching nil)
  (projectile-indexing-method 'alien)
  (projectile-file-exists-remote-cache-expire nil)
  (projectile-sort-order 'recently-active)
  (projectile-current-project-on-switch 'keep)
  (projectile-find-dir-includes-top-level t)
  (projectile-switch-project-action #'consult-projectile)
  :config
  (projectile-mode 1)

  (defun my/projectile-compile-project (&optional force-prompt)
    (interactive)
    (let* ((prompt-for-project (lambda () (completing-read "Project: " projectile-known-projects nil t)))
           (root0 (projectile-project-root))
           (default-directory (if force-prompt (funcall prompt-for-project) (or root0 (funcall prompt-for-project)))))
      (call-interactively 'multi-compile-run)))

  ;; This lets Embark work with projectile
  (define-advice projectile-find-file (:around (orig-fn &optional arg) fix-dir)
	(let ((default-directory (projectile-project-root)))
	  (funcall orig-fn arg)))
  (define-advice projectile-switch-to-buffer (:around (orig-fn &optional arg) fix-dir)
	(let ((default-directory (projectile-project-root)))
	  (funcall orig-fn arg)))

  (add-to-list 'marginalia-command-categories '(projectile-switch-project . file))

  (general-define-key
   :states 'normal
   "SPC p" 'my/hydra-projectile/body)
  (pretty-hydra-define my/hydra-projectile
	(:color blue :hint nil :quit-key "SPC p" :title "Projectile")
	("Find"
	 (("p" projectile-switch-project "Project")
	  ("o" consult-projectile "Buffer/File")
	  ("f" consult-projectile-find-file "File")
	  ("b" consult-projectile-switch-to-buffer "Buffer")
	  ("d" consult-projectile-find-dir "Directory"))
	 "Act"
	 (("k" projectile-kill-buffers "Kill Buffers")
	  ("G" projectile-grep "Grep")
	  ("g" consult-grep "Grep Quick")
      ("D" (lambda () (interactive) (let ((default-directory "")) (call-interactively 'projectile-run-gdb))) "gdb")
	  ("s" (lambda (&optional arg) (interactive "P") (let ((default-directory "")) (projectile-run-shell arg))) "Shell")
      ("c" (lambda () (interactive) (my/projectile-compile-project t)) "Compile")))))

;;; BETTER JUMPER

(use-package better-jumper
  :straight t
  :custom
  (better-jumper-use-evil-jump-advice nil)
  (better-jumper-context 'buffer)
  (better-jumper-max-length 40)
  (better-jumper-add-jump-behavior 'replace)
  :config
  (general-define-key
   "M->" 'better-jumper-jump-forward
   "M-<" 'better-jumper-jump-backward)
  (defun my/better-jumper--around (fn &rest args)
	(let* ((orig-pt (point))
		   (orig-ln (line-number-at-pos orig-pt))
		   (orig-win (selected-window)))
	  (apply fn args)
	  (let* ((new-pt (point))
			 (new-ln (line-number-at-pos new-pt))
			 (new-win (selected-window)))
		(when (and (> (abs (- new-ln orig-ln)) 1)
				   (eq new-win orig-win))
		  (better-jumper-set-jump orig-pt)
		  (better-jumper-set-jump new-pt)))))
  (advice-add 'evil-ex :around 'my/better-jumper--around)
  (advice-add 'evil-next-line :around 'my/better-jumper--around)
  (advice-add 'evil-goto-mark :around 'my/better-jumper--around)
  (advice-add 'evil-previous-line :around 'my/better-jumper--around)
  (advice-add 'evil-goto :around 'my/better-jumper--around)
  (advice-add 'evil-goto-line :around 'my/better-jumper--around)
  (advice-add 'evil-goto-first-line :around 'my/better-jumper--around))

;;; EGLOT

;; (use-package eglot
;;   :straight t
;;   :custom
;;   (eglot-ignored-server-capabilites '(:documentHighlightProvider))
;;   :config
;;   (general-define-key
;;    :keymaps 'eglot-mode-map
;;    "C-h SPC" 'eldoc-doc-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      SHMORGISHBOARD                       ;;
;; This section is for writing miscellaneous elisp that may  ;;
;; depend on the default packages. Most of this section is   ;;
;; for defining my own useful functions, and configuring     ;;
;; various built-in settings.                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/recenter-if-offscreen--around (orig-fn &rest args)
  "Scroll line to center if cursor moved off visible range of buffer.
This function should be used as around advice."
  (let ((orig-ln (line-number-at-pos (point)))
		(upper (line-number-at-pos (window-start)))
		(lower (line-number-at-pos (window-end)))
		(orig-win (selected-window)))
	(apply orig-fn args)
	(let ((new-ln (line-number-at-pos)))
	  (when (and (> (abs (- orig-ln new-ln)) 1)
				 (or (< lower new-ln) (> upper new-ln))
				 (eq orig-win (selected-window)))
		(recenter)))))

(advice-add 'better-jumper--jump :around 'my/recenter-if-offscreen--around)

(advice-add 'evil-undo :around 'my/recenter-if-offscreen--around)
(advice-add 'evil-redo :around 'my/recenter-if-offscreen--around)
(advice-add 'evil-ex :around 'my/recenter-if-offscreen--around)
(advice-add 'evil-next-line :around 'my/recenter-if-offscreen--around)
(advice-add 'evil-previous-line :around 'my/recenter-if-offscreen--around)
(advice-add 'evil-goto-mark :around 'my/recenter-if-offscreen--around)
(advice-add 'evil-goto :around 'my/recenter-if-offscreen--around)
(advice-add 'evil-goto-first-line :around 'my/recenter-if-offscreen--around)

;(which-func-mode 1)

(savehist-mode 1)

(show-paren-mode 0)

(setq confirm-kill-emacs #'y-or-n-p)

(setq shr-use-colors nil)

(setq-default require-final-newline t)

(defun my/safe-kill-buffer ()
  "Like `kill-this-buffer' but prompt for y or n first."
  (interactive)
  (if (y-or-n-p "Do you really want to kill this buffer?")
	  (kill-this-buffer)))

(defvar my/saved-window-configuration nil)
(defun my/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
	  (set-window-configuration my/saved-window-configuration)
	(progn
	  (setq my/saved-window-configuration (current-window-configuration))
	  (delete-other-windows))))
(general-define-key
 "M-z" 'my/toggle-maximize-buffer)

(general-define-key
 :states 'normal
 (general-chord "dl") 'display-line-numbers-mode
 (general-chord "ld") 'display-line-numbers-mode)

(setq enable-recursive-minibuffers t)

;; (general-define-key :states 'normal "." 'my/back-and-forth-windows)
(defun my/back-and-forth-windows ()
  "Toggle between current window and previous window."
  (interactive)
  (let ((win (get-mru-window t t t)))
	(unless win (error "Last window not found"))
	(let ((frame (window-frame win)))
	  (select-frame-set-input-focus frame)
	  (select-window win))))

(general-define-key
 "M-j" 'my/scroll-down-half-page
 "M-k" 'my/scroll-up-half-page)

(setq display-buffer-base-action '(display-buffer-same-window))

(defun my/scroll-down-half-page ()
  "scroll down half a page while keeping the cursor centered"
  (interactive)
  (let ((ln (line-number-at-pos (point)))
		(lmax (line-number-at-pos (point-max)))
		(vis-lmid (save-excursion (move-to-window-line nil) (line-number-at-pos (point))))
		(vis-lmin (save-excursion (move-to-window-line -1) (line-number-at-pos (point)))))
	(cond ((>= ln (- lmax 1)) (recenter (window-end)))
		  ((<= ln (- vis-lmid 10)) (move-to-window-line nil))
		  ((= ln vis-lmin) (recenter))
		  (t (move-to-window-line -1)))))

(defun my/scroll-up-half-page ()
  "scroll up half a page while keeping the cursor centered"
  (interactive)
  (let ((ln (line-number-at-pos (point)))
		(lmax (line-number-at-pos (point-max)))
		(vis-lmid (save-excursion (move-to-window-line nil) (line-number-at-pos (point))))
		(vis-lmax (save-excursion (move-to-window-line 0) (line-number-at-pos (point)))))
	(cond ((= ln vis-lmax) (recenter))
		  ((>= ln (+ vis-lmid 10)) (move-to-window-line nil))
		  (t (move-to-window-line 0)))))

(defun my/dired-dwim (file-or-buffer &optional switches)
  "Open dired directory for FILE-OR-BUFFER"
  (interactive (list (or (buffer-file-name) default-directory (error "Could not find a default directory to visit"))))
  (let ((dir (cond ((buffer-live-p file-or-buffer)
					(let ((buffers-file (buffer-file-name file-or-buffer)))
					  (if buffers-file (file-name-directory buffers-file) (error "buffer %s is not visiting a file" (buffer-name file-or-buffer)))))
				   ((file-directory-p file-or-buffer) file-or-buffer)
				   ((file-name-absolute-p file-or-buffer) (file-name-directory file-or-buffer))
				   (t (error "%s is not a valid file or buffer" file-or-buffer)))))
	(dired dir switches)))
(general-define-key
 "M-D" 'my/dired-dwim)

(general-define-key
 :keymaps 'comint-mode-map
 "C-l" 'my/comint-clear
 "C-r" 'my/comint-history-search)

(setq history-delete-duplicates t)

(set-language-environment "UTF-8")

(defun my/comint-clear ()
  "Clear the comint input buffer"
  (interactive)
  (let ((buffer-undo-list nil) ; no undo
        (orig-ln (line-number-at-pos))
		(col (current-column))
		(cmd (progn (end-of-buffer)
					(move-end-of-line nil)
					(set-mark (point))
					(move-beginning-of-line nil)
					(buffer-substring (region-beginning) (region-end))))
		(after-ln (line-number-at-pos)))
	(delete-region (region-beginning) (region-end))
	(comint-clear-buffer)
	(insert cmd)
	(if (= orig-ln after-ln)
		(move-to-column col t)
	  (move-beginning-of-line nil))))

(defun my/comint-history-search ()
  "Search through the comint history with completing-read."
  (interactive)
  (let* ((selectrum-should-sort nil)
		 (vertico-prescient-mode nil)
		 (val (completing-read "Comint History: "
							   (delete-dups (ring-elements comint-input-ring)))))
	(end-of-buffer)
	(delete-region (line-beginning-position) (line-end-position))
	(insert val)))

(general-define-key
 :states '(normal motion visual)
 "TAB" 'my/tab)

(defun my/tab ()
  "If point is on first non-blank of the current line then
`indent-for-tab-command', otherwise goto the lines first non-blank."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
	  (indent-for-tab-command)
	(back-to-indentation)))

(setq user-full-name "Nicholas Hubbard"
	  user-mail-address "nicholashubbard@posteo.net")

(setq display-time-format "%H:%M")
(setq display-time-default-load-average nil)
(display-time-mode 1)
(display-battery-mode 1)

(menu-bar-mode 0)
(tool-bar-mode 0)
(set-fringe-mode 0)
(tooltip-mode 0)
(scroll-bar-mode 0)

(fset 'yes-or-no-p 'y-or-n-p)

(setq scroll-step 1
	  scroll-conservatively 101)

(setq-default indent-tabs-mode nil) ; spaces instead of tabs
(setq-default tab-width 4) ; for when we do want tabs

(blink-cursor-mode 0)

(setq winner-dont-bind-my-keys t)
(winner-mode 1)

(delete-selection-mode)

(setq auto-save-default nil)
(setq delete-by-moving-to-trash nil
	  make-backup-files nil)

(setq disabled-command-function nil)

(setq kill-buffer-query-functions nil)

(setq warning-minimum-level :error)

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

(setq auto-revert-check-vc-info nil)
(setq-default mode-line-format
			  (delete '(vc-mode vc-mode) mode-line-format))

(global-set-key (kbd "M-r") 'revert-buffer)

(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

(setq-default display-fill-column-indicator-column 80
			  fill-column 80)
(add-hook 'text-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(setq redisplay-dont-pause t)

(setq recentf-save-file "~/.emacs.d/.recentf")
(setq recentf-max-saved-items 1000)
(recentf-mode 1)
(add-hook 'after-init-hook 'recentf-cleanup)

(require 'tramp)
(add-hook 'kill-emacs-hook 'tramp-cleanup-all-buffers)
(add-hook 'kill-emacs-hook 'tramp-cleanup-all-connections)

(buffer-key-def
  "r" 'rename-buffer)

(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-BSD     (eq system-type 'berkeley-unix))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    NON-DEFAULT PACKAGES                   ;;
;; The rest of the configuration is non-default package      ;;
;; definitions. All of these packages can assume that the    ;;
;; default packages and shmorgishboard functions are loaded. ;;
;; Non-default packages should never assume another          ;;
;; non-default package is already loaded, and instead should ;;
;; use some kind of autoloading to be sure the package is    ;;
;; loaded.                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; XREF

(use-package xref
  :straight (xref :type built-in)
  :config
  (advice-add 'xref-find-definitions :around 'my/recenter-if-offscreen--around)
  (advice-add 'xref-pop-marker-stack :around 'my/recenter-if-offscreen--around)
  (general-define-key
   :states 'normal
   :keymaps 'xref--xref-buffer-mode-map
   "RET" '(lambda () (interactive)
			(let ((xref-buf (current-buffer))
				  (xref (xref--item-at-point)))
			  (if xref
				  (xref-goto-xref t)
				(find-file-at-point))
			  (bury-buffer xref-buf)))))

;;; DUMB JUMP

;; (use-package dumb-jump
;;   :straight t
;;   :demand t
;;   :custom
;;   (dumb-jump-force-searcher 'grep)
;;   (dumb-jump-grep-cmd "egrep")
;;   (xref-show-definitions-function #'xref-show-definitions-completing-read)
;;   :config
;;   (general-define-key
;;    :keymaps 'prog-mode-map
;;    "M-." '(lambda () (interactive) (let ((xref-backend-functions #'dumb-jump-xref-activate)) (call-interactively #'xref-find-definitions)))
;;    "M-," 'dumb-jump-back))

;;; WINDRESIZE

(use-package windresize
  :straight t
  :commands windresize
  :config
  (general-define-key
   :keymaps 'windresize-map
   "RET" 'windresize-exit)
  (window-key-def
	"r" 'windresize))

;;; AGGRESSIVE INDENT

(use-package aggressive-indent
  :straight t
  :blackout
  :commands aggressive-indent-mode)

;;; AVY

(use-package avy
  :straight t
  :custom
  (avy-timeout-seconds 0.5)
  (avy-single-candidate-jump t)
  (avy-style 'at-full)
  (avy-case-fold-search nil)
  :config
  (advice-add 'avy-jump :around 'my/better-jumper--around)
  (general-define-key
   :states '(normal visual)
   "J" 'avy-goto-char-2))

;;; CLEAN KILL RING

(use-package clean-kill-ring
  :straight t
  :custom
  (clean-kill-ring-prevent-duplicates t)
  (kill-ring-max 40)
  :config
  (clean-kill-ring-mode 1))

;;; CORFU

(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 3)
  (corfu-quit-at-boundary t)
  (corfu-cycle nil)
  (corfu-preselect 'first)
  (corfu-preview-current nil)
  :config
  (global-corfu-mode 1)
  (defun my/corfu-enable-always-in-minibuffer ()
	"Enable Corfu in the minibuffer if Vertico is not active."
	(unless (or (bound-and-true-p vertico--input)
				(bound-and-true-p ctrlf--active-p)
				(eq (current-local-map) read-passwd-map))
	  (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
				  corfu-popupinfo-delay nil)
	  (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook 'my/corfu-enable-always-in-minibuffer 1)
  ;; (general-define-key
  ;;  :states 'insert
  ;;  "TAB" 'completion-at-point)
  (general-define-key
   :keymaps 'corfu-map
   "RET" nil
   "TAB" 'corfu-insert
   "M-j" 'corfu-next
   "M-k" 'corfu-previous))

;;; CAPE

(use-package cape
  :straight t
  :config
  (add-to-list 'completion-at-point-functions 'cape-dabbrev)
  (add-to-list 'completion-at-point-functions 'cape-keyword)
  (add-to-list 'completion-at-point-functions 'cape-file))

;;; COMPANY

;; (use-package company
;;   :straight t
;;   :blackout
;;   :custom-face
;;   (company-tooltip-selection ((t (:foreground "green"))))
;;   (company-tooltip-common-selection ((t (:foreground "green"))))
;;   :custom
;;   (company-icon-margin 0)
;;   (company-format-margin-function nil)
;;   (company-dabbrev-code-modes t)
;;   (company-dabbrev-downcase nil)
;;   (company-dabbrev-ignore-case nil)
;;   (company-dabbrev-other-buffers 'all)
;;   (company-idle-delay 0)
;;   (company-require-match nil)
;;   (company-minimum-prefix-length 2)
;;   (company-show-numbers t)
;;   (company-backends '((company-files company-capf company-keywords company-dabbrev-code company-etags company-dabbrev)))
;;   (company-transformers '(delete-dups))
;;   :config
;;   (global-company-mode 1)
;;   (general-define-key
;;    :keymaps 'company-active-map
;;    "M-j" 'company-select-next
;;    "M-k" 'company-select-previous
;;    "C-h" nil
;;    "RET" nil
;;    [return] nil
;;    "TAB" 'company-complete-selection
;;    [tab] 'company-complete-selection))

;;; HASKELL

(use-package haskell-mode
  :magic (".*\\.hs$" . haskell-mode)
  :straight t)

;;; CPERL

(use-package cperl-mode
  :load-path "/home/_73/.emacs.d/my/cperl"
  :commands (perl-mode cperl-mode)
  :hook
  (cperl-mode . flycheck-mode)
  (cperl-mode . my/cperl-select-correct-flycheck-checker)
  :init
  (fset 'perl-mode 'cperl-mode)
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save))
  (flycheck-display-errors-delay 0.3)

  (cperl-invalid-face nil)
  (cperl-indent-level 4)
  (cperl-close-paren-offset (- cperl-indent-level))
  (cperl-indent-parens-as-block t)
  (cperl-electric-keywords nil)
  (cperl-electric-parens nil)
  (cperl-electric-lbrace-space nil)
  (cperl-extra-newline-before-brace-multiline nil)
  (cperl-auto-newline nil)
  :custom-face
  (cperl-array-face ((t :inherit font-lock-variable-name-face)) face-defface-spec)
  (cperl-hash-face  ((t :inherit font-lock-variable-name-face)) face-defface-spec)
  :config
  (add-to-list 'multi-compile-alist '(cperl-mode . (("perl-prove" . "prove")
                                                    ("plx-prove" . "plx prove"))))

  (flycheck-define-checker my/perl-plx
    ""
    :command ("plx" "-w" "-c"
              (option-list "-I" flycheck-perl-include-path)
              (option-list "-M" flycheck-perl-module-list concat))
    :standard-input t
    :error-patterns
    ((error line-start (minimal-match (message))
            " at - line " line
            (or "." (and ", " (zero-or-more not-newline))) line-end))
    :modes (perl-mode cperl-mode))

  (defun my/cperl-select-correct-flycheck-checker ()
    "If the current buffer is part of a plx project then use the `my/perl-plx'
checker, otherwise use the `perl' checker."
    (let ((proj-root (projectile-project-root (string-remove-suffix "/" default-directory))))
      (if (and proj-root (file-directory-p (concat proj-root ".plx")))
          (flycheck-select-checker 'my/perl-plx)
        (flycheck-select-checker 'perl))))


  (general-define-key ;; the default behavior of { is annoying
   :keymaps 'cperl-mode-map
   "{" nil))

;;; CRUX

(use-package crux
  :straight t
  :config
  (file-key-def
	"d" '(crux-delete-buffer-and-file :wk "delete-file")
	"r" '(crux-rename-buffer-and-file :wk "rename-file")))

;;; CTRLF

(use-package ctrlf
  :straight t
  :general
  ("C-f" 'ctrlf-forward-regexp)
  :custom-face
  (ctrlf-highlight-active  ((t (:background "gray37"  :foreground "green"))))
  (ctrlf-highlight-line    ((t (:background "gray37"  :foreground "green"))))
  (ctrlf-highlight-passive ((t (:background "default" :foreground "yellow"))))
  :custom
  (ctrlf-auto-recenter t)
  (ctrlf-go-to-end-of-match nil)
  (ctrlf-default-search-style 'regexp)
  :config
  (advice-add 'ctrlf--start :around 'my/better-jumper--around)

  (general-define-key
   :keymaps 'ctrlf-minibuffer-mode-map
   "M-j" 'ctrlf-next-match
   "M-k" 'ctrlf-previous-match)

  (general-define-key
   :keymaps 'ctrlf-minibuffer-mode-map
   :states 'normal
   "gg" 'ctrlf-first-match
   "G"  'ctrlf-last-match))

;;; DIRED

(use-package dired+
  :straight t
  :demand t
  :custom-face
  (diredp-dir-name       ((t (:foreground "purple" :background "default"))))
  (diredp-dir-priv       ((t :inherit diredp-dir-name)) face-defface-spec)
  (diredp-file-name      ((t (:foreground "white" :background "default"))))
  (diredp-file-suffix    ((t :inherit diredp-file-name)) face-defface-spec)
  (diredp-dir-heading    ((t (:foreground "gold2" :background "default"))))
  (diredp-flag-mark-line ((t (:foreground "green" :background "default"))))
  (diredp-flag-mark      ((t :inherit diredp-flag-mark-line)) face-defface-spec)
  (diredp-no-priv        ((t :inherit diredp-file-name)) face-defface-spec)
  (diredp-dir-priv       ((t :inherit diredp-no-priv)) face-defface-spec)
  (diredp-rare-priv      ((t :inherit diredp-no-priv)) face-defface-spec)
  (diredp-exec-priv      ((t :inherit diredp-no-priv)) face-defface-spec)
  (diredp-link-priv      ((t :inherit diredp-no-priv)) face-defface-spec)
  (diredp-read-priv      ((t :inherit diredp-no-priv)) face-defface-spec)
  (diredp-write-priv     ((t :inherit diredp-no-priv)) face-defface-spec)
  (diredp-other-priv     ((t :inherit diredp-no-priv)) face-defface-spec)

  :hook
  (dired-mode . auto-revert-mode)
  (dired-mode . auto-revert-mode)
  (dired-mode . (lambda () (rename-buffer (concat "dired: " dired-directory))))

  :custom
  (diredp-hide-details-initially-flag nil)
  (diredp-hide-details-propagate-flag nil)
  (dired-listing-switches "-DAlh --group-directories-first")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)

  :config

  (setq dired-mode-map (make-sparse-keymap))

  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "M-D" ' my/hydra-dired/body
   "m"     '(lambda (&optional arg interactive)
			  (interactive (list current-prefix-arg t))
			  (save-excursion (if (diredp-this-file-unmarked-p)
								  (dired-mark arg interactive)
								(dired-unmark arg interactive))))
   "M"     '(lambda () (interactive) (dired-unmark-all-marks) (dired-toggle-marks))
   "U"     'dired-unmark-all-marks
   "h"     'diredp-up-directory-reuse-dir-buffer
   "C-f"   'dired-goto-file
   "l"     '(lambda () (interactive)
			  (unless dired-hide-details-mode
				;; filename is 9th field with details
				(back-to-indentation)
				(forward-whitespace 8))
			  (let ((file (dired-get-file-for-visit)))
				(if (file-directory-p file)
					(dired-find-alternate-file)
				  (find-file file)))))

  (pretty-hydra-define my/hydra-dired
	;; This hydra is only available in dired-mode.
	(:color blue :hint nil :quit-key "M-D" :title "Dired")
	("Command"
	 (("rm" dired-do-delete "Remove")
	  ("cp" dired-do-copy "Copy")
	  ("mv" dired-do-rename "Move")
	  ("cm" dired-do-chmod "Chmod")
	  ("co" dired-do-chown "Chown")
	  ("to" dired-do-touch "Touch")
	  ("mf" dired-create-empty-file "Make File")
	  ("md" dired-create-directory "Mkdir"))
	 "Act"
	 (("sh" dired-do-shell-command "Shell Command")
	  ("gr" diredp-do-grep "Grep")
	  ("GR" diredp-do-grep-recursive "Grep Recursively")
	  ("un" dired-undo "Undo")
	  ("ca" dired-do-compress-to "Compress to Archive"))
	 "Find"
	 (("fg" find-grep-dired "Find Grep Regexp")
	  ("fn" find-name-dired "Find Name") )
	 "Mark"
	 (("ma" (lambda () (interactive) (dired-unmark-all-marks) (dired-toggle-marks)) "All" :color red)
	  ("re" dired-mark-files-regexp "Regexp" :color red)
	  ("RE" dired-mark-files-containing-regexp "Containing Regexp" :color red)
	  ("su" dired-mark-suffix "Suffix" :color red)
	  ("di" dired-mark-directories "Directories" :color red)))))

;;; DPASTE

(use-package dpaste
  :straight t
  :general
  (:states 'visual (general-chord "dp") 'dpaste-region))

;;; EMACS LISP

(use-package elisp-mode
  :straight (elisp-mode :type built-in)
  :after smartparens
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . aggressive-indent-mode)
  (emacs-lisp-mode . turn-on-smartparens-strict-mode))

;;; RCIRC

;; (use-package rcirc
;;   :straight (rcirc :type built-in)
;;   :config
;;   (evil-set-initial-state 'rcirc-mode 'normal)
;;   :custom
;;   (rcirc-log-directory "~/.irc-logs")
;;   (rcirc-time-format "%Y-%m-%d %H:%M ")
;;   (rcirc-default-nick "test")
;;   (rcirc-kill-channel-buffers t)
;;   (rcirc-omit-mode t)
;;   (rcirc-server-alist
;;    `(("irc.libera.chat"
;;       :password ,(password-store-get "libera")
;;       :port 6697
;;       :encryption tls
;;       :channels ("#emacs" "#perl"))
;;      ("irc.perl.org"
;;       :port 7062
;;       :encryption tls))))

;; (use-package rcirc-notify
;;   :straight t
;;   :custom
;;   (rcirc-notify-popup-timeout 12000)
;;   :config
;;   (rcirc-notify-add-hooks))

;;; ERC

(use-package erc
  :straight (erc :type built-in)
  :config
  (require 'erc)
  (require 'erc-log)
  (require 'erc-desktop-notifications)

  (setq
   erc-hide-list '("JOIN" "PART" "QUIT")
   erc-kill-buffer-on-part t
   erc-kill-queries-on-quit t
   erc-kill-server-buffer-on-quit t
   erc-server-auto-reconnect t
   erc-server-reconnect-attempts 5
   erc-server-reconnect-timeout 5
   erc-max-buffer-size 30000
   erc-log-channels-directory "~/.irc-logs"
   erc-generate-log-file-name-function 'erc-generate-log-file-name-short)

  (setq erc-modules '(button
					  completion
					  fill
					  irccontrols
					  list
					  log
					  match
					  menu
					  move-to-prompt
					  netsplit
					  truncate networks
					  noncommands
					  notifications
					  readonly
					  ring
					  stamp
					  track))
  (erc-update-modules)
  (erc-truncate-mode 1)
  (erc-track-mode 0)

  (defun irc-perl ()
	(interactive)
	(if (get-buffer "irc.perl.org:7062")
		(switch-to-buffer  "irc.perl.org:7062")
	  (erc-tls :server "irc.perl.org"
			   :port 7062
			   :nick "_73")))

  (defun irc-libera ()
	(interactive)
	(if (get-buffer "irc.libera.chat:6697")
		(switch-to-buffer  "irc.libera.chat:6697")
	  (erc-tls :server "irc.libera.chat"
			   :port 6697
			   :nick "_73"
			   :password (password-store-get "libera"))))

  (defun my/erc-regain-73 ()
	(interactive)
	(erc-move-to-prompt)
	(erc-kill-input)
	(erc-send-input (concat "/msg NickServ REGAIN _73 " (password-store-get "libera"))))

  (defun my/erc-shutdown ()
	(interactive)
	(dolist (buf (buffer-list))
	  (if (eq major-mode 'erc-mode)
		  (kill-buffer buf))))

  (unless (file-directory-p erc-log-channels-directory)
	(make-directory erc-log-channels-directory t)))

;;; EVIL COMMENTARY

(use-package evil-commentary
  :straight t
  :blackout t
  :config
  (evil-commentary-mode 1))

;;; EXPAND REGION

(use-package expand-region
  :straight t
  :config
  (text-editing-key-def
	"v"   '(nil :wk "mark-prefix")
	"v f" 'er/mark-defun
	"v x" 'mark-sexp
	"v s" 'er/mark-symbol
	"v S" 'er/mark-sentence
	"v p" 'er/mark-paragraph)
  (general-define-key
   :states 'visual
   "M-SPC" 'er/expand-region
   "C-M-SPC" 'er/contract-region))

;;; FLYCHECK

(use-package flycheck
  :straight t
  :commands flycheck-mode
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save))
  (flycheck-display-errors-delay 0.1)
  :config
  (advice-add 'flycheck-next-error :around 'my/better-jumper--around)
  (advice-add 'flycheck-previous-error :around 'my/better-jumper--around))

;;; FLYSPELL

(use-package flyspell-correct
  :straight t
  :general
  ("M-s" '(lambda () (interactive)
			(if (region-active-p)
				(flyspell-region (region-beginning) (region-end))
			  (flyspell-correct-at-point))))
  :blackout flyspell-mode
  :custom
  (ispell-program-name "hunspell")
  (ispell-local-dictionary "en_US")
  (flyspell-correct-highlight nil)
  :config
  (ispell-set-spellchecker-params)

  (defun my/flyspell-unhighlight ()
	(interactive)
	(flyspell-delete-all-overlays)))

;;; MAGIT

(use-package diff-hl
  :straight t
  :demand t
  :config
  (global-diff-hl-mode 1)
  (diff-hl-margin-mode 1)
  (diff-hl-flydiff-mode 1)
  (advice-add 'diff-hl-next-hunk :around 'my/recenter-if-offscreen--around)
  (advice-add 'diff-hl-previous-hunk :around 'my/recenter-if-offscreen--around)
  (advice-add 'diff-hl-next-hunk :around 'my/better-jumper--around)
  (advice-add 'diff-hl-previous-hunk :around 'my/better-jumper--around)
  :custom
  (diff-hl-show-staged-changes nil)
  :hook
  (dired-mode . diff-hl-dired-mode))

;; (use-package git-gutter
;;   :straight t
;;   :demand t
;;   :blackout
;;   :custom
;;   (git-gutter:ask-p nil)
;;   (git-gutter:update-commands '(switch-to-buffer select-window))
;;   (git-gutter:update-hooks '(after-save-hook after-revert-hook find-file-hook after-change-major-mode-hook text-scale-mode-hook focus-in-hook better-jumper-post-jump-hook))
;;   :config
;;   (global-git-gutter-mode 1)
;;   :config
;;   (advice-add 'git-gutter:next-hunk :around 'my/recenter-if-offscreen--around)
;;   (advice-add 'git-gutter:previous-hunk :around 'my/recenter-if-offscreen--around))

(use-package magit
  :straight t
  :demand t
  :custom
  (magit-clone-default-directory "~/p")
  :config
  (evil-collection-init 'magit)

  (magit-auto-revert-mode 1)

  (general-define-key "M-g" 'my/hydra-git/body)
  (general-define-key :keymaps 'magit-mode-map "M-w" nil)

  (general-define-key
   :keymaps 'magit-mode-map
   :states '(normal visual motion)
   "M-r" nil
   "M-g" 'magit-dispatch
   "C-f" nil)

  ;; we don't want magit stealing the leader key
  (general-define-key
   :keymaps `,evil-collection-magit-maps
   :states '(normal visual motion)
   "SPC" nil)
  (general-define-key
   :keymaps `,evil-collection-magit-maps
   "SPC" nil
   "Q" nil)

  (pretty-hydra-define my/hydra-git
	(:color amaranth :title "Git" :quit-key "SPC g")
	("Move"
	 (("j" diff-hl-next-hunk "next")
	  ("k" diff-hl-previous-hunk "previous"))
	 "Hunk"
	 (("s" diff-hl-stage-current-hunk "stage")
	  ("r" diff-hl-revert-hunk "revert" :exit t)
	  ("d" diff-hl-show-hunk "diff" :exit t)
	  ("m" diff-hl-mark-hunk "mark" :exit t))
	 "Magit"
	 (("g" magit-status "magit" :exit t))))
  :hook
  (git-commit-mode . evil-insert-state)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package magit-todos
  :straight t
  :after magit
  :custom
  (magit-todos-keywords-list '("TODO"))
  (magit-todos-section-map (make-sparse-keymap))
  (magit-todos-list-mode-map (make-sparse-keymap))
  (magit-todos-item-section-map (make-sparse-keymap))
  :config
  (magit-todos-mode 1)
  (general-define-key
   :keymaps 'magit-todos-item-section-map
   "RET" 'magit-todos-jump-to-item))

;;; HELPFUL

(use-package helpful
  :straight t
  :config
  (advice-add 'describe-function :override #'helpful-function)
  (advice-add 'describe-variable :override #'helpful-variable)
  (advice-add 'describe-command  :override #'helpful-callable)
  (advice-add 'describe-key      :override #'helpful-key)
  (advice-add 'describe-symbol   :override #'helpful-symbol)

  (general-define-key
   :keymaps '(emacs-lisp-mode-map helpful-mode-map ielm-map)
   "C-h SPC" 'helpful-at-point)

  :hook
  (helpful-mode . (lambda () (let ((buf-name (buffer-name)))
							   (and (string-match "^[^:]+: \\([^*]+\\)\\*$" buf-name)
									(let* ((buf-name (concat "*h: " (match-string 1 buf-name) "*"))
										   (buf (get-buffer buf-name)))
									  (when buf (kill-buffer buf))
									  (rename-buffer buf-name)))))))

;;; LINK HINT

(use-package link-hint
  :straight t
  :config
  (misc-key-def
	"l" 'link-hint-open-link))

;;; MU4E

(use-package mu4e
  :demand t
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  ;; :init
  ;; (defun mu4e--main-action-str (&rest _) nil)
  :hook
  (mu4e-compose-mode . (lambda () (auto-fill-mode 0)))
  :custom
  (send-mail-function 'smtpmail-send-it)
  (smtpmail-smtp-user "nicholashubbard@posteo.net")
  (smtpmail-default-smtp-server "posteo.de")
  (smtpmail-smtp-server "posteo.de")
  (smtpmail-smtp-service 587)
  (message-kill-buffer-on-exit t)
  (mml-secure-openpgp-sign-with-sender t)
  (smtpmail-stream-type 'starttls)
  (mu4e-drafts-folder "/Drafts")
  (mu4e-sent-folder "/Sent")
  (mu4e-trash-folder "/Trash")
  (mu4e-get-mail-command "mbsync --config ~/.mbsyncrc --all nicholashubbard@posteo.net")
  (mu4e-html2text-command "w3m -T text/html")
  (mu4e-update-interval 90)
  (mu4e-hide-index-messages t)
  (mu4e-confirm-quit nil)
  (mu4e-headers-auto-update nil)
  (mu4e-view-inhibit-images t)
  (mu4e-change-filenames-when-moving t)
  (mu4e-compose-signature "Nicholas Hubbard")
  (mu4e-compose-signature-auto-include t)
  (mu4e-use-fancy-chars t)
  (mu4e-completing-read-function #'completing-read)
  :config
  (add-to-list 'display-buffer-alist '("^\\*mu4e-main\\*$" display-buffer-same-window))
  (evil-collection-init 'mu4e)

  ;; redefine to add auto-update
  ;; (defun mu4e (&optional background)
  ;;   (interactive "P")
  ;;   (mu4e--start (unless background #'mu4e--main-view))
  ;;   (mu4e-update-mail-and-index nil))

  (general-define-key
   :states 'normal
   :keymaps 'mu4e-view-mode-map
   "SPC" nil))

;;; ORG

;; (use-package org
;;   :straight (org :type built-in)
;;   :config
;;   (use-package evil-org
;;     :straight t
;;     :hook (org-mode . (lambda () (evil-org-mode 1)))
;;     :config
;;     (require 'evil-org-agenda)
;;     (evil-org-agenda-set-keys))
;;   :custom
;;   (org-directory "~/org")

;;   (org-log-done 'time)

;;   (org-agenda-files (list "inbox.org"))
;;   (org-agenda-hide-tags-regexp ".")
;;   (org-agenda-files (mapcar 'file-truename (file-expand-wildcards "~/org/*.org")))
;;   (org-agenda-prefix-format
;;    '((agenda . " %i %-12:c%?-12t% s")
;;      (todo   . " ")
;;      (tags   . " %i %-12:c")
;;      (search . " %i %-12:c")))
;;   (org-agenda-custom-commands
;;    '(("g" "Get Things Done (GTD)"
;;       ((agenda ""
;;                ((org-agenda-skip-function
;;                  '(org-agenda-skip-entry-if 'deadline))
;;                 (org-deadline-warning-days 0)))
;;        (todo "NEXT"
;;              ((org-agenda-skip-function
;;                '(org-agenda-skip-entry-if 'deadline))
;;               (org-agenda-prefix-format "  %i %-12:c [%e] ")
;;               (org-agenda-overriding-header "\nTasks\n")))
;;        (agenda nil
;;                ((org-agenda-entry-types '(:deadline))
;;                 (org-agenda-format-date "")
;;                 (org-deadline-warning-days 7)
;;                 (org-agenda-skip-function
;;                  '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
;;                 (org-agenda-overriding-header "\nDeadlines")))
;;        (tags-todo "inbox"
;;                   ((org-agenda-prefix-format "  %?-12t% s")
;;                    (org-agenda-overriding-header "\nInbox\n")))
;;        (tags "CLOSED>=\"<today>\""
;;              ((org-agenda-overriding-header "\nCompleted today\n")))))))

;;   (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)")))

;;   (org-refile-targets '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))
;;   (org-refile-use-outline-path 'file)
;;   (org-outline-path-complete-in-steps nil)

;;   (org-capture-templates
;;    `(("i" "Inbox" entry  (file "inbox.org")
;;       ,(concat "* TODO %?\n"
;;                "/Entered on/ %U"))
;;      ("@" "Inbox [mu4e]" entry (file "inbox.org")
;;       ,(concat "* TODO Process \"%a\" %?\n"
;;                "/Entered on/ %U"))
;;      ("m" "Meeting" entry  (file+headline "agenda.org" "Future")
;;       ,(concat "* %? :meeting:\n"
;;                "<%<%Y-%m-%d %a %H:00>>"))
;;      ("n" "Note" entry  (file "notes.org")
;;       ,(concat "* Note (%a)\n"
;;                "/Entered on/ %U\n" "\n" "%?"))))
;;   :config
;;   (defun my/log-todo-next-creation-date (&rest ignore)
;;     "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
;;     (when (and (string= (org-get-todo-state) "NEXT")
;;                (not (org-entry-get nil "ACTIVATED")))
;;       (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
;;   (add-hook 'org-after-todo-state-change-hook #'my/log-todo-next-creation-date)

;;   (defun my/gtd-save-org-buffers ()
;;     "Save `org-agenda-files' buffers without user confirmation.
;; See also `org-save-all-org-buffers'"
;;     (interactive)
;;     (message "Saving org-agenda-files buffers...")
;;     (save-some-buffers t (lambda ()
;;                            (when (member (buffer-file-name) org-agenda-files)
;;                              t)))
;;     (message "Saving org-agenda-files buffers... done"))

;;   ;; Add it after refile
;;   (advice-add 'org-refile :after
;;               (lambda (&rest _)
;;                 (my/gtd-save-org-buffers)))

;;   (defun my/org-capture-inbox ()
;;     (interactive)
;;     (call-interactively 'org-store-link)
;;     (org-capture nil "i"))

;;   (defun my/org-capture-mail ()
;;     (interactive)
;;     (call-interactively 'org-store-link)
;;     (org-capture nil "@"))

;;   (org-key-def
;;     "r" 'org-refile)

;;   (general-define-key
;;    :states '(normal visual)
;;    "SPC o C" 'org-capture
;;    "SPC o c" 'my/org-capture-inbox
;;    "SPC o a" 'org-agenda)

;;   (general-define-key
;;    :states '(normal visual)
;;    :keymaps '(mu4e-headers-mode-map mu4e-view-mode-map)
;;    "SPC o c" 'my/org-capture-mail))

;; (use-package org-alert
;;   :straight t
;;   :custom
;;   (alert-default-style 'libnotify)
;;   (org-alert-interval 300)
;;   (org-alert-notify-cutoff 10)
;;   (org-alert-notify-after-event-cutoff 10)
;;   :config
;;   (org-alert-enable))

;;; PDF TOOLS

(use-package pdf-tools
  :straight t
  :magic ("%PDF" . pdf-view-mode)
  :hook
  (pdf-tools-enabled . pdf-view-midnight-minor-mode)
  :config
  (pdf-tools-install :no-query)
  (evil-set-initial-state 'pdf-view-mode 'normal)
  (general-define-key
   :keymaps 'pdf-view-mode-map
   :states 'normal
   "j" 'pdf-view-next-page
   "k" 'pdf-view-previous-page
   "J" 'pdf-view-enlarge
   "K" 'pdf-view-shrink
   ":" 'pdf-view-goto-page
   "gg" 'pdf-view-first-page
   "G" 'pdf-view-last-page))

(use-package pdf-view-restore
  :straight t
  :after pdf-tools
  :hook
  (pdf-view-mode . pdf-view-restore-mode)
  :custom
  (pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore"))

;;; CONSULT DIR

(use-package consult-dir
  :straight t
  :general
  (general-define-key
   "M-d" 'consult-dir)
  (general-define-key
   :keymaps 'vertico-map
   "M-d" 'consult-dir)
  :custom
  (consult-dir-default-command 'consult-dir-dired))

;;; RAINBOW DELIMITERS

(use-package rainbow-delimiters
  :straight t
  :blackout
  :commands rainbow-delimiters-mode)

;;; SHELL

(use-package shell
  :straight (shell :type built-in)
  :config
  (defun my/shell-dirtrack ()
	"Track directory via the prompt."
	(shell-dirtrack-mode 0)
	(setq-local dirtrack-list '("^[[][^@]*@[^ ]* \\([^]]*\\)\\]\\$ " 1))
	(dirtrack-mode 1))
  :custom
  (setq comint-password-prompt-regexp (concat comint-password-prompt-regexp "|easy password|passphrase"))
  :hook
  (shell-mode . my/shell-dirtrack))

(use-package shx
  :straight t
  :config
  (general-define-key
   :keymaps 'shx-mode-map
   "SPC" nil
   "RET" 'shx-send-input
   [S-return] '(lambda () (interactive) (my/comint-clear) (shx-send-input)))
  :hook
  (shell-mode . shx-mode))

(use-package bash-completion
  :straight t
  :custom
  (bash-completion-use-separate-processes t)
  :config
  (bash-completion-setup))

(use-package shell-pop
  :straight t
  :custom
  (shell-pop-window-position "bottom")
  (shell-pop-full-span nil)
  (shell-pop-window-size 37)
  (shell-pop-restore-window-configuration nil)
  (shell-pop-cleanup-buffer-at-process-exit t)
  (shell-pop-autocd-to-working-dir nil)
  :config
  (general-define-key
   "M-SPC" 'shell-pop
   "M-S-SPC" '(lambda () (interactive)
				(let ((shell-pop-autocd-to-working-dir t))
				  (call-interactively 'shell-pop)))))

;;; SHELL SCRIPT

(use-package sh-script
  :straight (sh-script :type built-in)
  :commands sh-mode
  :hook
  (sh-mode . yas/minor-mode)
  :custom
  (sh-indentation 4)
  (sh-basic-offset 4))

;;; WGREP

(use-package grep
  :straight (grep :type built-in)
  :commands grep
  :custom
  (grep-program "egrep")
  (grep-template "egrep <X> <C> -nH --null -e <R> <F>")
  (grep-find-template "find -H <D> <X> -type f <F> -exec egrep <C> -nH --null -e <R> \\{\\} +"))

(use-package wgrep
  :straight t
  :after grep
  :config
  (evil-set-initial-state 'grep-mode 'normal))

;;; SMARTPARENS

(use-package smartparens
  :straight t
  :blackout
  :config
  (smartparens-global-mode t)
  (require 'smartparens-config)
  (text-editing-key-def
	:states '(normal visual)
	"w" '(nil :wk "wrap-prefix")
	"w r" 'sp-rewrap-sexp
	"w u" 'sp-unwrap-sexp
	"w (" 'sp-wrap-round
	"w [" 'sp-wrap-square
	"w {" 'sp-wrap-curly
	"w \"" '(lambda () (interactive) (sp-wrap-with-pair "\""))
	"w '" '(lambda () (interactive) (sp-wrap-with-pair "'"))
	"s" '(nil :wk "slurp-prefix")
	"s f" 'sp-forward-slurp-sexp
	"s b" 'sp-backward-slurp-sexp
	"b" '(nil :wk "barf-prefix")
	"b f" 'sp-forward-barf-sexp
	"b b" 'sp-backward-barf-sexp)
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil))

;;; SUDO EDIT

(use-package sudo-edit
  :straight t
  :commands sudo-edit
  :config
  (sudo-edit-indicator-mode 1))

;;; WHICH KEY

(use-package which-key
  :straight t
  :blackout
  :config
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.7)
  (which-key-idle-secondary-delay 0.1)
  (which-key-prefix-prefix "++")
  (which-key-sort-order 'which-key-prefix-then-key-order))

;;; WHITESPACE CLEANUP

;; (use-package whitespace-cleanup-mode
;;   :straight t
;;   :blackout
;;   :custom
;;   (whitespace-cleanup-mode-only-if-initially-clean t)
;;   :hook
;;   (prog-mode . whitespace-cleanup-mode)
;;   (text-mode . whitespace-cleanup-mode))

;;; WS BUTLER

(use-package ws-butler
  :straight t
  :custom
  (ws-butler-keep-whitespace-before-point nil)
  :hook
  (prog-mode . ws-butler-mode)
  (text-mode . ws-butler-mode))

;;; MAN

(use-package man
  :straight (man :type built-in)
  :custom
  (Man-switches "-a")
  (Man-notify-method 'pushy)
  ;; (Man-notify-method 'aggressive)
  :config
  (evil-set-initial-state 'Man-mode 'normal)
  (defun my/man ()
	(interactive)
	(let ((man (completing-read "Manual entry: " 'Man-completion-table nil nil "^")))
	  (man man)))
  (general-define-key "C-h m" 'my/man))

;;; INFO

(use-package info
  :straight (info :type built-in)
  :config
  (evil-set-initial-state 'Info-mode 'normal))

;;; PROCED

(use-package proced
  :straight (proced :type built-in)
  :commands proced
  :hook
  (proced-mode . evil-normal-state)
  :custom
  (proced-auto-update-interval 3)
  (proced-filter 'all)
  :config
  (general-define-key
   :states 'normal
   :keymaps 'proced-mode-map
   "m" '(lambda (&optional count)
		  (interactive (list current-prefix-arg))
		  (save-excursion
			(move-beginning-of-line nil)
			(if (looking-at (proced-marker-regexp))
				(proced-unmark count)
			  (proced-mark count))))
   "M" 'proced-mark-all
   "U" 'proced-unmark-all
   "u" '(lambda () (interactive) (proced-update t nil) (recenter))
   "K" 'proced-send-signal
   "?" 'proced-why
   "su" 'proced-sort-user
   "sm" 'proced-sort-pmem
   "sc" 'proced-sort-pcpu
   "sp" 'proced-sort-pid
   "st" 'proced-toggle-tree))

;;; CSCOPE

;; (use-package helm-cscope
;;   :straight t
;;   :config
;;   (advice-add 'helm-cscope--goto-line :around 'my/better-jumper--around)
;;   (general-define-key
;;    :keymaps 'c-mode-map
;;    "M-." 'my/hydra-helm-cscope/body
;;    "M-," 'helm-cscope-pop-mark)
;;   (pretty-hydra-define my/hydra-helm-cscope
;;     (:color blue :hint nil :quit-key "M-." :title "CScope")
;;     ("Find"
;;      (("." helm-cscope-find-global-definition "Global Definition")
;;       ("s" helm-cscope-find-this-symbol "Symbol")
;;       ("S" helm-cscope-find-this-text-string "Text String")
;;       ("a" helm-cscope-find-assignments-to-this-symbol "Assignments to Symbol")
;;       ("c" helm-cscope-find-calling-this-function "Calling Function")
;;       ("C" helm-cscope-find-called-function "Called Function")
;;       ("f" helm-cscope-find-this-file "File")
;;       ("F" helm-cscope-find-files-including-file "Including File")))))

;;; GOOGLE THIS

(use-package google-this
  :straight t
  :general
  (:states
   '(normal visual)
   "go" '(lambda () (interactive)
		   (if (not (region-active-p))
			   (google-this-string nil)
			 (let ((thing (buffer-substring-no-properties (region-beginning) (region-end))))
			   (if (string-match-p link-hint-url-regexp thing)
				   (browse-url thing)
				 (call-interactively 'google-this)))))))

;;; CC MODE

(use-package cc-mode
  :straight (cc-mode :type built-in)
  :custom
  (c-default-style "bsd")
  (c-basic-offset 4)
  ;; :config
  ;; (add-to-list 'eglot-server-programs
  ;;   		   '((c-mode c++-mode)
  ;;   			 . ("/usr/lib/llvm/15/bin/clangd"
  ;;   				"-j=2"
  ;;   				"--log=error"
  ;;   				"--malloc-trim"
  ;;   				"--background-index"
  ;;   				"--clang-tidy"
  ;;   				"--cross-file-rename"
  ;;   				"--completion-style=detailed"
  ;;   				"--pch-storage=memory"
  ;;   				"--header-insertion=never"
  ;;   				"--header-insertion-decorators=0")))
  )

;;; RUBY MODE

(use-package ruby-mode
  :straight (ruby-mode :type built-in)
  :magic (".*\\.rb$" . ruby-mode)
  :hook
  (ruby-mode . (lambda () (flycheck-mode 1) (flycheck-select-checker 'ruby))))

;;; ORDERLESS

;; (use-package orderless
;;   :straight t
;;   :custom
;;   (completion-styles '(orderless basic))
;;   (completion-category-defaults nil)
;;   (completion-category-overrides '((file (styles partial-completion))))
;;   :custom-face
;;   (orderless-match-face-0 ((t :italic t)))
;;   (orderless-match-face-1 ((t :inherit orderless-match-face-0)) face-defface-spec)
;;   (orderless-match-face-2 ((t :inherit orderless-match-face-0)) face-defface-spec)
;;   (orderless-match-face-3 ((t :inherit orderless-match-face-0)) face-defface-spec))

;;; IBUFFER

;; (use-package ibuffer
;;   :straight (ibuffer :type built-in)
;;   :general
;;   ("M-O" 'ibuffer)
;;   :hook
;;   (ibuffer-mode . (lambda () (display-line-numbers-mode 1)))
;;   (ibuffer-mode . (lambda () (ibuffer-auto-mode 1)))
;;   (ibuffer-mode . (lambda () (ibuffer-switch-to-saved-filter-groups "Default")))
;;   :config
;;   (evil-set-initial-state 'ibuffer-mode 'normal)
;;   (general-define-key
;;    :keymaps 'ibuffer-mode-map
;;    "M-O" 'my/hydra-ibuffer/body)
;;   (general-define-key
;;    :states '(normal visual)
;;    :keymaps 'ibuffer-mode-map
;;    "M-j" 'ibuffer-forward-filter-group
;;    "M-k" 'ibuffer-backward-filter-group
;;    "RET" 'ibuffer-visit-buffer
;;    "m" 'ibuffer-mark-forward
;;    "u" 'ibuffer-unmark-forward)
;;   (pretty-hydra-define my/hydra-ibuffer
;;     (:color blue :title "Ibuffer" :quit-key "M-O")
;;     ("Filter"
;;      (("fm" (lambda () (interactive) (ibuffer-toggle-marks) (ibuffer-do-kill-lines)) "Marked")
;;       ("fu" ibuffer-do-kill-lines "Unmarked")
;;       ("fM" ibuffer-filter-by-mode "Major Mode")
;;       ("fn" ibuffer-filter-by-name "Name")
;;       ("fN" ibuffer-filter-by-filename "Filename")
;;       ("fd" ibuffer-filter-by-directory "Directory")
;;       ("fc" ibuffer-filter-by-content "Content"))
;;      "Mark"
;;      (("ma" (lambda () (interactive) (ibuffer-unmark-all nil) (ibuffer-toggle-marks)) "All")
;;       ("mu" ibuffer-unmark-all "Unmark All")
;;       ("mM" ibuffer-mark-by-mode "Major Mode")
;;       ("mn" ibuffer-mark-by-name-regexp "Name (regex)")
;;       ("mN" ibuffer-mark-by-file-name-regexp "Filename (regex)")
;;       ("mc" ibuffer-mark-by-content-regexp "Content (regex)"))
;;      "Sort"
;;      (("sa" ibuffer-do-sort-by-alphabetic "Alphabetical")
;;       ("sr" ibuffer-do-sort-by-recency "Recency")
;;       ("ss" ibuffer-do-sort-by-size "Size"))
;;      "Act"
;;      (("ds" ibuffer-do-shell-command-file "Shell Command File")
;;       ("dS" ibuffer-do-shell-command-pipe "Shell Command Pipe")
;;       ("dk" ibuffer-do-delete "Kill Buffers")
;;       ("dg" ibuffer-do-occur "Grep"))))
;;   :custom
;;   (ibuffer-default-sorting-mode 'alphabetic)
;;   (ibuffer-saved-filter-groups
;;    '(("Default"
;;       ("Shell" (mode . shell-mode))
;;       ("Dired" (mode . dired-mode))
;;       ("Erc"   (mode . erc-mode))
;;       ("Eww"   (mode . eww-mode))
;;       ("MU4E"  (or (name . "^\\*mu4e")
;;                    (mode . mu4e-compose-mode)))
;;       ("Help"  (mode . helpful-mode))
;;       ("Manual" (or (mode . Man-mode)
;;                     (mode . Info-mode)))
;;       ("Emacs" (or
;;                 (name . "^\\*scratch\\*$")
;;                 (name . "^\\*GNU Emacs\\*$")
;;                 (name . "^\\*straight-process\\*$")
;;                 (name . "^\\*Backtrace\\*$")
;;                 (name . "^\\*ielm\\*$")
;;                 (name . "^\\*tramp")
;;                 (name . "^\\*Apropos\\*$")
;;                 (name . "^\\*Warnings\\*$")
;;                 (name . "^\\*Messages\\*$"))))))
;;   (ibuffer-formats
;;    '((mark
;;       (name 25 25 :left :elide)
;;       " "
;;       (mode 16 16 :left :elide)
;;       " " filename-and-process))))

;;; EWW

(use-package eww
  :straight (eww :type built-in)
  :commands eww
  :config
  (advice-add 'eww-back-url :after '(lambda (&rest _) (recenter)))
  (advice-add 'eww-forward-url :after '(lambda (&rest _) (recenter)))
  (advice-add 'eww-follow-link :after '(lambda (&rest _) (recenter)))
  (general-define-key
   :keymaps 'eww-mode-map
   :states '(normal insert motion)
   "RET" 'eww-follow-link
   "M-," 'eww-back-url
   "M-." 'eww-forward-url)
  (general-define-key
   :keymaps 'eww-bookmark-mode-map
   :states '(normal insert motion)
   "RET" 'eww-bookmark-browse)
  :custom-face
  (erc-input-face ((t :inherit erc-default-face)) face-defface-spec))

;;; IFLIPB

(use-package iflipb
  :straight t
  :general
  ("C-." 'iflipb-next-buffer
   "C-," 'iflipb-previous-buffer)
  :custom
  (iflipb-ignore-buffers nil)
  (iflipb-other-buffer-template "| %s"))

;;; DOCKER

(use-package dockerfile-mode
  :straight t
  :magic ("Dockerfile" . dockerfile-mode))

(use-package docker-tramp
  :straight t
  :custom
  (docker-tramp-use-names t))

;;; NOTMUCH

;; (use-package notmuch
;;   :straight t
;;   :custom
;;   (notmuch-fcc-dirs "SENT")
;;   (notmuch-saved-searches
;;    '((:name "Unread"
;;             :query "tag:inbox and tag:unread"
;;             :count-query "tag:inbox and tag:unread"
;;             :sort-order newest-first)
;;      (:name "Inbox"
;;             :query "tag:inbox"
;;             :count-query "tag:inbox"
;;             :sort-order newest-first)
;;      (:name "Archive"
;;             :query "tag:archive"
;;             :count-query "tag:archive"
;;             :sort-order newest-first)
;;      (:name "Sent"
;;             :query "tag:sent or tag:replied"
;;             :count-query "tag:sent or tag:replied"
;;             :sort-order newest-first)
;;      (:name "Trash"
;;             :query "tag:deleted"
;;             :count-query "tag:deleted"
;;             :sort-order newest-first)))
;;   :config
;;   (evil-collection-init 'notmuch))

;;; SHELLHIST

;;(load "~/p/shellhist/shellhist.el")
;;(shellhist-mode 1)
;;(add-to-list 'shellhist-filters 'my/string-contains-root-password)
;;(add-to-list 'shellhist-filters "^cd ?$")
;;(add-to-list 'shellhist-filters "^ls$")
;;(add-to-list 'shellhist-filters #'(lambda (str) (<= (length str) 3)))
;;(setq shellhist-max-hist-size 2000)
;;(add-hook 'shell-mode-hook 'shellhist-mode)
;;(general-define-key
;; :keymaps 'shell-mode-map
;; "C-r" '(lambda () (interactive)
;;		  (let ((vertico-prescient-enable-sorting nil)
;;				(vertico-sort-function nil))
;;			(shellhist-history-search))))

;;; GITLAB CI

(use-package gitlab-ci-mode
  :straight t
  :magic ("\\.gitlab-ci\\.yml" . gitlab-ci-mode))

;;; ELDOC

(use-package eldoc
  :straight (eldoc :type built-in)
  :blackout
  :commands (eldoc eldoc-mode)
  :custom
  (eldoc-idle-delay 0)
  (eldoc-echo-area-use-multiline-p nil))

;;; FLYMAKE

(use-package flymake
  :straight (flymake :type built-in)
  :commands flymake-mode
  :config
  (advice-add 'flycheck-goto-next-error :around 'my/better-jumper--around)
  (advice-add 'flycheck-goto-prev-error :around 'my/better-jumper--around))

;;; PACKAGE LINT

(use-package package-lint
  :straight t
  :commands package-lint-current-buffer)

;;; EBUILD MODE

(use-package ebuild-mode
  :straight t)

;;; CLUE

(use-package clue
  :straight (clue :type git :host github :repo "AmaiKinono/clue")
  :blackout
  :defer t
  :init
  (add-hook 'find-file-hook #'clue-auto-enable-clue-mode)
  :custom
  (clue-project-root-function #'(lambda () nil))
  :config
  (defun my/clue-goto ()
	(interactive)
	(when-let* ((loc (clue--link-location-at-point))
				(aw-dispatch-when-more-than 1)
				(aw (aw-select "Ace Window")))
	  (select-window aw)
	  (clue--goto-location loc)
	  (run-hooks 'clue-after-jump-hook))))

;;; GGTAGS

(use-package ggtags
  :straight t
  :custom
  (ggtags-mode-sticky nil)
  (ggtags-enable-navigation-keys nil)
  (ggtags-split-window-function #'(lambda () nil))
  ;; :hook
  ;; (c-mode-common . (lambda ()
  ;;                    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
  ;;                      (ggtags-mode 1))))
  )

;;; HEXL

(use-package hexl
  :straight (hexl :type built-in)
  :commands hexl-find-file
  :config
  (general-define-key
   :keymaps 'hexl-mode-map
   "C-f" nil
   "C-a" nil
   "M-z" nil))

;;; EDITORCONFIG

(use-package editorconfig
  :straight t
  :blackout " ec"
  :after ws-butler
  :custom
  (editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  :config
  (editorconfig-mode 1))

;;; GUD

(use-package gud
  :straight (gud :type built-in)
  :commands (gud-gdb perldb jdb pdb guiler dbx xdb sdb)
  :init
  (fset 'gdb 'gud-gdb)
  :config
  ;; (general-define-key
  ;;  :keymaps 'gud-mode-map
  ;;  :states 'insert
  ;;  "TAB" nil)
  (defadvice gud-display-line (after my/gud-display-line-centered act)
    (let ((bf (gud-find-file true-file)))
      (save-excursion
        (with-selected-window (get-buffer-window bf)
          (save-restriction
            (goto-line (ad-get-arg 1))
            (recenter)
            (which-func-update)))
        (set-buffer bf)))))

;;; SHMAN

;; (use-package shman
;;   :load-path "/home/_73/p/shman"
;;   :custom
;;   (shman-cd-function #'(lambda () projectile-known-projects))
;;   :config
;;   (general-define-key
;;    "M-SPC" 'shman-pop-shell
;;    "M-S-SPC" 'shman-pop-shell-autocd
;;    "C-M-SPC" 'shman-pop-one-time-shell))

;;; STRACE

(use-package strace-mode
  :straight t
  :magic (".*\\.strace$" . strace-mode))

;;; PROLOG

(use-package prolog ; https://bruda.ca/_media/emacs/prolog.el
  :load-path "/home/_73/.emacs.d/my/prolog"
  :commands (prolog-mode run-prolog)
  :custom
  (prolog-program-name '((t "/home/_73/.local/bin/scryer-prolog"))))

(use-package ediprolog
  :straight t
  :after prolog
  :custom
  (ediprolog-program "/home/_73/.local/bin/scryer-prolog"))

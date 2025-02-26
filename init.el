;;;; init.el -*- lexical-binding: t; -*-

(setq debug-on-error t)
(add-hook 'after-init-hook #'(lambda () (setq debug-on-error nil)))

;;; DEFAULT

(use-package emacs
  :custom
  (confirm-kill-emacs #'y-or-n-p)
  (enable-recursive-minibuffers t)
  (user-full-name "Nicholas Hubbard")
  (user-mail-address "nicholashubbard@posteo.net")
  (display-time-format "%H:%M")
  (display-time-default-load-average nil)
  (scroll-step 1)
  (scroll-conservatively 101)
  (coding-system-for-write 'utf-8)
  (display-line-numbers-type 'relative)
  (redisplay-dont-pause t)
  (recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-max-saved-items 1000)
  (auto-save-default nil)
  (delete-by-moving-to-trash nil)
  (make-backup-files nil)
  (disabled-command-function nil)
  (auto-revert-use-notify nil)
  (warning-minimum-level :error)
  (kill-buffer-query-functions nil)
  (display-buffer-base-action '(display-buffer-same-window))
  :config
  (scroll-bar-mode 0)
  (set-fringe-mode 0)
  (tooltip-mode 0)
  (tool-bar-mode 0)
  (show-paren-mode 0)
  (blink-cursor-mode 0)
  (electric-pair-mode 1)
  (recentf-mode 1)
  (which-function-mode 1)
  (display-time-mode 1)
  (global-auto-revert-mode 1)
  (transient-mark-mode 1)
  (delete-selection-mode 1)
  (prefer-coding-system 'utf-8)
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq-default require-final-newline t)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  :hook
  (prog-mode . display-line-numbers-mode)
  (text-mode . display-line-numbers-mode))

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

;;; BLACKOUT

(use-package blackout
  :straight t)

;;; THEME

;; dark mode
(invert-face 'default)

;;; UNDO FU

(use-package undo-fu
  :straight t
  :custom
  (undo-fu-allow-undo-in-region t)
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   #'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") #'undo-fu-only-redo))

;;; MARGINALIA

(use-package marginalia
  :straight t
  :config
  (marginalia-mode 1))

;;; ACE WINDOW

(use-package ace-window
  :straight t
  :bind
  ("M-w" . ace-window)
  :custom
  (aw-dispatch-always-nil)
  (aw-dispatch-when-more-than 2)
  (aw-minibuffer-flag t))

;;; PRESCIENT

(use-package prescient
  :straight t
  :custom
  (prescient-save-file (concat user-emacs-directory "prescient"))
  (prescient-sort-full-matches-first t)
  :config
  (prescient-persist-mode 1))

;;; CONSULT

(use-package consult
  :straight t
  :custom
  (consult-buffer-sources '(consult--source-buffer consult--source-recent-file))
  (consult-preview-key 'any)
  :config
  (global-set-key (kbd "M-o") #'consult-buffer))

;;; EMBARK

(use-package embark
  :straight t
  :bind
  ("C-." . embark-act)
  ("C-h B" . embark-bindings)
  :custom
  (embark-prompter #'embark-completing-read-prompter)
  (embark-quit-after-action nil)
  (embark-indicators '(embark-minimal-indicator)))

(use-package embark-consult
  :ensure t  ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; VERTICO

(use-package vertico
  :straight t
  :init
  (vertico-mode 1)
  :custom
  (vertico-count 15)
  (vertico-scroll-margin 0))

(use-package vertico-prescient
  :straight t
  :config
  (vertico-prescient-mode 1))

;;; PROJECTILE

(use-package projectile
  :straight t
  :blackout
  :custom
  (projectile-project-search-path '("~/p"))
  (projectile-track-known-projects-automatically nil)
  (projectile-enable-caching nil)
  (projectile-indexing-method 'alien)
  (projectile-file-exists-remote-cache-expire nil)
  (projectile-sort-order 'recently-active)
  (projectile-current-project-on-switch 'keep)
  (projectile-find-dir-includes-top-level t)
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

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
  (avy-case-fold-search nil))

;;; CLEAN KILL RING

(use-package clean-kill-ring
  :straight t
  :config
  (clean-kill-ring-mode 1))

;;; CAPE

(use-package cape
  :straight t
  :config
  (add-to-list 'completion-at-point-functions 'cape-dabbrev)
  (add-to-list 'completion-at-point-functions 'cape-keyword)
  (add-to-list 'completion-at-point-functions 'cape-file)
  (add-to-list 'completion-at-point-functions 'cape-symbol)
  (add-to-list 'completion-at-point-functions 'cape-line)
  (add-to-list 'completion-at-point-functions 'cape-history)
  (add-to-list 'completion-at-point-functions 'cape-dict))

;;; CORFU

(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 3)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-cycle nil)
  (corfu-preselect 'first)
  (corfu-preview-current nil)
  (global-corfu-minibuffer t)
  :config
  (global-corfu-mode 1))

;;; RAINBOW DELIMITERS

(use-package rainbow-delimiters
  :straight t
  :blackout
  :commands rainbow-delimiters-mode)

;;; CTRLF

(use-package ctrlf
  :straight t
  :custom
  (ctrlf-auto-recenter t)
  (ctrlf-go-to-end-of-match nil)
  (ctrlf-default-search-style 'regexp)
  :config
  (ctrlf-mode 1)
  (define-key ctrlf-minibuffer-mode-map (kbd "C-n") #'ctrlf-next-match)
  (define-key ctrlf-minibuffer-mode-map (kbd "C-p") #'ctrlf-previous-match))

;;; MU4E

(use-package mu4e
  :commands mu4e
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
  (mu4e-modeline-support nil))

;;; SHELL

(use-package shell
  :hook
  (shell-mode . (lambda ()
                  (shell-dirtrack-mode 0)
	              (setq-local dirtrack-list '("[[][^@]*@[^ ]* \\([^]]*\\)\\][$#] " 1))
                  (shell-dirtrack-mode 1))))

;;; BASH COMPLETION

(use-package bash-completion
  :straight t
  :custom
  (bash-completion-use-separate-processes t)
  :config
  (bash-completion-setup))

;;; SHELL POP

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
  (global-set-key (kbd "M-SPC") #'shell-pop)
  (global-set-key (kbd "M-S-SPC") #'(lambda () (interactive)
                                      (let ((shell-pop-autocd-to-working-dir t))
                                        (call-interactively #'shell-pop)))))

;;; SHX

(use-package shx
  :straight t
  :hook
  (shell-mode . shx-mode))

;;; SH SCRIPT

(use-package sh-script
  :commands sh-mode
  :custom
  (sh-indentation 4)
  (sh-basic-offset 4))

;;; SUDO EDIT

(use-package sudo-edit
  :straight t
  :commands sudo-edit
  :config
  (sudo-edit-indicator-mode 1))

;;; AVY

(use-package avy
  :straight t
  :bind
  ("C-," . avy-goto-char-2)
  :custom
  (avy-timeout-seconds 0.5)
  (avy-single-candidate-jump t)
  (avy-style 'at-full)
  (avy-case-fold-search nil))

;;; EXPAND REGION.

(use-package expand-region
  :straight t
  :bind
  ("C-=" . er/expand-region))

;;; EMACS LISP

(use-package elisp-mode
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . aggressive-indent-mode))

;;; ELDOC

(use-package eldoc
  :blackout
  :commands (eldoc eldoc-mode)
  :custom
  (eldoc-idle-delay 0)
  (eldoc-echo-area-use-multiline-p nil))

;;; HELPFUL

(use-package helpful
  :straight t
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command)
  ("C-h SPC" . helpful-at-point)
  ("C-h F" . helpful-function))

;;; ERC

(use-package erc
  :custom
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-kill-buffer-on-part t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-server-auto-reconnect t)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 5)
  (erc-max-buffer-size 30000)
  (erc-log-channels-directory "~/.irc-logs"))

;;; WHICH KEY

(use-package which-key
  :straight t
  :blackout
  :custom
  (which-key-idle-delay 0.8)
  (which-key-idle-secondary-delay 0.1)
  (which-key-prefix-prefix "++")
  (which-key-sort-order 'which-key-prefix-then-key-order)
  :config
  (which-key-mode 1))

;;; REALGUD

(use-package realgud
  :straight t)

;;; WS BUTLER

(use-package ws-butler
  :straight t
  :custom
  (ws-butler-keep-whitespace-before-point nil)
  :hook
  (prog-mode . ws-butler-mode)
  (text-mode . ws-butler-mode))

;;; MAGIT

(use-package magit
  :straight t
  :commands (magit magit-status)
  :custom
  (magit-clone-default-directory "~/p"))

;;; DIFF HL

(use-package diff-hl
  :straight t
  :custom
  (diff-hl-show-staged-changes nil)
  (diff-hl-update-async t)
  :config
  (global-diff-hl-mode 1)
  (diff-hl-margin-mode 1))

;;; EDITORCONFIG

(use-package editorconfig
  :straight t
  :blackout " ec"
  :custom
  (editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  :config
  (editorconfig-mode 1))

;;; MAN

(use-package man
  :commands man
  :custom
  (Man-notify-method 'pushy))

;;; GOOGLE THIS

(use-package google-this
  :straight t
  :commands (google-this google-this-search)
  :bind
  ("C-c g" . google-this-search))

;;; CPERL

(use-package cperl-mode
  :mode ("\\.p[lm]\\'" . cperl-mode)
  :custom
  (cperl-invalid-face nil)
  (cperl-indent-level 4)
  (cperl-close-parent-offset (- cperl-indent-leve))
  (cperl-indent-parens-as-block t)
  (cperl-electric-keywords nil)
  (cperl-electric-parens nil)
  (cperl-electric-lbrace-space nil)
  (cperl-extra-newline-before-brace-multiline nil)
  (cperl-auto-newline nil)
  :custom-face
  (cperl-array-face ((t :inherit font-lock-variable-name-face)) face-defface-spec)
  (cperl-hash-face  ((t :inherit font-lock-variable-name-face)) face-defface-spec))

;;; HASKELL

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :straight t)

;;; PROOF GENERAL

(use-package proof-general
  :straight t
  :mode ("\\.v\\'" . coq-mode)
  :custom
  (proof-splash-enable nil)
  (proof-three-window-mode-policy 'hybrid)
  (proof-script-fly-past-comments t)
  (coq-compile-before-require t))

;;; OCAML

(use-package tuareg
  :straight t
  :mode ("\\.mli?\\'" . tuareg-mode)
  :hook
  (tuareg-mode . rainbow-delimiters-mode)
  (tuareg-mode . (lambda ()
                   (setq-local comment-style 'multi-line)
                   (setq-local comment-continue "   "))))

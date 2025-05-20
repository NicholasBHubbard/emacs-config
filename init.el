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
  (display-line-numbers-type t)
  (redisplay-dont-pause t)
  (auto-save-default nil)
  (delete-by-moving-to-trash nil)
  (make-backup-files nil)
  (disabled-command-function nil)
  (warning-minimum-level :error)
  (kill-buffer-query-functions nil)
  (ring-bell-function #'ignore)
  (revert-without-query '(".*"))
  (use-package-enable-imenu-support t)
  (scroll-margin 1)
  (undo-in-region t)
  (inhibit-startup-screen t)
  (auto-revert-verbose nil)
  (display-buffer-base-action '(display-buffer-same-window))
  :config
  (scroll-bar-mode 0)
  (set-fringe-mode 0)
  (tooltip-mode 0)
  (tool-bar-mode 0)
  (show-paren-mode 0)
  (blink-cursor-mode 0)
  (display-time-mode 1)
  (global-auto-revert-mode 1)
  (transient-mark-mode 1)
  (delete-selection-mode 1)
  (prefer-coding-system 'utf-8)
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq-default require-final-newline t)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq-default display-fill-column-indicator-column 80)
  (setq-default fill-column 80)
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (global-set-key (kbd "M-r") #'revert-buffer-quick)
  (global-set-key (kbd "C-S-q") #'kill-current-buffer)
  :hook
  (prog-mode . display-line-numbers-mode)
  (text-mode . display-line-numbers-mode)
  (prog-mode . display-fill-column-indicator-mode)
  (text-mode . display-fill-column-indicator-mode))

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

;;; SMARTPARENS

(use-package smartparens
  :straight t
  :blackout
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  (require 'smartparens-config)
  :bind
  (:map smartparens-mode-map
        ("M-("   . sp-wrap-round)
        ("M-)"   . sp-unwrap-sexp)
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-k" . sp-kill-sexp))
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil))

;;; MARGINALIA

(use-package marginalia
  :straight t
  :config
  (marginalia-mode 1))

;;; WINNER

(use-package winner
  :init
  (setq winner-dont-bind-my-keys t)
  (winner-mode 1)
  :bind
  ("M-[" . winner-undo)
  ("M-]" . winner-redo))

;;; ACE WINDOW

(use-package ace-window
  :straight t
  :bind
  ("C-;" . ace-window)
  :custom
  (aw-dispatch-always-nil)
  (aw-dispatch-when-more-than 2)
  (aw-minibuffer-flag t))

;;; RECENTF

(use-package recentf
  :init
  (recentf-mode 1)
  :custom
  (recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-auto-cleanup 180)
  (recentf-max-saved-items 500)
  (recentf-exclude '("/tmp/" "/ssh:" "/sudo:" "/elpa/" "COMMIT_EDITMSG" ".*-autoloads\\.el$"))
  :bind
  ("C-c f" . recentf))

;;; EGLOT

(use-package eglot
  :custom
  (eglot-send-changes-idle-time 99999)
  (eglot-autoshutdown t)
  :commands (eglot eglot-ensure))

;;; PRESCIENT

(use-package prescient
  :straight t
  :custom
  (prescient-save-file (concat user-emacs-directory "prescient"))
  (prescient-sort-full-matches-first t)
  :config
  (prescient-persist-mode 1)
  (advice-add 'prescient--save :around
              (lambda (orig-fn &rest args)
                (let ((coding-system-for-write 'utf-8))
                  (apply orig-fn args)))))

;;; CONSULT

(use-package consult
  :straight t
  :custom
  (consult-buffer-sources '(consult--source-buffer consult--source-project-root consult--source-recent-file))
  (consult-preview-key "M-SPC")
  :bind
  ("M-o" . consult-buffer)
  ("M-g g" . consult-goto-line)
  ("C-S-y" . consult-yank-from-kill-ring))

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
  :init
  (projectile-mode 1)
  :custom
  (projectile-project-search-path '("~/p"))
  (projectile-track-known-projects-automatically nil)
  (projectile-enable-caching nil)
  (projectile-auto-discover nil)
  (projectile-indexing-method 'alien)
  (projectile-file-exists-remote-cache-expire nil)
  (projectile-sort-order 'recently-active)
  (projectile-current-project-on-switch 'keep)
  (projectile-find-dir-includes-top-level t)
  (projectile-switch-project-action #'projectile-dired)
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map)))

(use-package consult-projectile
  :straight t
  :bind
  (:map projectile-mode-map
        ("C-c P" . consult-projectile)))

;;; AGGRESSIVE INDENT

(use-package aggressive-indent
  :straight t
  :blackout
  :commands aggressive-indent-mode)

;;; CLEAN KILL RING

(use-package clean-kill-ring
  :straight t
  :config
  (clean-kill-ring-mode 1))

;;; CAPE

(use-package cape
  :straight t
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))

;;; CORFU

(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 3)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-cycle nil)
  (corfu-preselect 'first)
  (corfu-preview-current nil)
  (global-corfu-minibuffer t)
  :config
  (global-corfu-mode 1)
  (unbind-key "RET" corfu-map))

;;; PASS

(use-package pass
  :straight t
  :commands password-store-get)

(use-package auth-source-pass
  :custom
  (auth-source-debug t)
  (auth-source-do-cache nil)
  (auth-sources '(password-store))
  :config
  (auth-source-pass-enable))

;;; RAINBOW DELIMITERS

(use-package rainbow-delimiters
  :straight t
  :blackout
  :commands rainbow-delimiters-mode)

;;; CTRLF

(use-package ctrlf
  :straight t
  :init
  (ctrlf-mode 1)
  :custom
  (ctrlf-auto-recenter t)
  (ctrlf-go-to-end-of-match nil)
  (ctrlf-default-search-style 'regexp)
  :bind
  (:map ctrlf-minibuffer-mode-map
        ("C-n" . ctrlf-next-match)
        ("C-p" . ctrlf-previous-match)))

;;; ISEARCH

(use-package isearch
  :defer t
  :custom
  (isearch-wrap-pause 'no-ding))

;;; COMPILE

(use-package compile
  :commands compilation-mode
  :custom
  (compilation-ask-about-save nil))

;;; MULTIPLE CURSORS

(use-package multiple-cursors
  :straight t
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this)
  (:map mc/keymap
        ("<return>" . nil)))

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
  (mu4e-update-interval 180)
  (mu4e-index-lazy-check t)
  (mu4e-index-cleanup nil)
  (mu4e-hide-index-messages t)
  (mu4e-confirm-quit nil)
  (mu4e-headers-auto-update nil)
  (mu4e-view-inhibit-images t)
  (mu4e-change-filenames-when-moving t)
  (mu4e-compose-signature "Nicholas Hubbard")
  (mu4e-compose-signature-auto-include t)
  (mu4e-use-fancy-chars t)
  (mu4e-completing-read-function #'completing-read)
  (mu4e-modeline-support nil)
  :config
  (add-to-list 'display-buffer-alist `(,(regexp-quote mu4e-main-buffer-name) display-buffer-same-window)))

;;; EVIL NERD COMMENTER

(use-package evil-nerd-commenter
  :straight t
  :config
  (evilnc-default-hotkeys t))

;;; SHELL

(use-package shell
  :hook
  (shell-mode . (lambda () ; compatible with bash prompt - PS1='[\u@\h \w]\$ '
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
  :bind
  ("M-SPC" . shell-pop)
  ("M-S-SPC" . (lambda () (interactive)
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
  ("C-'" . avy-goto-char-timer)
  :custom
  (avy-timeout-seconds 0.6)
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
  (emacs-lisp-mode . aggressive-indent-mode)
  (emacs-lisp-mode . smartparens-strict-mode))

;;; IELM

(use-package ielm
  :commands ielm
  :hook
  (ielm-mode . rainbow-delimiters-mode)
  (ielm-mode . aggressive-indent-mode)
  (ielm-mode . smartparens-strict-mode))

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
  (erc-server-reconnect-timeout 15)
  (erc-max-buffer-size 30000)
  :config
  (defun my/erc-libera ()
    (interactive)
    (erc-tls :server "irc.libera.chat"
             :port 6697
             :nick "_73"
             :password (password-store-get "libera"))))

;;; WHICH KEY

(use-package which-key
  ;; :straight t (included in emacs 30)
  :blackout
  :custom
  (which-key-idle-delay 0.8)
  (which-key-idle-secondary-delay 0.1)
  (which-key-prefix-prefix "++")
  (which-key-max-display-columns 4)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-popup-type 'minibuffer)
  (which-key-allow-imprecise-window-fit t)
  (which-key-separator " → ")
  :config
  (which-key-mode 1))

;;; REALGUD

(use-package realgud
  :straight t
  :defer t)

;;; WS BUTLER

(use-package ws-butler
  :straight t
  :blackout
  :custom
  (ws-butler-keep-whitespace-before-point nil)
  :hook
  (prog-mode . ws-butler-mode)
  (text-mode . ws-butler-mode))

;;; MAGIT

(use-package magit
  :straight t
  :commands (magit magit-status)
  :bind
  ("C-x g" . magit-status)
  :custom
  (magit-clone-default-directory "~/p")
  :config
  (magit-auto-revert-mode 1))

;;; VC

(use-package vc
  :custom
  (mode-line-format (delete '(vc-mode vc-mode) mode-line-format))
  (vc-follow-symlinks t)
  (auto-revert-check-vc-info t))

;;; DIFF HL

(use-package diff-hl
  :straight t
  :init
  (global-diff-hl-mode 1)
  :custom
  (diff-hl-show-staged-changes nil)
  (diff-hl-update-async t)
  (diff-hl-ask-before-revert-hunk nil)
  :config
  (diff-hl-margin-mode 1)
  (diff-hl-flydiff-mode 1)
  :bind
  (:map diff-hl-mode-map
        ("C-c g" . diff-hl-command-map)))

;;; DIRED

(use-package dired
  :commands dired
  :hook
  (dired-mode . auto-revert-mode)
  (dired-mode . (lambda () (rename-buffer (concat "dired: " dired-directory))))
  :custom
  (dired-listing-switches "-DAlh --group-directories-first")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always))

;;; MAN

(use-package man
  :commands man
  :custom
  (Man-notify-method 'pushy)
  :bind
  ("C-h m" . man))

;;; GOOGLE THIS

(use-package google-this
  :straight t
  :commands (google-this google-this-search)
  :bind
  ("C-c g" . google-this-search))

;;; PERSPECTIVE

;; (use-package perspective
;;   :straight t
;;   :init
;;   (persp-mode 1)
;;   :bind
;;   ("M-o" . persp-switch-to-buffer*)
;;   ("M-O" . persp-switch-to-buffer)
;;   :custom
;;   (persp-mode-prefix-key (kbd "C-c v"))
;;   (persp-modestring-short t)
;;   (persp-state-default-file (expand-file-name "previous-state" (expand-file-name ".persp-states" user-emacs-directory)))
;;   :config
;;   (let ((persp-state-dir (file-name-directory persp-state-default-file)))
;;     (unless (file-directory-p persp-state-dir)
;;       (make-directory persp-state-dir)))
;;   :hook
;;   (kill-emacs . persp-state-save))

;;; CPERL

(use-package cperl-mode
  :init (fset 'perl-mode 'cperl-mode)
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
  (proof-three-window-enable nil)
  (proof-three-window-mode-policy 'hybrid)
  (proof-script-fly-past-comments t)
  (coq-compile-before-require t))

;;; OCAML

(use-package tuareg
  :straight t
  :mode ("\\.mli?\\'" . tuareg-mode)
  :hook
  ;; (tuareg-mode . eglot-ensure)
  (tuareg-mode . rainbow-delimiters-mode)
  (tuareg-mode . (lambda ()
                   (setq-local comment-style 'multi-line)
                   (setq-local comment-continue "   "))))

(use-package merlin
  :straight t
  :after tuareg
  :hook
  (tuareg-mode . merlin-mode))

;;; GOTO LAST CHANGE

(use-package goto-last-change
  :straight t
  :bind
  ("C-c l" . goto-last-change))

;;; COMINT

(use-package comint
  :commands comint-mode
  :bind
  (:map comint-mode-map
        ("C-l" . comint-clear-buffer)
        ([S-return] . (lambda () (interactive)
                        (comint-clear-buffer)
                        (comint-send-input)))))

;;; COMINT HISTORIES

(use-package comint-histories
  :straight t
  :demand t
  :bind
  (:map comint-mode-map
        ("C-r" . (lambda () (interactive)
                   (let ((vertico-sort-function nil)
                         (vertico-sort-override-function nil)
                         (vertico-prescient-enable-sorting nil))
                     (call-interactively #'comint-histories-search-history)))))
  :custom
  (comint-histories-global-filters '((lambda (x) (<= (length x) 3)) string-blank-p))
  :config
  (comint-histories-mode 1)
  (comint-histories-add-history gdb
    :predicates '((lambda () (string-match-p "^(gdb)" (comint-histories-get-prompt))))
    :length 2000
    :no-dups t)

  (comint-histories-add-history python
    :predicates '((lambda () (or (derived-mode-p 'inferior-python-mode)
                                 (string-match-p "^>>>" (comint-histories-get-prompt)))))
    :length 2000
    :no-dups t)

  (comint-histories-add-history ielm
    :predicates '((lambda () (derived-mode-p 'inferior-emacs-lisp-mode)))
    :length 2000
    :no-dups t)

  (comint-histories-add-history ocaml
    :predicates '((lambda () (derived-mode-p 'tuareg-interactive-mode)))
    :length 2000
    :no-dups t)

  (comint-histories-add-history debugger-generic
    :predicates '((lambda () (or (derived-mode-p 'gud-mode)
                                 (derived-mode-p 'realgud-mode))))
    :length 2000
    :no-dups t)

  (comint-histories-add-history shell-cds
    :predicates '((lambda () (derived-mode-p 'shell-mode))
                  (lambda () (string-match-p "^cd [~/]" (comint-histories-get-input))))
    :length 250
    :no-dups t)

  (comint-histories-add-history shell
    :predicates '((lambda () (derived-mode-p 'shell-mode)))
    :filters '("^ +" "^cd [^~/]" "^:e +[^~/]")
    :length 3500
    :ltrim nil
    :no-dups t))

;;; DPASTE

(use-package dpaste
  :straight t
  :defer t)

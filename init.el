;;;; init.el -*- lexical-binding: t; -*-

(setq debug-on-error t)
(add-hook 'after-init-hook #'(lambda () (setq debug-on-error nil)))

;;; DEFAULT

(use-package emacs
  :demand t
  :custom
  (user-full-name "Nicholas Hubbard")
  (user-mail-address "nicholashubbard@posteo.net")
  (confirm-kill-emacs #'y-or-n-p)
  (enable-recursive-minibuffers t)
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
  (mode-line-percent-position nil)
  (auto-revert-verbose nil)
  (display-buffer-base-action '(display-buffer-same-window))
  :config
  (invert-face 'default)
  (scroll-bar-mode 0)
  (set-fringe-mode 0)
  (tooltip-mode 0)
  (tool-bar-mode 0)
  (show-paren-mode 0)
  (blink-cursor-mode 0)
  (display-time-mode 1)
  (line-number-mode 0)
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
  :hook
  (prog-mode . display-line-numbers-mode)
  (text-mode . display-line-numbers-mode)
  (prog-mode . display-fill-column-indicator-mode)
  (text-mode . display-fill-column-indicator-mode)
  :bind
  ("M-r" . revert-buffer-quick)
  ("C-q" . kill-current-buffer)
  ("C-S-q" . kill-buffer-and-window))

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

;;; HYDRA

(use-package pretty-hydra
  :straight t
  :demand t
  :bind
  (:map hydra-base-map
        ("q"   . hydra-keyboard-quit)
        ("C-g" . hydra-keyboard-quit)))

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
  ("C-:" . ace-swap-window)
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
  ("C-S-y" . consult-yank-from-kill-ring)
  ("M-G" . consult-grep)
  ("M-F" . consult-find))

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

;;; CORFU

(use-package corfu
  :straight t
  :init
  (global-corfu-mode 1)
  (corfu-history-mode 1)
  :custom
  (corfu-cycle t)
  :bind
  (:map corfu-mode-map
        ("M-SPC" . completion-at-point)))

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
  (compilation-ask-about-save nil)
  (compilation-scroll-output t)
  (compilation-max-output-line-length nil)
  :bind
  ("<f6>" . recompile)
  :hook
  (compilation-filter . (lambda ()
                          (let ((inhibit-read-only t))
                            (ansi-color-apply-on-region (point-min) (point-max))))))

;;; EVIL NERD COMMENTER

(use-package evil-nerd-commenter
  :straight t
  :config
  (evilnc-default-hotkeys t))

;;; SHELL

(use-package shell
  :hook
  (shell-mode . (lambda () ; compatible with shell prompt: PS1='[\u@\h \w]\$ '
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
  ("M-SPC" . (lambda (arg) (interactive "P")
               (let ((shell-pop-autocd-to-working-dir arg))
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
  ("C-=" . er/expand-region)
  ("C-+" . er/contract-region))

;;; EMACS LISP

(use-package elisp-mode
  :hook
  (emacs-lisp-mode . which-function-mode)
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
  (magit-clone-default-directory "~/p/")
  (git-commit-post-finish-hook-timeout 10)
  :config
  (magit-auto-revert-mode 1)
  :hook
  (git-commit-post-finish . (lambda ()
                              (let* ((latest-commit (magit-rev-parse "HEAD"))
                                     (snd-latest-commit (magit-rev-parse "HEAD~1"))
                                     (git-root (magit-toplevel))
                                     (changed-files (magit-changed-files latest-commit snd-latest-commit)))
                                (dolist (file changed-files)
                                  (when-let ((buf (find-buffer-visiting (concat git-root file))))
                                    (with-current-buffer buf
                                      (revert-buffer t t t))))))))

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
        ("C-c g" . diff-hl-hydra/body))
  :pretty-hydra
  ((:color pink :quit-key "q")
   ("diff-hl"
    (("n" diff-hl-next-hunk "next hunk")
     ("p" diff-hl-previous-hunk "previous hunk")
     ("s" diff-hl-stage-current-hunk "stage hunk")
     ("r" diff-hl-revert-hunk "revert hunk")))))

;;; DIRED

(use-package dired
  :commands dired
  :hook
  (dired-mode . auto-revert-mode)
  (dired-mode . (lambda () (rename-buffer (concat "dired: " dired-directory))))
  :custom
  (dired-listing-switches "-DAlh --group-directories-first")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  :bind
  (:map dired-mode-map
        ("SPC" . dired-hydra/body))
  :pretty-hydra
  ((:color blue :quit-key "SPC")
   ("Command"
    (("rm" dired-do-delete "Remove")
     ("mv" dired-do-rename "Move")
     ("cp" dired-do-copy "Copy")
     ("cm" dired-do-chmod "Chmod")
     ("co" dired-do-chown "Chown")
     ("mf" dired-create-empty-file "Make File")
     ("md" dired-create-directory "Make Dir"))
    "Act"
    (("sh" dired-do-shell-command "Shell Command")
     ("SH" dired-do-async-shell-command "Shell Command Async")
     ("un" dired-undo "Undo")
     ("ca" dired-do-compress-to "Compress to Archive"))
    "Find"
    (("fg" find-grep-dired "Find Grep Regexp")
     ("fn" find-name-dired "Find Name"))
    "Mark"
    (("ma" (lambda () (interactive) (dired-unmark-all-marks) (dired-toggle-marks)) "All" :color red)
     ("um" dired-unmark-all-marks "Unmark All" :exit nil)
     ("re" dired-mark-files-regexp "Regexp" :exit nil)
     ("RE" dired-mark-files-containing-regexp "Containing Regexp" :exit nil)
     ("su" dired-mark-suffix "Suffix" :exit nil)
     ("di" dired-mark-directories "Directories" :exit nil)))))

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
  :commands (google-this google-this-search))

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
  (cperl-close-parent-offset (- cperl-indent-level))
  (cperl-indent-parens-as-block t)
  (cperl-electric-keywords nil)
  (cperl-electric-parens nil)
  (cperl-extra-newline-before-brace-multiline nil)
  (cperl-auto-newline nil)
  :custom-face
  (cperl-array-face ((t :inherit font-lock-variable-name-face)) face-defface-spec)
  (cperl-hash-face  ((t :inherit font-lock-variable-name-face)) face-defface-spec)
  :hook
  (cperl-mode . which-function-mode)
  :bind
  (:map cperl-mode-map
        ("{" . nil)))

;;; PYTHON

(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :hook
  (python-mode . which-function-mode))

(use-package pyvenv
  :straight t
  :after python-mode
  :hook
  (python-mode . (lambda () (pyvenv-mode 1))))

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
  :hook
  (comint-mode . ansi-color-for-comint-mode-on)
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

  (comint-histories-add-history pdb
    :predicates '((lambda () (string-match-p "^(Pdb)" (comint-histories-get-prompt))))
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

;;; XCSCOPE

(use-package xcscope
  :straight t)

;;; GNUS

(use-package gnus
  :commands gnus
  :custom
  (gnus-group-buffer "*gnus*")
  (gnus-select-method '(nnnil nil))
  (gnus-secondary-select-methods
   '((nnimap "nicholashubbard@posteo.net"
             (nnimap-user "nicholashubbard@posteo.net")
             (nnimap-address "posteo.de"))
     ;; (nnimap "nhubbard@redhat.com"
     ;;         (nnimap-user "nhubbard@redhat.com")
     ;;         (nnimap-address "imap.gmail.com"))
     ))
  (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
  (gnus-use-full-window nil))

;;; GPTEL

(use-package gptel
  :straight t
  :commands gptel
  :bind
  ("C-c RET" . gptel-send)
  :custom
  (gptel-api-key #'(lambda () (password-store-get "openai-api-key")))
  :config
  (gptel-make-anthropic "Claude" :stream t :key #'(lambda () (password-store-get "anthropic-api-key"))))

;;; JINJA

(use-package jinja2-mode
  :straight t
  :commands jinja2-mode
  :bind
  (:map jinja2-mode-map
        ("M-o" . nil)))

;;; DIFF

(use-package diff
  :commands (diff diff-mode)
  :bind
  (:map diff-mode-map
        ("M-o" . nil)))

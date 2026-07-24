;;;; init.el -*- lexical-binding: t; -*-

(setq debug-on-error t)
(add-hook 'after-init-hook #'(lambda () (setq debug-on-error nil)))

;;; USE PACKAGE

(use-package use-package
  :demand t
  :custom
  (use-package-hook-name-suffix nil))

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
  (auto-save-default nil)
  (delete-by-moving-to-trash nil)
  (make-backup-files nil)
  (disabled-command-function nil)
  (warning-minimum-level :error)
  (kill-buffer-query-functions nil)
  (ring-bell-function #'ignore)
  (revert-without-query '(".*"))
  (use-package-enable-imenu-support t)
  (nobreak-char-display nil)
  (scroll-margin 1)
  (display-line-numbers-widen t)
  (undo-in-region t)
  (use-short-answers t)
  (inhibit-startup-screen t)
  (mode-line-percent-position nil)
  (auto-revert-verbose nil)
  (window-sides-vertical t)
  (gc-cons-threshold (* 2 gc-cons-threshold))
  (display-buffer-base-action '(display-buffer-same-window))
  :config
  (when-let ((font (seq-find (lambda (font) (find-font (font-spec :family font)))
                             '("Red Hat Mono" "Adwaita Mono" "Monospace"))))
    (set-face-attribute 'default nil :family font :height 120))
  (setq-default require-final-newline t)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq-default display-fill-column-indicator-column 80)
  (setq-default fill-column 80)
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (setq-default c-basic-offset 4)
  (invert-face 'default)
  (scroll-bar-mode 0)
  (menu-bar-mode 0)
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
  :hook
  (prog-mode-hook . display-line-numbers-mode)
  (text-mode-hook . display-line-numbers-mode)
  (prog-mode-hook . display-fill-column-indicator-mode)
  (text-mode-hook . display-fill-column-indicator-mode)
  :bind*
  ("C-S-<backspace>" . (lambda () (interactive) (call-interactively #'kill-whole-line)))
  ("C-M-p"       . (lambda () (interactive) (scroll-up 1)))
  ("C-M-n"       . (lambda () (interactive) (scroll-down 1)))
  ("C-M-z"       . delete-pair)
  ("C-M-<up>"    . enlarge-window)
  ("C-M-<down>"  . shrink-window)
  ("C-M-<left>"  . shrink-window-horizontally)
  ("C-M-<right>" . enlarge-window-horizontally)
  ("C-c j"       . join-line)
  ("M-r"         . revert-buffer-quick)
  :bind
  ("C-q"         . kill-current-buffer)
  ("C-S-q"       . kill-buffer-and-window))

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
  :straight t
  :demand t)

;;; EXEC PATH FROM SHELL

(use-package exec-path-from-shell
  :straight t
  :custom
  (exec-path-from-shell-shell-name "bash")
  (exec-path-from-shell-arguments '("-i"))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("SUDO_ASKPASS")))

;;; SERVER

(use-package server
  :ensure nil
  :if window-system
  :config
  (unless (server-running-p)
    (server-start)))

;;; MODUS THEMES

;; (use-package modus-themes
;;   :straight t
;;   :ensure t
;;   :custom
;;   (modus-themes-disable-other-themes t)
;;   (modus-themes-common-palette-overrides
;;    '((bg-line-number-active unspecified)
;;      (bg-line-number-inactive unspecified)))
;;   :config
;;   (load-theme 'modus-vivendi-tritanopia t))

;;; EF THEMES

;; (use-package ef-themes
;;   :straight t
;;   :custom
;;   (modus-themes-disable-other-themes t)
;;   :config
;;   (load-theme 'ef-dark t))

;;; HYDRA

(use-package pretty-hydra
  :straight t
  :demand t
  :bind
  (:map hydra-base-map
        ("q"   . hydra-keyboard-quit)
        ("C-g" . hydra-keyboard-quit)))

;; ;;; YASNIPPET

;; (use-package yasnippet
;;   :straight t
;;   :blackout yas-minor-mode
;;   :hook
;;   (prog-mode-hook . yas-minor-mode)
;;   (text-mode-hook . yas-minor-mode)
;;   (conf-mode-hook . yas-minor-mode)
;;   :config
;;   (yas-reload-all))

;; (use-package yasnippet-snippets
;;   :straight t
;;   :after yasnippet)

;; (use-package yasnippet-capf
;;   :straight t
;;   :preface
;;   (defun my/yas-capf-setup ()
;;     (setq-local corfu-auto-trigger ";")
;;     (add-hook 'completion-at-point-functions
;;               (cape-capf-properties
;;                (cape-capf-trigger #'yasnippet-capf ?\;)
;;                :annotation-function #'(lambda (&rest _args) " YAS"))
;;               nil t))
;;   :hook
;;   (prog-mode-hook . my/yas-capf-setup)
;;   (text-mode-hook . my/yas-capf-setup)
;;   (conf-mode-hook . my/yas-capf-setup))

;;; MARGINALIA

(use-package marginalia
  :straight t
  :config
  (marginalia-mode 1))

;;; TRANSIENT

(use-package transient
  :defer t
  :custom
  (transient-default-level 7))

;;; WINNER

(use-package winner
  :init
  (setq winner-dont-bind-my-keys t)
  (winner-mode 1)
  :bind*
  ("M-[" . winner-undo)
  ("M-]" . winner-redo))

;;; ACE WINDOW

(use-package ace-window
  :straight t
  :bind*
  ("C-;" . ace-window)
  :custom
  (aw-dispatch-always nil)
  (aw-scope 'frame)
  (aw-dispatch-when-more-than 2)
  (aw-minibuffer-flag t))

;;; RECENTF

(use-package recentf
  :init
  (recentf-mode 1)
  :bind*
  ("C-c r" . recentf)
  :custom
  (recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
  (recentf-auto-cleanup 180)
  (recentf-max-saved-items 500)
  (recentf-exclude '("^/tmp/" "^/ssh:" "^/sudo:" "/elpa/" "COMMIT_EDITMSG" ".*-autoloads\\.el$" file-remote-p)))

;;; FLYMAKE

(use-package flymake
  :defer t
  :commands (flymake-mode flymake-start)
  :custom
  (flymake-no-changes-timeout nil)
  (flymake-fringe-indicator-position nil)
  (flymake-fringe-indicators nil))

;;; ORG

;; (use-package org
;;   :ensure nil
;;   :bind* ("C-c c" . org-capture)
;;   :hook
;;   (org-mode . visual-line-mode)
;;   :custom
;;   (org-directory (concat user-emacs-directory "org/"))
;;   (org-default-notes-file (concat org-directory "brain.org"))

;;   (org-capture-templates
;;    '(("t" "TODO" entry (file org-default-notes-file)
;;       "* TODO: %?\n  [%a]\n  %U" :prepend t :empty-lines-after 2)

;;      ("r" "Remember" entry (file org-default-notes-file)
;;       "* NOTE: %?\n  [%a]\n  %U" :prepend t :empty-lines-after 2)))

;;   :config
;;   (setopt
;;    display-buffer-alist
;;    (cons '("\\*Org Select\\*" (display-buffer-below-selected))
;;          display-buffer-alist))
;;   (setopt
;;    display-buffer-alist
;;    (cons '("CAPTURE*" (display-buffer-below-selected))
;;          display-buffer-alist))


;;   (unless (file-directory-p org-directory)
;;     (make-directory org-directory)))

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

;;; WGREP

(use-package wgrep
  :straight t
  :commands wgrep-change-to-wgrep-mode
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

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
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 3)
  (corfu-auto-trigger nil)
  (corfu-cycle t)
  (corfu-on-exact-match nil)
  (corfu-preselect 'prompt)
  (corfu-preview-current nil)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t))

;;; CAPE

(use-package cape
  :straight t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;;; DABBREV

(use-package dabbrev
  :defer t
  :custom
  (dabbrev-case-fold-search nil))

;;; TAB BAR

(use-package tab-bar
  :defer t
  :custom
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-select-tab-modifiers '(meta))
  (tab-bar-tab-hints t)
  (tab-bar-auto-width nil)
  (tab-bar-new-tab-to 'right)
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  :custom-face
  (tab-bar-tab ((t (:foreground "dark green"))))
  (tab-bar-tab-inactive ((t (:foreground "red")))))

;;; PROJECT

(use-package project
  :defer t
  :custom
  (project-mode-line t)
  (project-vc-extra-root-markers '(".project"))
  (project-list-file (expand-file-name ".projects" user-emacs-directory))
  (project-switch-commands 'project-dired))

;;; OTPP

(use-package otpp
  :straight t
  :after project
  :custom
  (otpp-bury-on-kill-buffer-when-multiple-tabs nil)
  :init
  (otpp-mode 1)
  (otpp-override-mode 1))

;;; AGGRESSIVE INDENT

(use-package aggressive-indent
  :straight t
  :blackout
  :commands aggressive-indent-mode)

;;; YANK INDENT

(use-package yank-indent
  :straight (:host github :repo "jimeh/yank-indent")
  :commands yank-indent-mode)

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
  (auth-source-debug nil)
  (auth-source-do-cache nil)
  (auth-sources '(password-store "~/.authinfo.json.gpg"))
  :config
  (auth-source-pass-enable))

(use-package auth-source-xoauth2-plugin
  :straight t
  :after (:any gnus message)
  :config
  (auth-source-xoauth2-plugin-mode 1))

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
  :bind*
  ("C-s" . ctrlf-forward-default)
  (:map ctrlf-minibuffer-mode-map
        ("C-n" . ctrlf-next-match)
        ("C-p" . ctrlf-previous-match)))

;;; PROCED

(use-package proced
  :commands proced
  :custom
  (proced-auto-update-flag nil)
  (proced-auto-update-interval 1)
  (proced-goal-attribute nil)
  (proced-enable-color-flag t))

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
  :hook
  (compilation-filter-hook . (lambda ()
                               (let ((inhibit-read-only t))
                                 (ansi-color-apply-on-region compilation-filter-start (point-max))))))

;;; EAT

(use-package eat
  :straight t
  :commands (eat eat-mode))

;;; VTERM

(use-package vterm
  :straight t
  :commands (vterm vterm-mode))

;;; SHELL

(use-package shell
  :commands (shell shell-mode)
  :custom
  (shell-file-name "/bin/bash")
  (shell-kill-buffer-on-exit t)
  :hook
  (shell-mode-hook . (lambda () (setq-local corfu-auto nil)))
  (shell-mode-hook . (lambda ()
                       (shell-dirtrack-mode -1)
		               (add-hook 'comint-output-filter-functions #'comint-osc-process-output nil t))))

;;; GHOSTEL

(use-package ghostel
  :straight t
  :commands (ghostel ghostel-project)
  :custom
  (ghostel-module-auto-install 'download)
  (ghostel-initial-input-mode 'line)
  (ghostel-shell-integration t)
  (ghostel-tramp-shell-integration t)
  (ghostel-line-mode-use-bash-completion t)
  (ghostel-enable-osc52 t)
  :bind*
  (:map ghostel-line-mode-map
        ([S-return] . (lambda () (interactive) (ghostel-clear-scrollback) (ghostel-line-mode-send)))))

;;; BASH COMPLETION

(use-package bash-completion
  :straight t
  :after shell
  :custom
  (bash-completion-use-separate-processes nil)
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
  (shell-pop-per-window t)
  (shell-pop-pop-under-shell t)
  :bind*
  ("M-SPC" . (lambda () (interactive)
               (let ((default-directory (if (file-remote-p default-directory)
                                            "~" default-directory)))
                 (call-interactively #'shell-pop))))
  ("M-S-SPC" . (lambda () (interactive)
                 (let ((shell-pop-autocd-to-working-dir t)
                       (default-directory (if (file-remote-p default-directory)
                                              "~" default-directory)))
                   (call-interactively #'shell-pop)
                   (comint-clear-buffer)))))

;;; SHX

(use-package shx
  :straight t
  :hook
  (shell-mode-hook . shx-mode))

;;; SH SCRIPT

(use-package sh-script
  :commands sh-mode
  :custom
  (sh-indentation 4)
  (sh-basic-offset 4))

;;; SUDO EDIT

(use-package sudo-edit
  :straight t
  :commands (sudo-edit sudo-edit-find-file)
  :config
  (sudo-edit-indicator-mode 1))

;;; AVY

;; (use-package avy
;;   :straight t
;;   :bind*
;;   ("C-'" . avy-goto-char-timer)
;;   :custom
;;   (avy-timeout-seconds 0.4)
;;   (avy-single-candidate-jump t)
;;   (avy-style 'at-full)
;;   (avy-case-fold-search nil))

;;; FLASH

(use-package flash
  :straight t
  :bind*
  ("C-'" . flash-jump)
  :custom
  (flash-multi-window t)
  (flash-jumplist t)
  (flash-label-uppercase t)
  (flash-rainbow t)
  (flash-rainbow-shade 5))

;;; EXPAND REGION

(use-package expand-region
  :straight t
  :custom
  (expand-region-smart-cursor nil)
  (expand-region-skip-whitespace t)
  (expand-region-subword-enabled nil)
  :bind*
  ("C-=" . er/expand-region))

;;; ELECTRIC PAIR

(use-package elec-pair
  :hook
  (prog-mode-hook . electric-pair-local-mode)
  (text-mode-hook . electric-pair-local-mode))

;;; MULTIPLE CURSORS

(use-package multiple-cursors
  :straight t
  :preface
  (define-prefix-command 'my/multiple-cursors-map)
  :custom
  (mc/always-run-for-all t)
  :bind-keymap*
  ("C-c m" . my/multiple-cursors-map)
  :bind*
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  (:map mc/keymap ("RET" . nil))
  (:map my/multiple-cursors-map
        ("n" . mc/mark-next-like-this)
        ("p" . mc/mark-previous-like-this)
        ("a" . mc/mark-all-like-this)
        ("d" . mc/mark-all-like-this-dwim)
        ("s" . mc/skip-to-next-like-this)
        ("S" . mc/skip-to-previous-like-this)
        ("u" . mc/unmark-next-like-this)
        ("U" . mc/unmark-previous-like-this)
        ("l" . mc/edit-lines)
        ("b" . mc/edit-beginnings-of-lines)
        ("e" . mc/edit-ends-of-lines)
        ("#" . mc/insert-numbers)
        ("A" . mc/insert-letters)))

;;; EMACS LISP

(use-package elisp-mode
  :hook
  (emacs-lisp-mode-hook . rainbow-delimiters-mode)
  (emacs-lisp-mode-hook . aggressive-indent-mode)

;;; IELM

  (use-package ielm
    :commands ielm
    :hook
    (ielm-mode-hook . rainbow-delimiters-mode)
    (ielm-mode-hook . aggressive-indent-mode))

;;; ELDOC

(use-package eldoc
  :blackout
  :commands (eldoc eldoc-mode)
  :custom
  (eldoc-idle-delay 0.5)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly))

;;; HELPFUL

(use-package helpful
  :straight t
  :bind*
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command)
  ("C-h SPC" . helpful-at-point)
  ("C-h F" . helpful-function))

;;; ERC

(use-package erc
  :commands (erc erc-tls)
  :custom
  (erc-nick "seven3")
  (erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "353" "366"))
  (erc-kill-buffer-on-part t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-server-auto-reconnect t)
  (erc-track-position-in-mode-line nil)
  (erc-max-buffer-size 1150)
  (erc-user-full-name erc-nick)
  (erc-modules '(button completion fill irccontrols match move-to-prompt networks notifications ring stamp))
  :bind
  (:map erc-mode-map
        ("C-q" . bury-buffer))
  :hook
  (erc-after-connect . my/erc-znc-request-playback)
  :config
  (remove-hook 'erc-kill-channel-hook #'erc-part-channel-on-kill)
  :init
  (defun my/erc-znc-request-playback (_server _nick)
    (erc-send-line "ZNC *playback PLAY * 0" #'ignore))

  (defun my/slackserver-ssh-znc-tunnel (&optional arg)
    (let ((tunnel-process (get-process "slackserver-ssh-znc-tunnel")))
      (when (and tunnel-process
                 (or arg (not (process-live-p tunnel-process))))
        (message "Deleting existing SSH tunnel...")
        (delete-process tunnel-process)
        (setq tunnel-process nil)
        (sleep-for 2))
      (when (or (not tunnel-process) (not (process-live-p tunnel-process)))
        (message "Starting SSH tunnel...")
        (unless (processp
                 (make-process :name "slackserver-ssh-znc-tunnel"
                               :command '("ssh" "-p" "22"
                                          "-L" "6697:localhost:6697" "-n" "-N"
                                          "-o" "ServerAliveInterval=60"
                                          "-o" "ServerAliveCountMax=3"
                                          "slackserver")
                               :buffer " *slackserver-ssh-znc-tunnel*"
                               :connection-type 'pty
                               :sentinel #'(lambda (_ msg)
                                             (when (string-match "exited abnormally" msg)
                                               (when-let ((buf (get-buffer " *slackserver-ssh-znc-tunnel*")))
                                                 (kill-buffer buf))))))
          (user-error "Failed to create ssh tunnel"))
        (sleep-for 5)
        (with-current-buffer " *slackserver-ssh-znc-tunnel*"
          (add-hook
           'kill-buffer-hook
           #'(lambda ()
               (dolist (buf (match-buffers '(derived-mode . erc-mode)))
                 (kill-buffer buf)))
           nil t)))))

  (defun my/erc (&optional arg)
    (interactive "P")
    (when-let ((pass (password-store-get "znc-admin")))
      (my/slackserver-ssh-znc-tunnel arg)
      ;; (erc-tls :server "localhost"
      ;;          :port 6697
      ;;          :id "*znc-perl-server*"
      ;;          :nick erc-nick
      ;;          :password (concat "admin@erc/perl:" pass))
      ;; (erc-tls :server "localhost"
      ;;          :port 6697
      ;;          :id "*znc-overnet-canary-server*"
      ;;          :nick erc-nick
      ;;          :password (concat "admin@erc/overnet:" pass))
      (erc-tls :server "localhost"
               :port 6697
               :id "*znc-libera-server*"
               :nick erc-nick
               :password (concat "admin@erc/libera:" pass))))

  (defun my/erc-regain-nick ()
    (interactive)
    (erc-move-to-prompt)
    (erc-kill-input)
    (erc-send-line (concat "PRIVMSG NickServ REGAIN " erc-nick " " (password-store-get "irc")) #'ignore)))

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
  (prog-mode-hook . ws-butler-mode)
  (text-mode-hook . ws-butler-mode))

;;; MAGIT

(use-package magit
  :straight t
  :commands (magit magit-status)
  :custom
  (magit-clone-default-directory "~/p/")
  (magit-tramp-pipe-stty-settings 'pty)
  (magit-commit-show-diff nil)
  (magit-branch-direct-configure nil)
  (magit-refresh-status-buffer nil)
  (git-commit-major-mode 'git-commit-elisp-text-mode)
  :config
  (magit-auto-revert-mode 1))

;;; MAGIT TODOS

(use-package magit-todos
  :straight t
  :after magit
  :config
  (magit-todos-mode 1)
  :custom
  (magit-todos-keywords-list '("TODO" "HACK"))
  (magit-todos-branch-list nil))

;;; VC

(use-package vc
  :defer t
  :custom
  (mode-line-format (remove '(vc-mode vc-mode) mode-line-format))
  (vc-follow-symlinks t)
  (auto-revert-check-vc-info t))

;;; GIT TIMEMACHINE

(use-package git-timemachine
  :straight t
  :commands git-timemachine)

;;; DIFF HL

(use-package diff-hl
  :straight t
  :init
  (global-diff-hl-mode 1)
  :custom
  (diff-hl-show-staged-changes nil)
  (diff-hl-update-async nil)
  (diff-hl-ask-before-revert-hunk nil)
  (diff-hl-next-previous-hunk-auto-recenter t)
  :config
  (diff-hl-margin-mode 1)
  (diff-hl-flydiff-mode 0)
  :hook
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :bind
  (:map diff-hl-mode-map
        ("C-c g" . diff-hl-hydra/body))
  :pretty-hydra
  ((:color pink :quit-key "q")
   ("diff-hl"
    (("n" diff-hl-next-hunk "next hunk")
     ("p" diff-hl-previous-hunk "previous hunk")
     ("s" diff-hl-stage-current-hunk "stage hunk")
     ("r" diff-hl-revert-hunk "revert hunk")
     ("g" magit-status "magit" :exit t)))))

;;; TRAMP

(use-package tramp
  ;; :defer t
  :init
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)
  :custom
  (tramp-default-method "ssh")
  (remote-file-name-inhibit-locks t)
  (tramp-copy-size-limit (* 1024 1024))
  (tramp-use-scp-direct-remote-copying t)
  (remote-file-name-inhibit-auto-save-visited t)
  (tramp-default-remote-shell "/bin/bash")
  (tramp-connection-local-default-shell-variables
   '((shell-file-name . "/bin/bash")
     (shell-command-switch . "-c"))))

;;; DIRED

(use-package dired
  :commands dired
  :hook
  (dired-mode-hook . auto-revert-mode)
  (dired-mode-hook . (lambda () (rename-buffer (concat "dired: " dired-directory))))
  :custom
  (dired-listing-switches "-Alh --group-directories-first")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-clean-up-buffers-too t)
  :bind
  (:map dired-mode-map
        ("SPC" . dired-hydra/body)
        ("C-s" . dired-goto-file))
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
     ("ca" dired-do-compress-to "Compress to Archive")
     ("wd" wdired-change-to-wdired-mode "Wdired"))
    "Find"
    (("fg" find-grep-dired "Find Grep Regexp")
     ("fn" find-name-dired "Find Name"))
    "View"
    (("is" dired-maybe-insert-subdir "Insert Subdir" :exit nil)
     ("hs" dired-hide-subdir "Hide Subdir" :exit nil)
     ("ha" dired-hide-all "Hide All" :exit nil)
     ("ns" dired-next-subdir "Next Subdir" :exit nil)
     ("ps" dired-prev-subdir "Prev Subdir" :exit nil))
    "Mark"
    (("ma" (lambda () (interactive) (dired-unmark-all-marks) (dired-toggle-marks)) "All" :color red)
     ("um" dired-unmark-all-marks "Unmark All" :exit nil)
     ("re" dired-mark-files-regexp "Regexp" :exit nil)
     ("RE" dired-mark-files-containing-regexp "Containing Regexp" :exit nil)
     ("su" dired-mark-suffix "Suffix" :exit nil)
     ("di" dired-mark-directories "Directories" :exit nil)))))

(use-package dired-subtree
  :straight t
  :after dired
  :custom
  (dired-subtree-line-prefix "│    ")
  (dired-subtree-line-prefix-face nil)
  (dired-subtree-use-backgrounds t)
  :bind
  (:map dired-mode-map ("TAB" . dired-subtree-toggle)))

;;; MAN

(use-package man
  :commands man
  :custom
  (Man-notify-method 'pushy)
  (Man-prefer-synchronous-call t)
  :bind*
  ("C-h m" . man))

;;; GOOGLE THIS

(use-package google-this
  :straight t
  :commands (google-this google-this-search)
  :bind*
  ("C-c G" . google-this))

;;; LSP MODE

(use-package lsp-mode
  :straight t
  :blackout
  :commands (lsp lsp-deferred)
  :custom
  (lsp-auto-guess-root t)
  (lsp-enable-snippet nil)
  (lsp-enable-folding nil)
  (lsp-idle-delay 0.5)
  (lsp-enable-symbol-highlighting nil))

;;; CPERL

(use-package cperl-mode
  :hook
  (perl-mode-hook . cperl-mode)
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
  :bind
  (:map cperl-mode-map
        ("{" . nil)))

;;; PYTHON

(use-package python
  :mode ("\\.py\\'" . python-mode))

;; (use-package pet
;;   :straight t
;;   :blackout
;;   :after python
;;   :config
;;   (add-hook 'python-base-mode-hook 'pet-mode -10))

;;; PROLOG

(use-package prolog-mode
  :commands prolog-mode
  :bind*
  (:map prolog-mode-map
        ("C-c RET" . nil)))

;;; MERCURY

(use-package mercury-mode
  :mode ("\\.m\\'" . mercury-mode))

;;; CC MODE

(use-package cc-mode
  :defer t)

;;; HASKELL

(use-package haskell-mode
  :straight t
  :mode ("\\.hs\\'" . haskell-mode))

(use-package ghcid
  :straight (ghcid :type git :host github :repo "NicholasBHubbard/ghcid.el")
  :commands (ghcid-start ghcid-pop-to-buffer-or-start))

;; (use-package lsp-haskell
;;   :straight t
;;   :after haskell-mode
;;   :hook
;;   (haskell-mode . lsp))

;;; GO

(use-package go-mode
  :straight t
  :mode ("\\.go\\'" . go-mode))

;;; ZIG

(use-package zig-mode
  :straight t
  :mode ("\\.\\(?:zig\\|zon\\)\\'" . zig-mode))

;;; RUST

(use-package rust-mode
  :straight t
  :mode ("\\.rs\\'" . rust-mode))

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

;;; PLLISP

(use-package pllisp-mode
  :straight (pllisp-mode
             :type git
             :host github
             :repo "NicholasBHubbard/pllisp"
             :files ("extras/pllisp-mode.el"))
  :mode (("\\.pll\\'" . pllisp-mode)
         ("\\.pllisp\\'" . pllisp-mode))
  :hook
  (pllisp-mode-hook . rainbow-delimiters-mode))

;;; OCAML

(use-package tuareg
  :straight t
  :mode ("\\.mli?\\'" . tuareg-mode)
  :hook
  (tuareg-mode-hook . rainbow-delimiters-mode)
  (tuareg-mode-hook . (lambda ()
                        (setq-local comment-style 'multi-line)
                        (setq-local comment-continue "   "))))

(use-package merlin
  :straight t
  :after tuareg
  :hook
  (tuareg-mode-hook . merlin-mode))

;;; IY GO TO CHAR

(use-package iy-go-to-char
  :straight t
  :custom
  (iy-go-to-char-stop-position 'include)
  :bind*
  ("C-c \'" . iy-go-up-to-char)
  ("C-c \"" . iy-go-to-char-backward))

;;; COMINT

(use-package comint
  :commands comint-mode
  :custom
  (comint-pager "cat")
  :bind
  (:map comint-mode-map
        ([S-return] . (lambda () (interactive)
                        (comint-clear-buffer)
                        (comint-send-input)))))

;;; COMINT HISTORIES

(use-package comint-histories
  :straight t
  ;; :straight (comint-histories :type git :host github :repo "NicholasBHubbard/comint-histories" :branch "reselect-after")
  :after comint
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
    :reselect-after t
    :no-dups t)

  (comint-histories-add-history pdb
    :predicates '((lambda () (string-match-p "^(Pdb)" (comint-histories-get-prompt))))
    :length 2000
    :reselect-after t
    :no-dups t)

  (comint-histories-add-history rdbg
    :predicates '((lambda () (let ((prompt (comint-histories-get-prompt)))
                               (or (string-match-p "^\\[[0-9]+\\] pry(" prompt)
                                   (string-match-p "^(rdbg)" prompt)))))
    :length 2000
    :reselect-after t
    :no-dups t)

  (comint-histories-add-history python
    :predicates '((lambda () (or (derived-mode-p 'inferior-python-mode)
                                 (string-match-p "^>>>" (comint-histories-get-prompt)))))
    :length 2000
    :reselect-after t
    :no-dups t)

  (comint-histories-add-history prolog
    :predicates '((lambda () (derived-mode-p 'prolog-inferior-mode)))
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

  (comint-histories-add-history bashdb
    :predicates '((lambda () (string-match-p "^bashdb" (comint-histories-get-prompt))))
    :length 2000
    :reselect-after t
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
    :reselect-after t
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
  :straight t
  :after cc-mode
  :custom
  (cscope-display-cscope-buffer nil)
  :config
  (cscope-setup))

;;; SMTPMAIL

(use-package smtpmail
  :commands message-send-and-exit
  :custom
  ;; (send-mail-function 'smtpmail-send-it)
  (smtpmail-smtp-user nil)
  (smtpmail-default-smtp-server "posteo.de")
  (smtpmail-smtp-server "posteo.de")
  (smtpmail-smtp-service 587)
  (smtpmail-stream-type 'starttls))

;;; ECOMPLETE

(use-package ecomplete
  :after (:any message gnus))

;;; MESSAGE

(use-package message
  :commands (compose-mail message-send)
  :custom
  (message-mail-alias-type 'ecomplete)
  (message-self-insert-commands nil)
  (message-expand-name-standard-ui t)
  (message-kill-buffer-on-exit t)
  (message-send-mail-function #'message-smtpmail-send-it)
  (message-server-alist
   '(("nicholashubbard@posteo.net" . "smtp posteo.de 587 nicholashubbard@posteo.net")
     ("nhubbard@redhat.com" . "smtp smtp.gmail.com 587 nhubbard@redhat.com")))
  (mml-secure-openpgp-signers '("508022AE06C2C446D8072447C700A066BB25F148"))
  (message-signature
   (let ((nl (propertize "\n" 'hard t)))
     (concat "Nicholas B. Hubbard" nl
             "Keys: https://github.com/NicholasBHubbard/public-keys" nl
             "Key ID: 508022AE06C2C446D8072447C700A066BB25F148")))
  :hook
  (message-send-hook . mml-secure-message-sign-pgpmime)
  (message-sent-hook . message-put-addresses-in-ecomplete))

;;; MESSAGES ARE FLOWING

(use-package messages-are-flowing
  :straight t
  :after message
  :hook
  (message-mode-hook . messages-are-flowing-use-and-mark-hard-newlines))

;;; GNUS

(use-package gnus
  :commands gnus
  :hook
  (gnus-summary-exit-hook . (lambda ()
                              (dolist (buf (match-buffers '(derived-mode . gnus-article-mode)))
                                (when-let ((win (get-buffer-window buf)))
                                  (delete-window win)))))
  (gnus-started-hook . gnus-group-list-all-groups)
  :bind
  (:map gnus-article-mode-map
        ("q" . gnus-summary-expand-window))
  :custom
  (gnus-group-buffer "*gnus*")
  (gnus-select-method '(nnnil nil))
  (gnus-startup-file (concat user-emacs-directory ".newsrc"))
  (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
  (gnus-use-full-window nil)
  (gnus-buttonized-mime-types '("multipart/signed" "multipart/encrypted"))
  (gnus-auto-select-next nil)
  (gnus-search-use-parsed-queries t)
  (gnus-use-trees nil)
  (mm-verify-option 'known)
  (mm-decrypt-option 'known)
  (gnus-always-read-dribble-file nil)
  (gnus-parameters '((".*" (display . all))))
  (gnus-use-scoring nil)
  (gnus-summary-next-group-on-exit nil)
  (gnus-posting-styles '(("^nnimap\\+nhubbard@redhat\\.com:" (address "nhubbard@redhat.com"))))
  (gnus-secondary-select-methods
   '((nnimap "nicholashubbard@posteo.net"
             (nnimap-user "nicholashubbard@posteo.net")
             (nnimap-address "posteo.de")
             (nnimap-inbox ("INBOX" "Sent"))
             (nnimap-server-port "993")
             (nnimap-stream ssl)
             (nnmail-split-fancy-match-partial-words t)
             (nnimap-split-methods 'nnimap-split-fancy)
             (nnimap-split-fancy
              (| ("subject" "^Bounce probe.*" "INBOX.junk")
                 (from "MAILER-DAEMON@[^ ]+\\.posteo\\.de" "INBOX.send-confirmation")
                 (list "perl5-porters@perl\\.org" "INBOX.ml.p5p")
                 (list "linux-btrfs@vger\\.kernel\\.org" "INBOX.ml.btrfs")
                 (list "linux-bcachefs@vger\\.kernel\\.org" "INBOX.ml.bcachefs")
                 (list "linux-unionfs@vger\\.kernel\\.org" "INBOX.ml.overlayfs")
                 (list "linux-crypto@vger\\.kernel\\.org" "INBOX.ml.linux-crypto")
                 "INBOX")))
     (nnimap "nhubbard@redhat.com"
             (nnimap-user "nhubbard@redhat.com")
             (nnimap-address "imap.gmail.com")
             (nnimap-server-port "993")
             (nnimap-stream ssl)
             (nnimap-authenticator xoauth2)
             (nnimap-inbox ("INBOX" "[Gmail]/Sent Mail"))))))

;;; GPTEL

;; (use-package gptel
;;   :straight t
;;   :bind*
;;   ("C-c RET" . gptel-send)
;;   :custom
;;   (gptel-default-mode 'markdown-mode)
;;   (gptel-api-key #'(lambda () (password-store-get "openai-api-key")))
;;   :config
;;   (gptel-make-anthropic "Claude"
;;     :stream t
;;     :key #'(lambda () (password-store-get "anthropic-api-key"))))

;;; CLAUDE CODE

;; (use-package claude-code
;;   :straight t
;;   :bind*
;;   ("C-c c" . claude-code-transient)
;;   :custom
;;   (claude-code-terminal-backend 'eat))

;;; AGENT SHELL

(use-package agent-shell
  :straight t
  :commands (agent-shell-openai-start-codex agent-shell-anthropic-start-claude-code)
  :bind*
  ("C-c c" . agent-shell-help-menu)
  (:map agent-shell-mode-map ("C-q" . bury-buffer))
  :config
  (exec-path-from-shell-copy-envs
   '("CLAUDE_CODE_EFFORT_LEVEL"
     "CLAUDE_CODE_MAX_OUTPUT_TOKENS"
     "CLOUD_ML_REGION"
     "CLAUDE_CODE_USE_VERTEX"
     "ANTHROPIC_VERTEX_PROJECT_ID"))
  :hook
  (agent-shell-mode . (lambda ()
                        (setq-local comint-move-point-for-output 'all)
                        (setq-local comint-scroll-show-maximum-output t)
                        (setq-local window-point-insertion-type t)))
  :custom
  (agent-shell-preferred-agent-config (agent-shell-openai-make-codex-config))
  (agent-shell-openai-default-session-mode-id "agent-full-access")
  (agent-shell-anthropic-default-model-id "opus")
  (agent-shell-header-style 'text)
  (agent-shell-session-restore-verbosity 'minimal)
  (agent-shell-show-config-icons nil)
  (agent-shell-dot-subdir-function
   (lambda (subdir)
     (let ((project-path (agent-shell-cwd)))
       (expand-file-name (file-name-concat ".agent-shell" project-path subdir)
                         user-emacs-directory)))))

;;; YAML

(use-package yaml-mode
  :straight t
  :mode ("\\.ya?ml\\'" . yaml-mode))

;;; JINJA

(use-package jinja2-mode
  :straight t
  :commands jinja2-mode
  :bind
  (:map jinja2-mode-map
        ("M-o" . nil)))

;;; SGML

(use-package sgml-mode
  :commands sgml-mode
  :bind
  (:map sgml-mode-map
        ("M-o" . nil)
        :map html-mode-map
        ("M-o" . nil)))

;;; DIFF

(use-package diff
  :commands (diff diff-mode)
  :bind
  (:map diff-mode-map
        ("M-o" . nil)))

;;; PDF TOOLS

(use-package pdf-tools
  :straight t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install t)
  (pdf-loader-install t)
  :hook
  (pdf-view-mode-hook . pdf-view-midnight-minor-mode)
  (TeX-after-compilation-finished-functions . TeX-revert-document-buffer))

(use-package pdf-view-restore
  :straight t
  :after pdf-tools
  :hook
  (pdf-view-mode-hook . pdf-view-restore-mode)
  :custom
  (pdf-view-restore-filename (concat user-emacs-directory ".pdf-view-restore")))

;;; DOCKER

(use-package docker
  :straight t
  :commands docker
  :bind*
  ("C-c d" . docker)
  :custom
  (docker-terminal-backend 'shell)
  (docker-image-default-sort-key '("Repository" . "Ascending")))

(use-package dockerfile-mode
  :straight t
  :defer t)

;;; MARKDOWN

(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'" . markdown-mode))

;;; JIRA

(use-package jira
  :straight t
  :commands (jira-issues jira-issues-menu)
  :custom
  (jira-base-url "https://redhat.atlassian.net")
  (jira-username "nhubbard@redhat.com")
  (jira-token (password-store-get "jira-token"))
  (jira-token-is-personal-access-token nil)
  (jira-api-version 2))

;;; AUTOTEST

(use-package autotest-mode
  :mode ("\\.at\\'" . autotest-mode))

;;; MIDNIGHT

(use-package midnight
  :hook (emacs-startup-hook . midnight-mode)
  :custom
  (midnight-delay 16200) ; 4:30AM
  (clean-buffer-list-kill-regexps
   '("\\`\\*Man " "\\`\\*helpful " "\\`magit-process: " "\\`magit-diff: ")))

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
  (display-line-numbers-widen t)
  (undo-in-region t)
  (inhibit-startup-screen t)
  (mode-line-percent-position nil)
  (auto-revert-verbose nil)
  (display-buffer-base-action '(display-buffer-same-window))
  :config
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
  (fset 'yes-or-no-p 'y-or-n-p)
  :hook
  (prog-mode . display-line-numbers-mode)
  (text-mode . display-line-numbers-mode)
  (prog-mode . display-fill-column-indicator-mode)
  (text-mode . display-fill-column-indicator-mode)
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

;;; BUFFERS WITH MODE

(use-package buffers-with-mode
  :straight (buffers-with-mode :type git :host github :repo "NicholasBHubbard/buffers-with-mode")
  :commands buffers-with-mode)

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

;;; YASNIPPET

;; (use-package yasnippet
;;   :straight t
;;   :blackout yas-minor-mode
;;   :config
;;   (use-package yasnippet-snippets :straight t)
;;   (yas-global-mode 1))

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
  :bind*
  ("M-[" . winner-undo)
  ("M-]" . winner-redo))

;;; ACE WINDOW

(use-package ace-window
  :straight t
  :bind*
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
  (recentf-auto-cleanup 'never)
  (recentf-max-saved-items 500)
  (recentf-exclude '("^/tmp/" "^/ssh:" "^/sudo:" "/elpa/" "COMMIT_EDITMSG" ".*-autoloads\\.el$" file-remote-p))
  :bind*
  ("C-c f" . recentf))

;;; FLYMAKE

(use-package flymake
  :commands (flymake-mode flymake-start)
  :custom
  (flymake-no-changes-timeout nil))

;;; EGLOT

(use-package eglot
  :commands (eglot eglot-ensure)
  :custom
  (eglot-send-changes-idle-time 99999)
  (eglot-autoshutdown t)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  :bind
  (:map eglot-mode-map
        ("C-c e" . eglot-hydra/body))
  :pretty-hydra
  ((:color teal :quit-key "C-c e")
   ("Flymake"
    (("n" flymake-goto-next-error "next error" :exit nil)
     ("p" flymake-goto-prev-error "previous error" :exit nil)
     ("db" flymake-show-buffer-diagnostics "diagnostics buffer")
     ("dp" flymake-show-project-diagnostics "diagnostics project"))
    "Find"
    (("fd" eglot-find-declaration "declaration")
     ("ft" eglot-find-typeDefinition "type def")
     ("fi" eglot-find-implementation "implementation"))
    "Format"
    (("Fr" eglot-format "region")
     ("Fb" eglot-format-buffer "buffer"))
    "Server"
    (("ss" eglot-shutdown "shutdown")
     ("sS" eglot-shutdown-all "shutdown all")
     ("sa" eglot-code-actions "code actions")))))

;;; ORG

(use-package org
  :ensure nil
  :bind* ("C-c c" . org-capture)
  :hook
  (org-mode . visual-line-mode)
  :custom
  (org-directory (concat user-emacs-directory "org/"))
  (org-default-notes-file (concat org-directory "brain.org"))

  (org-capture-templates
   '(("t" "TODO" entry (file org-default-notes-file)
      "* TODO: %?\n  [%a]\n  %U" :prepend t :empty-lines-after 2)

     ("r" "Remember" entry (file org-default-notes-file)
      "* NOTE: %?\n  [%a]\n  %U" :prepend t :empty-lines-after 2)))

  :config
  (setopt
   display-buffer-alist
   (cons '("\\*Org Select\\*" (display-buffer-below-selected))
         display-buffer-alist))
  (setopt
   display-buffer-alist
   (cons '("CAPTURE*" (display-buffer-below-selected))
         display-buffer-alist))


  (unless (file-directory-p org-directory)
    (make-directory org-directory)))

;;; WHICH FUNCTION

(use-package which-func
  :commands which-function-mode
  :hook
  (prog-mode . (lambda () (which-function-mode 1))))

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
  (consult-buffer-sources '(consult--source-buffer consult--source-recent-file))
  (consult-preview-key 'any)
  :bind*
  ("M-g g"  . consult-goto-line)
  ("M-o"   . (lambda () (interactive)
               (setq consult-preview-key "M-SPC")
               (unwind-protect
                   (call-interactively #'consult-buffer)
                 (setq consult-preview-key 'any))))
  ("C-z"   . consult-global-mark)
  ("C-S-y" . consult-yank-from-kill-ring)
  ("C-c f" . consult-find)
  ("C-c s" . consult-grep))

;;; CONSULT DIR

(use-package consult-dir
  :straight t
  :after consult
  :bind*
  ("M-O" . consult-dir)
  :custom
  (consult-dir-project-list-function #'consult-dir-project-dirs)
  (consult-dir-sources
   '(consult-dir--source-default
     consult-dir--source-project
     consult-dir--source-recentf
     consult-dir--source-tramp-local
     consult-dir--source-tramp-ssh)))

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
  (corfu-auto-delay 0)
  (corfu-auto-prefix 3)
  (corfu-cycle t)
  (corfu-on-exact-match nil)
  (corfu-preselect 'prompt)
  (corfu-preview-current nil)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t))

;;; CAPE

(use-package cape
  :straight t
  :bind
  ("M-<tab>" . cape-prefix-map)
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;;; DABBREV

(use-package dabbrev
  :defer t
  :custom
  (dabbrev-case-fold-search nil))

;;; PROJECT

(use-package project
  :defer t
  :custom
  (project-list-file (expand-file-name ".projects" user-emacs-directory))
  (project-switch-commands 'project-dired))

(use-package disproject
  :straight t
  :custom
  (disproject-shell-command #'project-shell)
  :bind*
  ("C-c p" . disproject-dispatch))

;;; AGGRESSIVE INDENT

(use-package aggressive-indent
  :straight t
  :blackout
  :commands aggressive-indent-mode)

;;; YANK INDENT

(use-package yank-indent
  :straight (:host github :repo "jimeh/yank-indent")
  :blackout
  :hook
  (prog-mode . yank-indent-mode))

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
  (compilation-filter . (lambda ()
                          (let ((inhibit-read-only t))
                            (ansi-color-apply-on-region (point-min) (point-max))))))

;;; SHELL

(use-package shell
  :commands (shell shell-mode)
  :custom
  (shell-file-name "/bin/bash")
  (shell-kill-buffer-on-exit t)
  :hook
  (shell-mode . (lambda () ; compatible with shell prompt: PS1='[\u@\h \w]\$ '
                  (shell-dirtrack-mode 0)
	              (setq-local dirtrack-list '("[[][^@]*@[^ ]* \\([^]]*\\)\\][$#] " 1))
                  (shell-dirtrack-mode 1))))

;;; BASH COMPLETION

(use-package bash-completion
  :straight t
  :after shell
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
  (shell-mode . shx-mode))

;;; SH SCRIPT

(use-package sh-script
  :commands sh-mode
  :custom
  (sh-indentation 4)
  (sh-basic-offset 4))

(use-package flymake-shellcheck
  :straight t
  :after sh-script
  :hook
  (sh-mode . flymake-shellcheck-load))

;;; SUDO EDIT

(use-package sudo-edit
  :straight t
  :commands (sudo-edit sudo-edit-find-file)
  :config
  (sudo-edit-indicator-mode 1))

;;; AVY

(use-package avy
  :straight t
  :bind*
  ("C-'" . avy-goto-char-timer)
  :custom
  (avy-timeout-seconds 0.4)
  (avy-single-candidate-jump t)
  (avy-style 'at-full)
  (avy-case-fold-search nil))

;;; EXPAND REGION

(use-package expand-region
  :straight t
  :custom
  (expand-region-smart-cursor t)
  (expand-region-skip-whitespace nil)
  (expand-region-subword-enabled t)
  :bind*
  ("C-=" . er/expand-region)
  ("C-+" . er/contract-region))

;;; MULTIPLE CURSORS

(use-package multiple-cursors
  :straight t
  :custom
  (mc/always-run-for-all t)
  :bind*
  ("C-c m" . mc/edit-lines)
  (:map mc/keymap
        ("RET" . nil)
        ("C->" . mc/mark-next-like-this)
        ("C-<" . mc/mark-previous-like-this)
        ("C-c >" . mc/mark-all-like-this)))

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
  (eldoc-idle-delay 0.5)
  (eldoc-echo-area-use-multiline-p nil))

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
  (erc-hide-list '("JOIN" "PART" "QUIT" "AWAY" "NICK" "353" "366"))
  (erc-kill-buffer-on-part t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-server-auto-reconnect t)
  (erc-track-position-in-mode-line nil)
  (erc-max-buffer-size 30000)
  (erc-user-full-name erc-nick)
  (erc-email-userid erc-nick)
  (erc-log-channels-directory "~/.irc-logs")
  (erc-generate-log-file-name-function #'erc-generate-log-file-name-network)
  (erc-modules '(autojoin button completion fill imenu irccontrols list match menu move-to-prompt netsplit networks readonly ring stamp track log))
  :bind
  (:map erc-mode-map
        ("C-q" . bury-buffer))
  :hook
  (erc-after-connect . (lambda () (erc-send-line "ZNC *playback play * 0" #'ignore)))
  :config
  (remove-hook 'erc-kill-channel-hook #'erc-part-channel-on-kill)
  :init
  (defun my/hetzner-ssh-znc-tunnel (&optional arg)
    (let ((tunnel-process (get-process "hetzner-ssh-znc-tunnel")))
      (when (and tunnel-process
                 (or arg (not (process-live-p tunnel-process))))
        (message "Deleting existing SSH tunnel...")
        (delete-process tunnel-process)
        (setq tunnel-process nil)
        (sleep-for 2))
      (when (or (not tunnel-process) (not (process-live-p tunnel-process)))
        (message "Starting SSH tunnel...")
        (make-process :name "hetzner-ssh-znc-tunnel"
                      :command '("ssh" "-L" "6697:localhost:6697" "-n" "-N"
                                 "-o" "ServerAliveInterval=60"
                                 "-o" "ServerAliveCountMax=3"
                                 "hetzner-debian-vps")
                      :buffer " *hetzner-ssh-znc-tunnel*"
                      :connection-type 'pty
                      :sentinel #'(lambda (_ msg)
                                    (when (string-match "exited abnormally" msg)
                                      (kill-buffer " *hetzner-ssh-znc-tunnel*"))))
        (sleep-for 3)
        (with-current-buffer " *hetzner-ssh-znc-tunnel*"
          (add-hook
           'kill-buffer-hook
           #'(lambda () (dolist (buf (buffers-with-mode 'erc-mode)) (kill-buffer buf))) nil t)))))

  (defun my/erc (&optional arg)
    (interactive "P")
    (my/hetzner-ssh-znc-tunnel arg)
    (erc :server "localhost"
         :port 6697
         :id "*znc-libera-server*"
         :nick erc-nick
         :password (concat "admin@erc/libera:" (password-store-get "znc-admin")))
    (erc :server "localhost"
         :port 6697
         :id "*znc-perl-server*"
         :nick erc-nick
         :password (concat "admin@erc/irc-perl:" (password-store-get "znc-admin"))))

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
  :straight (ws-butlet :type git :host github :repo "lewang/ws-butler")
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
  :custom
  (magit-clone-default-directory "~/p/")
  (git-commit-major-mode 'git-commit-elisp-text-mode)
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
  (mode-line-format (delete '(vc-mode vc-mode) mode-line-format))
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
  :config
  (diff-hl-margin-mode 1)
  (diff-hl-flydiff-mode 0)
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)
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
  :init
  (setq tramp-default-method "ssh")
  (setq tramp-default-remote-shell "/bin/bash")
  (setq tramp-connection-local-default-shell-variables
        '((shell-file-name . "/bin/bash")
          (shell-command-switch . "-c")))
  (setq tramp-ssh-controlmaster-options
        (concat "-o ControlPath=\~/.ssh/ssh-connection:%%r@%%h:%%p "
                "-o ControlMaster=auto -o ControlPersist=yes")))

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
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-clean-confirm-killing-deleted-buffers nil)
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
  (Man-prefer-synchronous-call t)
  :bind*
  ("C-h m" . man))

;;; GOOGLE THIS

(use-package google-this
  :straight t
  :commands (google-this google-this-search)
  :bind*
  ("C-c G" . google-this))

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
  :hook
  (perl-mode  . cperl-mode)
  (cperl-mode . flymake-mode)
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

(use-package pet
  :straight t
  :blackout
  :after python
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package flymake-ruff
  :straight t
  :after python
  :hook
  (python-mode . flymake-ruff-load)
  (python-mode . flymake-mode-on))

;;; PROLOG

(use-package prolog-mode
  :commands prolog-mode
  :bind
  (:map prolog-mode-map
        ("C-c RET" . nil)))

;;; MERCURY

(use-package mercury-mode
  :mode ("\\.m\\'" . mercury-mode))

;;; CC MODE

(use-package cc-mode
  :defer t
  :after eglot
  :config
  (add-to-list 'eglot-server-programs '((c-mode c++-mode) . ("clangd" "--background-index" "--clang-tidy"))))

;;; HASKELL

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :straight t)

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
  :hook
  (comint-mode . (lambda () (setq-local comint-process-echoes t)))
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
  (smtpmail-smtp-user user-mail-address)
  (smtpmail-default-smtp-server "posteo.de")
  (smtpmail-smtp-server "posteo.de")
  (smtpmail-smtp-service 587)
  (smtpmail-stream-type 'starttls))

;;; ECOMPLETE

(use-package ecomplete
  :after (:any message gnus))

;;; MESSAGE

(use-package message
  :commands compose-mail
  :after gnus
  :commands (compose-mail message-send message-send-and-exit)
  :custom
  (message-mail-alias-type 'ecomplete)
  (message-self-insert-commands nil)
  (message-expand-name-standard-ui t)
  (message-kill-buffer-on-exit t)
  (message-send-mail-function #'message-smtpmail-send-it)
  (mml-secure-openpgp-signers '("508022AE06C2C446D8072447C700A066BB25F148"))
  (message-signature "Nicholas B. Hubbard
Keys: https://github.com/NicholasBHubbard/public-keys
Key ID: 508022AE06C2C446D8072447C700A066BB25F148")
  :hook
  (message-send . mml-secure-message-sign-pgpmime)
  (message-sent . message-put-addresses-in-ecomplete))

;;; MESSAGES ARE FLOWING

(use-package messages-are-flowing
  :straight t
  :after message
  :hook
  (message-mode . messages-are-flowing-use-and-mark-hard-newlines))

;;; GNUS

(use-package gnus
  :defer t
  :commands gnus
  :hook
  (gnus-summary-exit . (lambda () (dolist (buf (buffers-with-mode 'gnus-article-mode))
                                    (when-let ((win (get-buffer-window buf)))
                                      (delete-window win)))))
  (gnus-started . gnus-group-list-all-groups)
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
  (gnus-use-trees nil)
  (mm-verify-option 'known)
  (mm-decrypt-option 'known)
  (gnus-always-read-dribble-file nil)
  (gnus-summary-next-group-on-exit nil)
  (gnus-secondary-select-methods
   '((nnimap "nicholashubbard@posteo.net"
             (nnimap-user "nicholashubbard@posteo.net")
             (nnimap-address "posteo.de")
             (nnimap-inbox ("INBOX" "Sent"))
             (nnimap-server-port "993")
             (nnimap-stream ssl)
             (nnimap-split-methods
              (("INBOX.junk" "^Subject: Bounce probe.*")
               ("INBOX.send-confirmation" "^From: MAILER-DAEMON@[^ ]+\\.posteo.de")
               ("INBOX.ml.p5p" "^\\(To\\|Cc\\):.*perl5-porters@perl\\.org")
               ("INBOX.ml.btrfs" "^\\(To\\|Cc\\):.*linux-btrfs@vger\\.kernel\\.org")
               ("INBOX.ml.bcachefs" "^\\(To\\|Cc\\):.*linux-bcachefs@vger\\.kernel\\.org")
               ("INBOX.ml.overlayfs" "^\\(To\\|Cc\\):.*linux-unionfs@vger\\.kernel\\.org")
               ("INBOX.ml.linux-crypto" "^\\(To\\|Cc\\):.*linux-crypto@vger\\.kernel\\.org")
               ("INBOX" "")))))))

;;; GPTEL

(use-package gptel
  :straight t
  :bind*
  ("C-c RET" . gptel-send)
  :custom
  (gptel-default-mode 'markdown-mode)
  (gptel-api-key #'(lambda () (password-store-get "openai-api-key")))
  :config
  (gptel-make-anthropic "Claude" :key #'(lambda () (password-store-get "anthropic-api-key"))))

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
  (pdf-view-mode . pdf-view-midnight-minor-mode)
  (TeX-after-compilation-finished-functions . TeX-revert-document-buffer))

(use-package pdf-view-restore
  :straight t
  :after pdf-tools
  :hook
  (pdf-view-mode . pdf-view-restore-mode)
  :custom
  (pdf-view-restore-filename (concat user-emacs-directory ".pdf-view-restore")))

;;; ALERT

(use-package alert
  :straight t
  :commands alert
  :init
  (setq alert-default-style 'notifier))

;;; DOCKER

(use-package docker
  :straight t
  :commands docker
  :bind*
  ("C-c d" . docker))

(use-package dockerfile-mode
  :straight t
  :magic ("Dockerfile" . dockerfile-mode))

;;; MARKDOWN

(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'" . markdown-mode))

;;; JIRA

(use-package jira
  :straight t
  :commands (jira-issues jira-issues-menu)
  :custom
  (jira-base-url "https://issues.redhat.com")
  (jira-username "nhubbard@redhat.com")
  (jira-token (password-store-get "jira-token"))
  (jira-token-is-personal-access-token t)
  (jira-api-version 2))

;;; AUTOTEST

(use-package autotest-mode
  :mode ("\\.at\\'" . autotest-mode))

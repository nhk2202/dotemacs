;;; -*- lexical-binding: t -*-

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)

(column-number-mode 1)

(setq user-full-name "Hải Khánh"
      user-mail-address "haikhanh220204@gmail.com")
(if (file-exists-p "~/.authinfo.gpg")
    (setq auth-sources '("~/.authinfo.gpg")))

(setq custom-file (concat data-dir "custom.el"))
(load custom-file t)

(require 'package)
(setq package-user-dir (concat data-dir "packages/")
      package-gnupghome-dir (concat package-user-dir "gnupg/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq use-package-enable-imenu-support t)
(require 'use-package)
(setq use-package-always-ensure t
      use-package-expand-minimally t)

(setq make-backup-files nil)

(setq auto-save-list-file-prefix (concat data-dir "auto-save-list/.saves-")
      kill-buffer-delete-auto-save-files t)

(setq global-auto-revert-non-file-buffers t
      auto-revert-check-vc-info t)
(global-auto-revert-mode 1)

(setq recentf-save-file (concat data-dir "recentf"))
(recentf-mode 1)

(setq savehist-file (concat data-dir "history"))
(savehist-mode 1)

(with-eval-after-load 'project
  (setq project-list-file (concat data-dir "projects")
	    project-kill-buffers-display-buffer-list t)
  (dolist (useless-project-switch-commands '((project-vc-dir "VC-Dir")
                                             (project-find-regexp "Find regexp")))
    (delete useless-project-switch-commands project-switch-commands)))

(pixel-scroll-precision-mode t)

(setq initial-scratch-message nil
      initial-major-mode 'fundamental-mode)

(setq default-input-method "vietnamese-telex")

(setq x-underline-at-descent-line t)

(setq-default truncate-lines t)

(setq switch-to-buffer-obey-display-actions t)

(setq scroll-margin 10)

(setq use-short-answers t)

(setq visible-bell t)

(setq whitespace-style '(face
                         trailing
                         missing-newline-at-eof))
(setq-default indicate-empty-lines t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq comment-multi-line t)

(setq sentence-end-double-space nil)

(global-subword-mode 1)

(setq delete-active-region 'kill)

(setq electric-quote-replace-consecutive nil)
(electric-pair-mode 1)

(setq show-paren-style 'mixed
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t
      show-paren-context-when-offscreen 'overlay)

(setq kill-whole-line t
      kill-do-not-save-duplicates t)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq open-paren-in-column-0-is-defun-start nil)

(setq imenu-auto-rescan t)

(setq xref-auto-jump-to-first-xref t
      xref-auto-jump-to-first-definition t)

(setq enable-recursive-minibuffers t
      resize-mini-windows t
      history-delete-duplicates t)
(minibuffer-electric-default-mode 1)
(minibuffer-depth-indicate-mode 1)

(setq read-extended-command-predicate 'command-completion-default-include-p)

(setq ibuffer-expert t)

(setq compilation-scroll-output 'first-error
      compilation-auto-jump-to-first-error 'first-known)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "j") 'ibuffer-forward-line)
  (define-key ibuffer-mode-map (kbd "k") 'ibuffer-backward-line))

(setq dired-switches-in-mode-line 'as-is)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "j") 'dired-next-line)
  (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  (define-key dired-mode-map (kbd "/") 'dired-goto-file))

(setq-default fill-column 100)
(add-hook 'text-mode-hook (lambda ()
                            (auto-fill-mode 1)))

(setq bookmark-save-flag t
      bookmark-sort-flag 'last-modified
      bookmark-default-file (concat data-dir "bookmarks"))

(setq abbrev-file-name (concat user-emacs-directory "abbrevs"))

(setq completion-auto-help nil)

(setq help-window-keep-selected t
      help-enable-variable-value-editing t)

(setq disabled-command-function nil)

(with-eval-after-load 'make-mode
  (add-hook 'makefile-gmake-mode (lambda ()
                                   (setq-local indent-tabs-mode t))))

(with-eval-after-load 'c-ts-mode
  (setq c-ts-mode-indent-style 'linux
        c-ts-common-indent-offset 4
        c-ts-indent-offset 4)
  (add-hook 'c-ts-base-mode-hook (lambda ()
                                   (setq-local indent-tabs-mode t))))

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(with-eval-after-load 'octave-mode
  (setq octave-block-offset 4))

(use-package diminish
  :hook
  (prog-mode . (lambda ()
                 (whitespace-mode 1)
                 (diminish 'whitespace-mode)))
  :config
  (diminish 'global-auto-revert-mode)
  (diminish 'abbrev-mode)
  (diminish 'auto-fill-mode)
  (diminish 'eldoc-mode)
  (diminish 'subword-mode))

(use-package gcmh
  :diminish
  :custom
  (gcmh-verbose t)
  :config
  (gcmh-mode 1))

(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  (global-treesit-auto-mode 1))

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-syntax '(yellow-comments
                            green-strings))

(use-package auto-dark
  :diminish
  :custom
  (auto-dark-dark-theme 'modus-vivendi)
  (auto-dark-light-theme 'modus-operandi)
  (auto-dark-polling-interval-seconds 60)
  :config
  (auto-dark-mode 1))

(use-package pulsar
  :custom
  (pulsar-pulse t)
  :config
  (pulsar-global-mode 1))

(use-package page-break-lines
  :diminish
  :hook (font-lock-mode . page-break-lines-mode))

(use-package disable-mouse
  :init
  (setq disable-mouse-wheel-events nil)
  :config
  (global-disable-mouse-mode 1))

(use-package puni
  :config
  (puni-global-mode 1))

(use-package vundo
  :commands (vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t)
  :bind
  (:map vundo-mode-map
        ("j" . vundo-next)
        ("k" . vundo-previous)
        ("h" . vundo-backward)
        ("l" . vundo-forward)
        ("u" . vundo-goto-last-saved)
        ("s" . vundo-save)
        ("H" . vundo-stem-root)
        ("L" . vundo-stem-end)))

(use-package magit
  :commands (magit-status
             magit-dispatch)
  :custom
  (magit-delete-by-moving-to-trash nil)
  (magit-view-git-manual-method 'man))

(use-package transient
  :ensure nil
  :config
  (defconst transient-dir (concat data-dir "transient/"))
  (setq transient-history-file (concat transient-dir "history.el")))

(use-package forge
  :after magit)

(use-package org
  :ensure nil
  :custom
  (org-directory "~/Notes"))

(use-package helpful
  :commands (helpful-callable
             helpful-command
             helpful-symbol
             helpful-function
             helpful-macro
             helpful-variable
             helpful-at-point
             helpful-key))

(use-package vertico
  :demand t
  :custom
  (vertico-scroll-margin 3)
  (vertico-cycle t)
  :bind
  (:map vertico-map
        ("M-j" . vertico-next)
        ("M-k" . vertico-previous)
        ("M-J" . vertico-scroll-up)
        ("M-K" . vertico-scroll-down)
        ("M-g" . vertico-first)
        ("M-G" . vertico-last)
        ("<return>" . vertico-directory-enter)
        ("<backspace>" . vertico-directory-delete-char)
        ("/" . my/vertico-insert))
  (:map vertico-multiform-map
        ("M-G" . nil))
  :init
  (defun my/vertico-insert ()
    (interactive)
    (let* ((mb (minibuffer-contents-no-properties))
           (lc (if (string= mb "") mb (substring mb -1))))
      (cond ((string-match-p "^[/~:]" lc) (self-insert-command 1 ?/))
            ((file-directory-p (vertico--candidate)) (vertico-insert))
            (t (self-insert-command 1 ?/)))))
  (defadvice vertico-insert
      (after vertico-insert-add-history activate)
    (unless (eq minibuffer-history-variable t)
      (add-to-history minibuffer-history-variable (minibuffer-contents))))
  :hook
  ((rfn-eshadow-update-overlay . vertico-directory-tidy)
   (minibuffer-setup . vertico-repeat-save))
  :config
  (vertico-mode 1)
  (vertico-multiform-mode 1)
  (vertico-mouse-mode 1)
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

(use-package corfu
  :demand t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  :bind
  (:map corfu-map
        ("M-j"             . corfu-next)
        ("M-k"             . corfu-previous)
        ("M-J"             . corfu-scroll-up)
        ("M-K"             . corfu-scroll-down)
        ("M-g"             . corfu-first)
        ("M-G"             . corfu-last)
        ("M-<backspace>"   . corfu-reset))
  :config
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package consult
  :demand t
  :after vertico
  :bind
  (:map consult-narrow-map
        ("M-?" . consult-narrow-help))
  :init
  (setq register-preview-delay 0.2
        register-preview-function 'consult-register-format)
  (setq xref-show-xrefs-function 'consult-xref
        xref-show-definitions-function 'consult-xref)
  (advice-add 'register-preview :override 'consult-register-window)
  :custom
  (consult-narrow-key "M--")
  (consult-widen-key "M-+")
  :config
  (with-eval-after-load 'consult-imenu
    (dolist (my/consult-imenu-configs
             '((c-ts-mode :toplevel "Function"
                          :types ((?f "Function" font-lock-function-name-face)
                                  (?v "Variable" font-lock-variable-name-face)
                                  (?d "Typedef" font-lock-type-face)
                                  (?s "Struct" font-lock-type-face)
                                  (?e "Enum" font-lock-type-face)))
               (c++-ts-mode :toplevel "Function"
                            :types ((?f "Function" font-lock-function-name-face)
                                    (?v "Variable" font-lock-variable-name-face)
                                    (?d "Typedef" font-lock-type-face)
                                    (?s "Struct" font-lock-type-face)
                                    (?e "Enum" font-lock-type-face)
                                    (?c "Class" font-lock-type-face)))))
      (add-to-list 'consult-imenu-config my/consult-imenu-configs)))
  (add-to-list 'vertico-multiform-commands
               '(consult-line flat)))

(use-package embark
  :commands (embark-act
             embark-export)
  :bind
  (:map minibuffer-local-map
        ("M-<return>" . embark-act)
        ("M-SPC" . embark-export))
  :hook
  (embark-after-export . (lambda ()
                           (other-window 1)
                           (next-error 1)))
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark
          consult))

(use-package citre
  :init
  (require 'citre-config)
  :custom
  (citre-default-create-tags-file-location 'global-cache)
  (citre-use-project-root-when-creating-tags t)
  (citre-prompt-language-for-ctags-command t)
  (citre-auto-enable-citre-mode-modes '(prog-mode)))

(use-package tempel
  :commands (tempel-complete
             tempel-insert)
  :bind
  (:map tempel-map
        ("M-[" . tempel-previous)
        ("M-]" . tempel-next)))

(use-package tempel-collection
  :after (tempel))

(use-package eglot
  :ensure nil
  :custom
  (eglot-autoshutdown t))

(use-package flymake
  :ensure nil)

(use-package disaster
  :commands (disaster))

;; (use-package cape
;;   :demand t
;;   :hook
;;   (emacs-lisp-mode . (lambda ()
;;                        (setq-local completion-at-point-functions
;;                                    (cons 'cape-elisp-symbol
;;                                          completion-at-point-functions))))
;;   (text-mode . (lambda ()
;;                  (setq-local completion-at-point-functions
;;                              (list 'cape-abbrev
;;                                    (cape-capf-super 'cape-dabbrev
;;                                                     'cape-dict)
;;                                    'tempel-complete
;;                                    t)))))

(use-package orderless
  :after (vertico
          corfu
          consult)
  :custom
  (orderless-component-separator 'orderless-escapable-split-on-space)
  :config
  (setq completion-styles '(basic orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :after (vertico
          corfu)
  :demand t
  :bind
  (:map minibuffer-local-map
        ("M-TAB" . marginalia-cycle))
  :config
  (marginalia-mode 1))

(use-package which-key
  :custom
  (which-key-lighter "")
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0.1)
  (which-key-popup-tyle 'minibuffer)
  (which-key-show-remaining-keys t)
  (which-key-show-prefix 'left)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  :config
  (which-key-mode 1))

(use-package multistate
  :demand t
  :init
  (multistate-define-state 'emacs :lighter "E")
  (multistate-define-state
   'motion
   :lighter "M"
   :parent 'multistate-suppress-map)
  (multistate-define-state
   'normal
   :default t
   :lighter "N"
   :parent 'multistate-motion-state-map)
  (multistate-define-state
   'insert
   :lighter "I"
   :cursor 'bar)
  (define-prefix-command 'more-commands)
  (define-prefix-command 'help-commands)
  (define-prefix-command 'window-commands)
  (define-prefix-command 'buffer-commands)
  (define-prefix-command 'project-commands)
  (define-prefix-command 'bookmark-commands)
  (define-prefix-command 'git-commands)
  (define-prefix-command 'register-commands)
  (define-prefix-command 'more-motion-commands)
  (define-prefix-command 'puni-wrap-commands)
  (define-prefix-command 'more-mark-commands)
  (define-prefix-command 'insert-commands)
  (define-prefix-command 'search-commands)
  (defun my/yank ()
    (interactive)
    (if (region-active-p)
        (puni-kill-active-region))
    (call-interactively 'consult-yank-from-kill-ring))
  (defun my/replace-command ()
    (interactive)
    (puni-forward-delete-char)
    (multistate-insert-state))
  (defun my/kill-whole-line ()
    (interactive)
    (beginning-of-line)
    (puni-kill-line))
  (defun my/newline ()
    (interactive)
    (end-of-line)
    (newline-and-indent)
    (multistate-insert-state))
  (defun my/newline-above ()
    (interactive)
    (end-of-line 0)
    (newline-and-indent)
    (multistate-insert-state))
  (defun my/break-line ()
    (interactive)
    (multistate-insert-state)
    (newline-and-indent))
  (defun my/comment-command ()
    (interactive)
    (if (region-active-p)
        (if (or (puni-region-balance-p (region-beginning)
                                       (region-end)
                                       t)
                (y-or-n-p "Region is unbalanced, really kill? "))
            (call-interactively 'comment-or-uncomment-region))
      (if (or (puni-region-balance-p (line-beginning-position)
                                     (line-end-position)
                                     t)
              (y-or-n-p "Region is unbalanced, really kill? "))
          (call-interactively 'comment-line))))
  (defun my/shell-command ()
    (interactive)
    (if (region-active-p)
        (call-interactively 'shell-command-on-region)
      (call-interactively 'async-shell-command)))
  (defun my/mark-line-command ()
    (interactive)
    (beginning-of-line 1)
    (set-mark (point))
    (beginning-of-line 2))
  (defun my/mark-word ()
    (interactive)
    (forward-word)
    (backward-word)
    (mark-word))
  (defun my/mark-defun ()
    (interactive)
    (if (member major-mode '(c-mode c++-mode java-mode awk-mode))
        (call-interactively 'c-mark-function)
      (call-interactively 'mark-defun)))
  (defun my/ispell ()
    (interactive)
    (if (region-active-p)
        (call-interactively 'ispell-region)
      (call-interactively 'ispell-word)))
  (defun my/citre-create-tags-file ()
    (interactive)
    (call-interactively 'citre-create-tags-file)
    (citre-mode 1))
  :bind
  (:map multistate-emacs-state-map
        ("SPC" . more-commands))
  (:map multistate-insert-state-map
        ("<escape>" . multistate-normal-state))
  (:map multistate-motion-state-map
        ("~"    . universal-argument)
        ("h"    . backward-char)
        ("j"    . next-line)
        ("k"    . previous-line)
        ("l"    . forward-char)
        ("H"    . backward-word)
        ("J"    . scroll-up-command)
        ("K"    . scroll-down-command)
        ("L"    . forward-word)
        ("b"    . puni-beginning-of-sexp)
        ("e"    . puni-end-of-sexp)
        ("["    . puni-syntactic-backward-punct)
        ("]"    . puni-syntactic-forward-punct)
        ("/"    . search-commands)
        ("m"    . set-mark-command)
        ("M"    . more-mark-commands)
        ("x"    . exchange-point-and-mark)
        ("+"    . puni-expand-region)
        ("n"    . next-error)
        ("p"    . previous-error)
        ("N"    . flymake-goto-next-error)
        ("P"    . flymake-goto-prev-error)
        ("."    . xref-find-definitions)
        (","    . xref-find-references)
        ("<"    . xref-go-back)
        (">"    . xref-go-forward)
        ("s"    . kill-ring-save)
        ("G"    . embark-act)
        ("!"    . my/shell-command)
        (":"    . execute-extended-command)
        (";"    . consult-complex-command)
        ("g"    . more-motion-commands)
        ("SPC"  . more-commands)
        ("C-\\" . ignore))
  (:map multistate-normal-state-map
        ("i"             . multistate-insert-state)
        ("I"             . insert-commands)
        ("u"             . undo)
        ("U"             . vundo)
        ("r"             . puni-raise)
        ("c"             . my/replace-command)
        ("y"             . my/yank)
        ("#"             . my/comment-command)
        ("%"             . query-replace-regexp)
        ("$"             . my/ispell)
        ("w"             . puni-wrap-commands)
        ("("             . kmacro-start-macro-or-insert-counter)
        (")"             . kmacro-end-or-call-macro)
        ("@"             . consult-kmacro)
        ("^"             . join-line)
        ("<backspace>"   . puni-backward-delete-char)
        ("S-<backspace>" . my/kill-whole-line)
        ("<return>"      . my/break-line)
        ("M-<return>"    . my/newline)
        ("M-S-<return>"  . my/newline-above)
        ("<escape>"      . keyboard-escape-quit))
  (:map more-commands
        ("h" . help-commands)
        ("b" . buffer-commands)
        ("w" . window-commands)
        ("p" . project-commands)
        ("g" . git-commands)
        ("B" . bookmark-commands)
        ("r" . register-commands)
        ("m" . consult-minor-mode-menu)
        ("M" . consult-mode-command)
        (":" . execute-extended-command)
        ("!" . my/shell-command)
        ("G" . embark-act)
        ("~" . universal-argument))
  (:map help-commands
        ("a" . apropos)
        ("k" . helpful-key)
        ("K" . helpful-macro)
        ("f" . helpful-callable)
        ("F" . helpful-function)
        ("c" . helpful-command)
        ("v" . helpful-variable)
        ("." . helpful-at-point)
        ("p" . describe-package)
        ("x" . xref-find-apropos)
        ("M" . describe-mode)
        ("i" . info-emacs-manual)
        ("I" . info-display-manual)
        ("s" . helpful-symbol)
        ("S" . consult-info)
        ("m" . consult-man))
  (:map puni-wrap-commands
        ("(" . puni-wrap-round)
        ("[" . puni-wrap-round)
        ("{" . puni-wrap-curly)
        ("<" . puni-wrap-angle))
  (:map search-commands
        ("/" . consult-line)
        ("?" . vertico-repeat-last)
        (":" . vertico-repeat-select))
  (:map insert-commands
        ("e" . emoji-insert)
        ("E" . emoji-search)
        ("t" . tempel-insert))
  (:map window-commands
        ("r" . split-window-right)
        ("b" . split-window-below)
        ("o" . other-window)
        ("c" . delete-window)
        ("O" . delete-other-windows))
  (:map buffer-commands
        ("s" . save-buffer)
        ("l" . consult-locate)
        ("f" . find-file-at-point)
        ("F" . ffap-other-window)
        ("R" . rename-visited-file)
        ("o" . consult-buffer)
        ("O" . consult-buffer-other-window)
        ("r" . consult-recent-file)
        ("i" . bs-show)
        ("I" . ibuffer-other-window)
        ("L" . flymake-switch-to-log-buffer)
        ("d" . dired-at-point)
        ("D" . ffap-dired-other-window)
        ("k" . kill-this-buffer))
  (:map project-commands
        ("b" . consult-project-buffer)
        ("B" . project-list-buffers)
        ("d" . project-find-dir)
        ("g" . consult-grep)
        ("f" . consult-find)
        ("l" . consult-locate)
        ("t" . my/citre-create-tags-file)
        ("m" . consult-flymake)
        ("s" . flymake-show-buffer-diagnostics)
        ("S" . flymake-show-project-diagnostics)
        ("e" . consult-compile-error)
        ("i" . consult-imenu-multi)
        ("c" . project-compile)
        ("o" . project-switch-project)
        ("x" . project-forget-project)
        ("X" . project-forget-zombie-projects)
        ("E" . project-eshell)
        ("!" . project-async-shell-command)
        ("k" . project-kill-buffers))
  (:map git-commands
        ("g" . magit-dispatch)
        ("s" . magit-status)
        ("c" . with-editor-finish)
        ("k" . with-editor-cancel)
        ("G" . consult-git-grep))
  (:map register-commands
        ("r" . consult-register)
        ("s" . consult-register-store)
        ("l" . consult-register-load))
  (:map bookmark-commands
        ("b" . consult-bookmark)
        ("l" . list-bookmarks)
        ("k" . bookmark-delete))
  (:map more-mark-commands
        ("w" . my/mark-word)
        ("d" . my/mark-defun)
        ("p" . mark-paragraph)
        ("b" . mark-whole-buffer)
        ("l" . my/mark-line-command))
  (:map more-motion-commands
        ("k" . backward-paragraph)
        ("j" . forward-paragraph)
        ("h" . beginning-of-line)
        ("l" . end-of-line)
        ("g" . beginning-of-buffer)
        ("G" . end-of-buffer)
        ("n" . compilation-next-error)
        ("p" . compilation-previous-error)
        ("[" . compilation-previous-file)
        ("]" . compilation-next-file)
        ("m" . consult-mark)
        ("M" . consult-global-mark)
        ("i" . consult-imenu)
        ("o" . consult-outline))
  :custom
  (multistate-lighter-indicator " ")
  (multistate-lighter-format "<%s>")
  (multistate-suppress-no-digits t)
  :hook
  ((helpful-mode
    help-mode
    apropos-mode
    Info-mode
    dired-mode
    bs-mode
    ibuffer-mode
    vundo-mode
    grep-mode
    occur-mode
    compilation-mode
    package-menu-mode
    messages-buffer-mode
    Man-mode
    bookmark-bmenu-mode
    flymake-diagnostics-buffer-mode
    magit-mode) . multistate-emacs-state)
  (multistate-normal-state-enter . (lambda ()
                                     (corfu-quit)))
  (read-only-mode . multistate-motion-state)
  :config
  (multistate-global-mode 1))

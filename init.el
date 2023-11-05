;;; -*- lexical-binding: t -*-

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

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

(unless (or (version< "29" emacs-version)
            (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t
      use-package-expand-minimally t)

(setq make-backup-files nil)

(setq auto-save-list-file-prefix (concat data-dir "auto-save-list/.saves-")
      kill-buffer-delete-auto-save-files t)

(setq global-auto-revert-non-file-buffers t
      auto-revert-check-vc-info t)
(global-auto-revert-mode 1)

(setq savehist-file (concat data-dir "history"))
(savehist-mode 1)

(setq recentf-save-file (concat data-dir "recentf"))
(recentf-mode 1)

(setq project-list-file (concat data-dir "projects"))

(tooltip-mode 0)

(pixel-scroll-mode t)

(column-number-mode 1)
(setq tab-bar-show 0)

(setq initial-scratch-message nil
      initial-major-mode 'fundamental-mode)

(setq x-underline-at-descent-line t)

(setq-default truncate-lines t)

(setq switch-to-buffer-obey-display-actions t)

(setq scroll-margin 10)

(setq use-short-answers t)

(setq visible-bell t)

(setq whitespace-style '(face
                         trailing
                         missing-newline-at-eof
                         empty))
(add-hook 'prog-mode-hook (lambda ()
                            (whitespace-mode 1)
                            (diminish 'whitespace-mode)))

(setq comment-multi-line t)

(setq sentence-end-double-space nil)

(setq delete-active-region 'kill)

(electric-pair-mode 1)

(setq kill-whole-line t)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(setq default-input-method "vietnamese-telex")

(setq imenu-auto-rescan t)

(setq xref-auto-jump-to-first-xref t
      xref-auto-jump-to-first-definition t)

(setq enable-recursive-minibuffers t
      resize-mini-windows t
      history-delete-duplicates t)
(minibuffer-electric-default-mode 1)
(minibuffer-depth-indicate-mode 1)

(setq ibuffer-expert t)

(setq compilation-scroll-output 'first-error
      compilation-auto-jump-to-first-error t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(with-eval-after-load "ibuffer"
  (define-key ibuffer-mode-map (kbd "j") 'ibuffer-forward-line)
  (define-key ibuffer-mode-map (kbd "k") 'ibuffer-backward-line))

(with-eval-after-load "minibuffer"
  (define-key minibuffer-mode-map (kbd "<escape>") 'abort-minibuffers)
  (define-key minibuffer-local-shell-command-map (kbd "<escape>") 'abort-minibuffers)
  (define-key read-expression-map (kbd "<escape>") 'abort-minibuffers))

(with-eval-after-load "help-mode"
  (define-key help-mode-map (kbd "J") 'scroll-up-command)
  (define-key help-mode-map (kbd "K") 'scroll-down-command))

(with-eval-after-load "info"
  (define-key Info-mode-map (kbd "J") 'Info-scroll-up)
  (define-key Info-mode-map (kbd "K") 'Info-scroll-down))

(with-eval-after-load "man"
  (define-key Man-mode-map (kbd "J") 'scroll-up-command)
  (define-key Man-mode-map (kbd "K") 'scroll-down-command))

(with-eval-after-load "grep"
  (define-key grep-mode-map (kbd "J") 'scroll-up-command)
  (define-key grep-mode-map (kbd "K") 'scroll-down-command))

(with-eval-after-load "package"
  (define-key package-menu-mode-map (kbd "J") 'scroll-up-command)
  (define-key package-menu-mode-map (kbd "K") 'scroll-down-command)
  (define-key package-menu-mode-map (kbd "j") 'next-line)
  (define-key package-menu-mode-map (kbd "k") 'previous-line))

(with-eval-after-load "dired"
  (define-key dired-mode-map (kbd "J") 'scroll-up-command)
  (define-key dired-mode-map (kbd "K") 'scroll-down-command)
  (define-key dired-mode-map (kbd "j") 'dired-next-line)
  (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  (define-key dired-mode-map (kbd "x") 'dired-kill-line)
  (define-key dired-mode-map (kbd "f") 'dired-goto-file))

(setq-default fill-column 100)
(add-hook 'text-mode-hook (lambda ()
                            (auto-fill-mode 1)))

(setq kill-do-not-save-duplicates t)

(setq bookmark-save-flag 1
      bookmark-default-file (concat data-dir "bookmarks"))

(setq completion-auto-help nil)

(setq disabled-command-function nil)

(use-package diminish
  :config
  (diminish 'global-auto-revert-mode)
  (diminish 'abbrev-mode)
  (diminish 'auto-fill-mode))

(use-package gcmh
  :diminish
  :config
  (gcmh-mode 1))

(use-package solarized-theme
  :defer t
  :custom
  (solarized-distinct-doc-face t)
  (solarized-distinct-fringe-background t)
  (solarized-high-contrast-mode-line t)
  (solarized-use-more-italic t)
  (solarized-scale-markdown-headlines t))

(use-package auto-dark
  :diminish
  :custom
  (auto-dark-dark-theme 'solarized-dark)
  (auto-dark-light-theme 'solarized-light)
  (auto-dark-polling-interval-seconds 60)
  :config
  (auto-dark-mode 1))

(use-package disable-mouse
  :config
  (global-disable-mouse-mode 1))

(use-package page-break-lines
  :diminish
  :hook (font-lock-mode . page-break-lines-mode))

(use-package pulsar
  :custom
  (pulsar-pulse t)
  :config
  (pulsar-global-mode 1))

(use-package puni
  :config
  (puni-global-mode 1))

(use-package smart-tabs-mode
  :config
  (smart-tabs-insinuate 'c
                        'c++
                        'java
                        'javascript
                        'cperl
                        'python
                        'ruby))

(use-package vundo
  :commands (vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t))

(use-package magit
  :commands (magit-status)
  :custom
  (magit-delete-by-moving-to-trash nil)
  (magit-view-git-manual-method 'man))

(use-package transient
  :ensure nil
  :config
  (defconst transient-dir (concat data-dir "transient/"))
  (setq transient-history-file (concat transient-dir "history.el")))

(use-package with-editor
  :ensure nil
  :defines (prefix
            suffix
            advice))

(use-package forge
  :after magit)

(use-package sqlite3
  :if (version< emacs-version "29"))

(use-package org
  :ensure nil
  :mode ("\\.org$" . org-mode)
  :custom
  (org-directory "~/Notes"))

(use-package helpful
  :defines helpful-mode-map
  :commands (helpful-callable
             helpful-command
             helpful-function
             helpful-variable
             helpful-at-point
             helpful-key)
  :bind (:map helpful-mode-map
              ("J" . scroll-up-command)
              ("K" . scroll-down-command)))

(use-package consult
  :demand t
  :bind
  (:map consult-narrow-map
        ("?" . consult-narrow-help))
  (:map minibuffer-local-map
        ([remap previous-matching-history-element] . consult-history)
        ([remap next-matching-history-element] . consult-history))
  :init
  (setq register-preview-delay 0.1
        register-preview-function 'consult-register-format)
  (setq xref-show-xrefs-function 'consult-xref
        xref-show-definitions-function 'consult-xref)
  (advice-add 'register-preview :override 'consult-register-window)
  :custom
  (consult-narrow-key "-")
  (consult-widen-key "=")
  :config
  (consult-customize consult-git-grep
                     consult-recent-file
                     consult-find
                     consult-locate
                     consult-global-mark
                     consult-theme
                     consult-bookmark
                     :preview-key '(:debounce 0.5 any)
                     consult-grep
                     :preview-key '(:debounce 0.2 any))
  (with-eval-after-load "man"
    (define-key Man-mode-map (kbd "o") 'consult-outline))
  (with-eval-after-load 'consult-imenu
    (add-to-list 'consult-imenu-config '(c-mode :toplevel "function"
                                                :types ((?f "function" font-lock-function-name-face)
                                                        (?v "variable" font-lock-variable-name-face)
                                                        (?d "typedef" font-lock-type-face)
                                                        (?s "struct" font-lock-type-face)
                                                        (?r "<reference>" font-lock-reference-face))))))

(use-package vertico
  :demand t
  :custom
  (vertico-scroll-margin 3)
  (vertico-cycle t)
  :bind (:map vertico-map
              ("<tab>" . vertico-next)
              ("<backtab>" . vertico-previous)
              ("<escape>" . abort-minibuffers))
  :config
  (vertico-mode 1)
  (vertico-multiform-mode 1)
  (setq vertico-multiform-commands
        '((consult-isearch-history flat)
          (consult-line flat))))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :demand t
  :init
  (defun my/vertico-directory-insert ()
    (interactive)
    (let* ((mb (minibuffer-contents-no-properties))
           (lc (if (string= mb "") mb (substring mb -1))))
      (cond ((string-match-p "^[/~:]" lc) (self-insert-command 1 ?/))
            ((file-directory-p (vertico--candidate)) (vertico-insert))
            (t (self-insert-command 1 ?/)))))
  :bind (:map vertico-map
              ("<return>" . vertico-directory-enter)
              ("<backspace>" . vertico-directory-delete-char)
              ("/" . my/vertico-directory-insert))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package corfu
  :hook ((prog-mode
          org-mode) . corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-scroll-margin 3)
  (corfu-preselect 'prompt)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  :bind (:map corfu-map
              ([remap corfu-complete] . corfu-next)
              ("<backtab>" . corfu-previous)
              ("<escape>" . corfu-quit)
              ("S-<backspace>" . corfu-reset))
  :config
  (setq completion-styles '(prescient basic)
        completion-category-defaults nil
        completion-category-overrides nil
        completion-cycle-threshold 1))

(use-package corfu-popupinfo
  :ensure nil
  :custom
  (corfu-popupinfo-delay 0.5)
  :config
  (corfu-popupinfo-mode 1))

(use-package prescient
  :after (vertico
          consult)
  :custom
  (prescient-save-file (concat data-dir "prescient-save.el"))
  (prescient-use-char-folding nil)
  (prescient-filter-method '(literal initialism))
  (prescient-sort-full-matches-first t)
  :config
  (prescient-persist-mode 1))

(use-package vertico-prescient
  :after (vertico
          prescient)
  :config
  (vertico-prescient-mode 1))

(use-package marginalia
  :after (vertico)
  :custom
  (marginalia-align 'right)
  :config
  (marginalia-mode 1))

(use-package embark
  :commands (embark-act
             embark-export)
  :bind
  (:map embark-general-map
        ("SPC" . nil)
        ("S-SPC" . embark-select))
  (:map minibuffer-local-map
        ("S-<return>" . embark-export)
        ("S-SPC" . embark-act))
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark
          consult))

(use-package cape
  :commands (cape-abbrev
             cape-dabbrev
             cape-dict
             cape-elisp-block
             cape-elisp-symbol
             cape-keyword
             cape-tex)
  :hook (prog-mode . (lambda ()
                       (add-to-list 'completion-at-point-functions
                                    'cape-keyword)))
        (emacs-lisp-mode . (lambda ()
                             (add-to-list 'completion-at-point-functions
                                          'cape-elisp-block)
                             (add-to-list 'completion-at-point-functions
                                          'cape-elisp-symbol)))
        (text-mode . (lambda ()
                       (add-to-list 'completion-at-point-functions
                                    'cape-dict)
                       (add-to-list 'completion-at-point-functions
                                    'cape-tex
                                    t))))

(use-package citre
  :init
  (require 'citre-config)
  :custom
  (citre-default-create-tags-file-location 'global-cache)
  (citre-use-project-root-when-creating-tags t)
  (citre-prompt-language-for-ctags-command t)
  (citre-auto-enable-citre-mode-modes '(prog-mode)))

(use-package visual-regexp
  :commands (vr/query-replace))

(with-eval-after-load 'make-mode
  (add-hook 'makefile-gmake-mode (lambda ()
                                   (setq-local indent-tabs-mode t))))

(with-eval-after-load 'cc-mode
  (setq c-default-style '((c-mode      . "linux")
                          (c++-mode    . "stroustrup")
                          (java-mode   . "java")
                          (awk-mode    . "awk")
                          (python-mode . "python")
                          (awk-mode    . "awk")))
  (add-hook 'c-mode-hook (lambda ()
                           (setq-local indent-tabs-mode t)
                           (setq-local c-basic-offset 4)
                           (c-toggle-hungry-state t)))
  (add-hook 'c++-mode-hook (lambda ()
                             (setq-local indent-tabs-mode t)
                             (setq-local c-basic-offset 4))))

(use-package js2-mode
  :mode (("\\.\\(js\\|cjs\\|mjs\\)$" . js2-mode)
         ("\\.jsx$"                  . js2-jsx-mode)
         ("^node$"                   . js2-mode))
  :hook (js2-mode . (lambda ()
                      (setq-local indent-tabs-mode t))))

(use-package json-mode
  :mode ("\\.json$" . json-mode))

(use-package eglot
  :if (version< emacs-version "29")
  :defer t)

(use-package markdown-mode
  :mode ("\\.\\(md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)$" . markdown-mode)
  :hook ((markdown-mode gfm-mode) . (lambda ()
                                      (visual-line-mode 1)
                                      (if (package-installed-p 'corfu)
                                          (corfu-mode 1)))))

(use-package which-key
  :custom
  (which-key-lighter "")
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0.1)
  (which-key-popup-tyle 'minibuffer)
  (which-key-show-remaining-keys t)
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
   'custom
   :lighter "C"
   :parent 'multistate-motion-state-map)
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
  (define-prefix-command 'more-mark-commands)
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
  (defun my/comment-command ()
    ;; FIXME: doesn’t handle unbalanced region if commented out
    (interactive)
    (if (region-active-p)
        (if (puni-region-balance-p (region-beginning)
                                   (region-end))
            (comment-or-uncomment-region (region-beginning)
                                         (region-end))
          (user-error "%s" "Unbalanced region, can’t comment region"))
      (if (puni-region-balance-p (line-beginning-position)
                                 (line-end-position)
                                 t)
          (comment-or-uncomment-region (line-beginning-position)
                                       (line-end-position))
        (user-error "%s" "Unbalanced sexp detected, can’t comment line"))))
  (defun my/query-replace-regexp ()
    (interactive)
    (unless (region-active-p)
      (beginning-of-buffer))
    (call-interactively 'vr/query-replace))
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
        ("/"    . consult-line)
        ("m"    . set-mark-command)
        ("M"    . more-mark-commands)
        ("x"    . exchange-point-and-mark)
        ("+"    . puni-expand-region)
        ("p"    . previous-error)
        ("n"    . next-error)
        ("."    . xref-find-definitions)
        (","    . xref-pop-marker-stack)
        ("s"    . kill-ring-save)
        ("G"    . embark-act)
        ("!"    . my/shell-command)
        (":"    . execute-extended-command)
        ("g"    . more-motion-commands)
        ("SPC"  . more-commands)
        ("C-\\" . ignore))
  (:map multistate-custom-state-map
        ("i" . multistate-insert-state))
  (:map multistate-normal-state-map
        ("i"             . multistate-insert-state)
        ("u"             . undo)
        ("U"             . vundo)
        ("%"             . my/query-replace-regexp)
        ("r"             . puni-raise)
        ("c"             . my/replace-command)
        ("y"             . my/yank)
        (";"             . my/comment-command)
        ("$"             . ispell-word)
        ("^"             . join-line)
        ("<backspace>"   . puni-backward-delete-char)
        ("S-<backspace>" . my/kill-whole-line)
        ("<return>"      . my/newline)
        ("S-<return>"    . my/newline-above)
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
        ("S" . info-lookup-symbol)
        ("m" . consult-man))
  (:map window-commands
        ("r" . split-window-right)
        ("b" . split-window-below)
        ("o" . other-window)
        ("c" . delete-window)
        ("O" . delete-other-windows))
  (:map buffer-commands
        ("s" . save-buffer)
        ("S" . save-some-buffers)
        ("w" . write-file)
        ("l" . consult-locate)
        ("f" . find-file)
        ("F" . consult-find)
        ("o" . consult-buffer)
        ("O" . consult-buffer-other-window)
        ("r" . consult-recent-file)
        ("i" . bs-show)
        ("I" . ibuffer)
        ("d" . dired)
        ("k" . kill-buffer))
  (:map project-commands
        ("b" . consult-project-buffer)
        ("d" . project-dired)
        ("g" . consult-grep)
        ("f" . consult-find)
        ("m" . consult-flymake)
        ("e" . consult-compile-error)
        ("F" . find-file)
        ("i" . consult-imenu-multi)
        ("c" . project-compile)
        ("o" . project-switch-project)
        ("x" . project-forget-project)
        ("X" . project-forget-zombie-projects)
        ("S" . project-shell)
        ("E" . project-eshell)
        ("!" . project-async-shell-command)
        ("k" . project-kill-buffers))
  (:map git-commands
        ("c" . with-editor-finish)
        ("k" . with-editor-cancel)
        ("s" . magit-status)
        ("g" . consult-git-grep))
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
        ("l" . my/mark-line-command)
        ("s" . puni-mark-sexp-at-point)
        ("S" . puni-mark-sexp-around-point))
  (:map more-motion-commands
        ("k" . backward-paragraph)
        ("j" . forward-paragraph)
        ("b" . puni-beginning-of-sexp)
        ("e" . puni-end-of-sexp)
        ("B" . beginning-of-defun)
        ("E" . end-of-defun)
        ("h" . beginning-of-line)
        ("l" . end-of-line)
        ("g" . beginning-of-buffer)
        ("G" . end-of-buffer)
        ("m" . consult-mark)
        ("M" . consult-global-mark)
        ("i" . consult-imenu)
        ("o" . consult-outline))
  :custom
  (multistate-lighter-indicator " ")
  (multistate-lighter-format "<%s>")
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
    magit-mode) . multistate-emacs-state)
  (multistate-normal-state-enter . corfu-quit)
  (read-only-mode . multistate-motion-state)
  :config
  (multistate-global-mode 1))

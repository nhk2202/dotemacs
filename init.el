;;; -*- lexical-binding: t -*-

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)

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

(require 'use-package)
(setq use-package-always-ensure t
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

(setq make-backup-files nil)

(setq auto-save-list-file-prefix (concat data-dir "auto-save-list/.saves-")
      kill-buffer-delete-auto-save-files t)

(setq global-auto-revert-non-file-buffers t
      auto-revert-check-vc-info t)
(global-auto-revert-mode 1)

(setq multisession-directory (concat data-dir "multisession/"))

(setq savehist-file (concat data-dir "history"))
(savehist-mode 1)

(setq recentf-save-file (concat data-dir "recentf"))
(recentf-mode 1)

(setq project-list-file (concat data-dir "projects"))

(pixel-scroll-precision-mode t)

(column-number-mode 1)
(setq tab-bar-show 1)

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

(setq comment-multi-line t)

(setq sentence-end-double-space nil)

(global-subword-mode 1)

(setq delete-active-region 'kill)

(setq electric-quote-replace-consecutive nil)
(electric-pair-mode 1)

(setq kill-whole-line t)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

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

(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "j") 'ibuffer-forward-line)
  (define-key ibuffer-mode-map (kbd "k") 'ibuffer-backward-line))

(setq dired-switches-in-mode-line 'as-is)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "j") 'dired-next-line)
  (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  (define-key dired-mode-map (kbd "O") 'dired-display-file)
  (define-key dired-mode-map (kbd "/") 'dired-goto-file))

(setq-default fill-column 100)
(add-hook 'text-mode-hook (lambda ()
                            (auto-fill-mode 1)))

(setq kill-do-not-save-duplicates t)

(setq bookmark-save-flag t
      bookmark-default-file (concat data-dir "bookmarks"))

(setq completion-auto-help nil)

(setq help-window-keep-selected t
      help-enable-variable-value-editing t)

(setq disabled-command-function nil)

(use-package diminish
  :hook
  (prog-mode . (lambda ()
                 (whitespace-mode 1)
                 (diminish 'whitespace-mode)))
  :config
  (diminish 'global-auto-revert-mode)
  (diminish 'abbrev-mode)
  (diminish 'auto-fill-mode))

(use-package gcmh
  :diminish
  :custom
  (gcmh-verbose t)
  :config
  (gcmh-mode 1))

(use-package solarized-theme
  :defer t
  :custom
  (solarized-distinct-doc-face t)
  (solarized-distinct-fringe-background t)
  (solarized-high-contrast-mode-line t)
  (solarized-use-more-italic t)
  (solarized-scale-markdown-headlines t)
  :custom-face
  (font-lock-comment-face ((t (:slant italic)))))

(use-package auto-dark
  :diminish
  :custom
  (auto-dark-dark-theme 'solarized-dark)
  (auto-dark-light-theme 'solarized-light)
  (auto-dark-polling-interval-seconds 60)
  :config
  (auto-dark-mode 1))

(use-package page-break-lines
  :diminish
  :hook (font-lock-mode . page-break-lines-mode))

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
        ("L" . vundo-goto-last-saved)
        ("s" . vundo-save)
        ("H" . vundo-stem-root)
        ("K" . vundo-stem-end)))

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

(use-package with-editor
  :ensure nil
  :defines (prefix
            suffix
            advice))

(use-package forge
  :after magit)

(use-package org
  :ensure nil
  :mode ("\\.org$" . org-mode)
  :custom
  (org-directory "~/Notes"))

(use-package helpful
  :defines helpful-mode-map
  :commands (helpful-callable
             helpful-command
             helpful-symbol
             helpful-function
             helpful-variable
             helpful-at-point
             helpful-key))

(use-package consult
  :demand t
  :bind
  (:map consult-narrow-map
        ("?" . consult-narrow-help))
  :init
  (setq register-preview-delay 0.2
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
                     consult-grep
                     :preview-key '(:debounce 0.2 any))
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
  (corfu-auto t)
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
  :hook
  (embark-after-export . (lambda ()
                           (other-window 1)
                           (next-error)))
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
             cape-keyword)
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
                              'cape-dict))))

(use-package citre
  :init
  (require 'citre-config)
  :custom
  (citre-default-create-tags-file-location 'global-cache)
  (citre-use-project-root-when-creating-tags t)
  (citre-prompt-language-for-ctags-command t)
  (citre-auto-enable-citre-mode-modes '(prog-mode)))

(with-eval-after-load 'make-mode
  (add-hook 'makefile-gmake-mode (lambda ()
                                   (setq-local indent-tabs-mode t))))

(with-eval-after-load 'cc-mode
  (setq c-default-style '((c-mode      . "linux")
                          (c++-mode    . "stroustrup")
                          (java-mode   . "java")
                          (awk-mode    . "awk")
                          (python-mode . "python")))
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
  :ensure nil
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
  (define-prefix-command 'tab-commands)
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
  (defun my/replace-regexp (&optional query)
    (interactive "P")
    (unless (region-active-p)
      (beginning-of-buffer))
    (if query
        (call-interactively 'query-replace-regexp)
      (call-interactively 'replace-regexp)))
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
  (:map multistate-normal-state-map
        ("i"             . multistate-insert-state)
        ("e"             . emoji-insert)
        ("E"             . emoji-search)
        ("u"             . undo)
        ("U"             . vundo)
        ("r"             . puni-raise)
        ("c"             . my/replace-command)
        ("y"             . my/yank)
        (";"             . my/comment-command)
        ("%"             . my/replace-regexp)
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
        ("t" . tab-commands)
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
        ("." . find-file-at-point)
        ("r" . rename-visited-file)
        ("o" . consult-buffer)
        ("O" . consult-buffer-other-window)
        ("r" . consult-recent-file)
        ("i" . bs-show)
        ("I" . ibuffer)
        ("d" . dired-jump-other-window)
        ("D" . dired)
        ("k" . kill-buffer))
  (:map tab-commands
        ("t" . tab-bar-new-tab)
        ("c" . tab-bar-close-tab)
        ("o" . tab-bar-close-other-tabs)
        ("s" . tab-bar-switch-to-tab)
        ("n" . tab-bar-switch-to-next-tab)
        ("p" . tab-bar-switch-to-prev-tab)
        ("r" . tab-bar-rename-tab)
        ("R" . tab-bar-rename-tab-by-name))
  (:map project-commands
        ("b" . consult-project-buffer)
        ("B" . project-list-buffers)
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
        ("g" . magit-dispatch)
        ("s" . magit-status)
        ("c" . with-editor-finish)
        ("k" . with-editor-cancel)
        ("/" . consult-git-grep))
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

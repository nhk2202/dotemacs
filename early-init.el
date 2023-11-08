;;; -*- lexical-binding: t -*-

(setq package-enable-at-startup nil)

(setq gc-cons-threshold most-positive-fixnum)

(setq inhibit-startup-screen t)

(defconst data-dir (concat user-emacs-directory ".data/"))
(unless (file-exists-p data-dir)
  (make-directory data-dir))

(add-to-list 'native-comp-eln-load-path (concat data-dir "eln-cache/"))

(setq native-comp-async-report-warnings-errors 'silent
      byte-compile-warnings '(not obsolete))

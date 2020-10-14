;;; chemacs.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;
;;       ___           ___           ___           ___           ___           ___           ___
;;      /  /\         /__/\         /  /\         /__/\         /  /\         /  /\         /  /\
;;     /  /:/         \  \:\       /  /:/_       |  |::\       /  /::\       /  /:/        /  /:/_
;;    /  /:/           \__\:\     /  /:/ /\      |  |:|:\     /  /:/\:\     /  /:/        /  /:/ /\
;;   /  /:/  ___   ___ /  /::\   /  /:/ /:/_   __|__|:|\:\   /  /:/~/::\   /  /:/  ___   /  /:/ /::\
;;  /__/:/  /  /\ /__/\  /:/\:\ /__/:/ /:/ /\ /__/::::| \:\ /__/:/ /:/\:\ /__/:/  /  /\ /__/:/ /:/\:\
;;  \  \:\ /  /:/ \  \:\/:/__\/ \  \:\/:/ /:/ \  \:\~~\__\/ \  \:\/:/__\/ \  \:\ /  /:/ \  \:\/:/~/:/
;;   \  \:\  /:/   \  \::/       \  \::/ /:/   \  \:\        \  \::/       \  \:\  /:/   \  \2.0 /:/
;;    \  \:\/:/     \  \:\        \  \:\/:/     \  \:\        \  \:\        \  \:\/:/     \__\/ /:/
;;     \  \::/       \  \:\        \  \::/       \  \:\        \  \:\        \  \::/        /__/:/
;;      \__\/         \__\/         \__\/         \__\/         \__\/         \__\/         \__\/
;;
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;
;; Chemacs - Emacs Profile Switcher
;;
;; See README.md for instructions.

;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; this must be here to keep the package system happy, normally you do
;; `package-initialize' for real in your own init.el
;; (package-initialize)

;;; Code:
(defvar chemacs-version "2.0")
(defvar config-home (or (getenv "XDG_CONFIG_HOME") "~/.config"))
(defvar chemacs-profiles-paths (list "~/.emacs-profiles.el" (format "%s/%s" config-home "chemacs/profiles.el" )))
(defvar chemacs-default-profile-paths (list "~/.emacs-profile" (format "%s/%s" config-home "profile")))

(defvar chemacs-profiles-path (or (car (seq-filter 'file-exists-p chemacs-profiles-paths)) (car chemacs-profiles-paths)))
(defvar chemacs-default-profile-path (or (car (seq-filter 'file-exists-p chemacs-profiles-paths)) (car chemacs-profiles-paths)))

(when (not (file-exists-p chemacs-profiles-path))
  (error "[chemacs] %s does not exist." chemacs-profiles-path))

(defvar chemacs-emacs-profiles
  (with-temp-buffer
    (insert-file-contents chemacs-profiles-path)
    (goto-char (point-min))
    (read (current-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chemacs-detect-default-profile ()
  (if (file-exists-p chemacs-default-profile-path)
      (with-temp-buffer
        (insert-file-contents chemacs-default-profile-path)
        (goto-char (point-min))
        ;; (buffer-string))
        (symbol-name (read (current-buffer)) ))
    "default"))

(defun chemacs-load-straight ()
  (defvar bootstrap-version)
  (let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chemacs-get-emacs-profile (profile)
  (cdr (assoc profile chemacs-emacs-profiles)))

(defun chemacs-emacs-profile-key (key &optional default)
  (alist-get key (chemacs-get-emacs-profile chemacs-current-emacs-profile)
             default))

(defun chemacs-load-profile (profile)
  (when (not (chemacs-get-emacs-profile profile))
    (error "No profile `%s' in %s" profile chemacs-profiles-path))
  (setq chemacs-current-emacs-profile profile)
  (let* ((emacs-directory (file-name-as-directory
                           (chemacs-emacs-profile-key 'user-emacs-directory)))
         (init-file       (expand-file-name "init.el" emacs-directory))
         (custom-file-    (chemacs-emacs-profile-key 'custom-file init-file))
         (server-name-    (chemacs-emacs-profile-key 'server-name))
         (early-init-file (expand-file-name "early-init.el" emacs-directory)))
    (setq user-emacs-directory emacs-directory)

    ;; Allow multiple profiles to each run their server
    ;; use `emacsclient -s profile_name' to connect
    (when server-name-
      (setq server-name server-name-))

    ;; Set environment variables, these are visible to init-file with getenv
    (mapcar (lambda (env)
              (setenv (car env) (cdr env)))
            (chemacs-emacs-profile-key 'env))

    (if (and (boundp 'chemacs-early-init) chemacs-early-init)
        (when (file-exists-p early-init-file)
          (load early-init-file)
          (setq chemacs-early-init nil))
      (when (chemacs-emacs-profile-key 'straight-p)
        (chemacs-load-straight))

      ;; Start the actual initialization
      (load init-file)

      ;; Prevent customize from changing ~/.emacs (this file), but if init.el has
      ;; set a value for custom-file then don't touch it.
      (when (not custom-file)
        (setq custom-file custom-file-)
        (unless (equal custom-file init-file)
          (load custom-file))))))

(defun chemacs-check-command-line-args (args)
  (if args
      ;; Handle either `--with-profile profilename' or
      ;; `--with-profile=profilename'
      (let ((s (split-string (car args) "=")))
        (cond ((equal (car args) "--with-profile")
               ;; This is just a no-op so Emacs knows --with-profile
               ;; is a valid option. If we wait for
               ;; command-switch-alist to be processed then
               ;; after-init-hook has already run.
               (add-to-list 'command-switch-alist
                            '("--with-profile" .
                              (lambda (_) (pop command-line-args-left))))
               ;; Load the profile
               (chemacs-load-profile (cadr args)))

              ;; Similar handling for `--with-profile=profilename'
              ((equal (car s) "--with-profile")
               (add-to-list 'command-switch-alist `(,(car args) . (lambda (_))))
               (chemacs-load-profile (mapconcat 'identity (cdr s) "=")))

              (t (chemacs-check-command-line-args (cdr args)))))

    ;; If no profile given, load the "default" profile
    (chemacs-load-profile (chemacs-detect-default-profile))))

(chemacs-check-command-line-args command-line-args)

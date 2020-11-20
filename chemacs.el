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

;;; Code:
(require 'seq)

(defvar chemacs-version "2.0")
(defvar config-home (or (getenv "XDG_CONFIG_HOME") "~/.config"))
(defvar chemacs-profiles-paths (list "~/.emacs-profiles.el" (format "%s/%s" config-home "chemacs/profiles.el" )))
(defvar chemacs-default-profile-paths (list "~/.emacs-profile" (format "%s/%s" config-home "chemacs/profile")))

(defvar chemacs-profiles-path (or (car (seq-filter 'file-exists-p chemacs-profiles-paths)) (car chemacs-profiles-paths)))
(defvar chemacs-default-profile-path (or (car (seq-filter 'file-exists-p chemacs-default-profile-paths)) (car chemacs-default-profile-paths)))

(when (not (file-exists-p chemacs-profiles-path))
  (error "[chemacs] %s does not exist." chemacs-profiles-path))

(defvar chemacs-default-profile-name
  (if (file-exists-p chemacs-default-profile-path)
      (with-temp-buffer
        (insert-file-contents chemacs-default-profile-path)
        (goto-char (point-min))
        ;; (buffer-string))
        (symbol-name (read (current-buffer)) ))
    "default"))

(defun chemacs-handle-command-line (args)
  (when args
    ;; Handle either --with-profile profilename or
    ;; --with-profile=profilename
    (let ((s (split-string (car args) "=")))
      (cond ((equal (car args) "--with-profile")
             ;; This is just a no-op so Emacs knows --with-profile
             ;; is a valid option. If we wait for
             ;; command-switch-alist to be processed then
             ;; after-init-hook has already run.
             (add-to-list 'command-switch-alist
                          '("--with-profile" .
                            (lambda (_) (pop command-line-args-left))))
             (cadr args))

            ;; Similar handling for `--with-profile=profilename'
            ((equal (car s) "--with-profile")
             (add-to-list 'command-switch-alist `(,(car args) . (lambda (_))))
             (mapconcat 'identity (cdr s) "="))

            (t (chemacs-handle-command-line (cdr args)))))))

(defvar chemacs-profile-name
  (let ((name (chemacs-handle-command-line command-line-args)))
    (if name name chemacs-default-profile-name)))

(defvar chemacs-profile
  (let ((profiles
         (with-temp-buffer
           (insert-file-contents chemacs-profiles-path)
           (goto-char (point-min))
           (read (current-buffer)))))
    (cdr (assoc chemacs-profile-name profiles))))

(unless chemacs-profile
  (error "No profile `%s' in %s" profile chemacs-profiles-path))

(defun chemacs-profile-get (key &optional default)
  (alist-get key chemacs-profile default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-emacs-directory (file-name-as-directory
                            (chemacs-profile-get 'user-emacs-directory)))

;; Allow multiple profiles to each run their server
;; use `emacsclient -s profile_name' to connect
(let ((name (chemacs-profile-get 'server-name)))
  (when name (setq server-name name)))

;; Set environment variables, these are visible to init-file with
;; getenv
(mapcar (lambda (env)
          (setenv (car env) (cdr env)))
        (chemacs-profile-get 'env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chemacs-load-user-early-init ()
  (let ((early-init-file (expand-file-name "early-init.el" user-emacs-directory)))
    (load early-init-file t t)))

(defun chemacs-load-user-init ()
  (when (chemacs-profile-get 'straight-p) (chemacs-load-straight))
  (let ((init-file (expand-file-name "init.el" user-emacs-directory)))
    (load init-file t t)
    ;; Prevent customize from changing ~/.emacs (this file), but if
    ;; init.el has set a value for custom-file then don't touch it.
    (let ((custom-file- (chemacs-profile-get 'custom-file init-file)))
      (when (not custom-file)
        (setq custom-file custom-file-)
        (unless (equal custom-file init-file)
          (load custom-file))))))

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

(provide 'chemacs)

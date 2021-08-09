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

;; NOTE Don't require any libraries in this file. When emacs loads a library that
;; is byte compiled, it may start native-compiling it, so if we require anything
;; here, native compilation can start before the user has had a chance to configure
;; it in their init files.

;;; Code:
(defvar chemacs-version "2.0")
(defvar config-home (or (getenv "XDG_CONFIG_HOME") "~/.config"))
(defvar chemacs-profiles-paths (list "~/.emacs-profiles.el" (format "%s/%s" config-home "chemacs/profiles.el" )))
(defvar chemacs-default-profile-paths (list "~/.emacs-profile" (format "%s/%s" config-home "chemacs/profile")))
(defvar chemacs-profile-env-var "CHEMACS_PROFILE")

;; Copy `seq' library's `seq-filter' to avoid requiring it, see note above.
(defun chemacs--seq-filter (pred sequence)
  (let ((exclude (make-symbol "exclude")))
    (delq exclude (mapcar (lambda (elt)
                            (if (funcall pred elt)
                                elt
                              exclude))
                          sequence))))

(defvar chemacs-profiles-path (or (car (chemacs--seq-filter #'file-exists-p chemacs-profiles-paths))
                                  (car chemacs-profiles-paths)))
(defvar chemacs-default-profile-path (or (car (chemacs--seq-filter #'file-exists-p chemacs-default-profile-paths))
                                         (car chemacs-default-profile-paths)))

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

(defvar chemacs--with-profile-value
  (let* ((value (chemacs-handle-command-line command-line-args))
         (read-value (read value)))
    (when value
      (if (listp read-value)
          read-value
        value))))

(defvar chemacs-literal-profile-provided
  (and chemacs--with-profile-value
       (listp chemacs--with-profile-value)))

(unless (or (file-exists-p chemacs-profiles-path)
            (and chemacs--with-profile-value
                 (listp chemacs--with-profile-value)))
  (error "[chemacs] %s does not exist." chemacs-profiles-path))

(defvar chemacs-default-profile-name
  (if (file-exists-p chemacs-default-profile-path)
      (with-temp-buffer
        (insert-file-contents chemacs-default-profile-path)
        (goto-char (point-min))
        ;; (buffer-string))
        (symbol-name (read (current-buffer)) ))
    "default"))


(defvar chemacs-profile-name
  (let ((env-profile-value (getenv chemacs-profile-env-var)))
    (cond ((and chemacs--with-profile-value
                (stringp chemacs--with-profile-value))
           chemacs--with-profile-value)
          (env-profile-value env-profile-value)
          (t chemacs-default-profile-name))))

(defvar chemacs-profile
  (if (and chemacs--with-profile-value
           (listp chemacs--with-profile-value))
      chemacs--with-profile-value
      (let ((profiles
         (with-temp-buffer
           (insert-file-contents chemacs-profiles-path)
           (goto-char (point-min))
           (read (current-buffer)))))
    (cdr (assoc chemacs-profile-name profiles)))))

(unless chemacs-profile
  (error "No profile `%s' in %s" chemacs-profile-name chemacs-profiles-path))

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
    (setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
    (load init-file t t)
    ;; Prevent customize from changing ~/.emacs (this file), but if
    ;; init.el has set a value for custom-file then don't touch it.
    (let ((chemacs-custom-file (chemacs-profile-get 'custom-file init-file)))
      (when (not custom-file)
        (setq custom-file chemacs-custom-file)
        (unless (equal custom-file init-file)
          (unless (file-exists-p custom-file)
            (with-temp-buffer (write-file custom-file)))
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

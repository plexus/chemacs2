;;; early-init.el --- -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(undecorated-round . t))
(require 'chemacs
         (expand-file-name "chemacs.el"
                           (file-name-directory
                            (file-truename load-file-name))))
(chemacs-load-user-early-init)

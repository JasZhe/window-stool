;;; code-context-window.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Jason Zhen
;;
;; Author: Jason Zhen <jaszhe@gmail.com>
;; Maintainer: Jason Zhen <jaszhe@gmail.com>
;; Created: December 19, 2023
;; Modified: December 19, 2023
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jasonzhen/code-context-window
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEDICATED WINDOW IMPL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq code-context-window nil)
(defun code-context-window-create (_ _)
  (let* ((ctx (save-excursion (get-context-from (window-start))))
         (buf (get-buffer-create " *Code Context*"))
         (win (or (get-buffer-window " *Code Context*") (split-window (selected-window) (- (length ctx)) 'above))))
    (message "window %s" win)
    (when win
      (set-window-buffer win buf)
      (window-resize win (- (length ctx)))
      (with-current-buffer buf
        (erase-buffer)
        (setq mode-line-format nil)
        (dolist (c ctx) (insert c "\n"))
        ))
    )
  )

;; call windmove up/down again if we switch into the code context window
(defun code-context-windmove-up-advice ()
  (when (string= (buffer-name) " *Code Context*")
    (windmove-up))
  )
(defun code-context-windmove-down-advice ()
  (when (string= (buffer-name) " *Code Context*")
    (windmove-down))
  )

(defun code-context-split-window-advice (&optional _ _ _ _)
  (let ((win (get-buffer-window " *Code Context*")))
    (when win (delete-window win)))
  )

(defun advise-window-functions ()
  (advice-add 'windmove-up :after #'code-context-windmove-up-advice)
  (advice-add 'windmove-down :after #'code-context-windmove-down-advice)
  (advice-add 'split-window :before #'code-context-split-window-advice)
  )

(defun remove-window-function-advice ()
  (advice-remove 'windmove-up #'code-context-windmove-up-advice)
  (advice-remove 'windmove-down #'code-context-windmove-down-advice)
  (advice-remove 'split-window #'code-context-split-window-advice)
  )

(provide 'code-context-window)
;;; code-context-window.el ends here

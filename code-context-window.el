;;; code-context-window.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Jason Zhen
;;
;; Author: Jason Zhen
;; Maintainer: Jason Zhen
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


(defun get-context-from--window (pos)
  (goto-char pos)
  (find-previous-non-empty-line)
  (let ((ctx '())
        (prev-indentation (current-indentation)))
    (while (> (current-indentation) 0)
      (forward-line -1)
      (find-previous-non-empty-line)
      (when (< (current-indentation) prev-indentation)
        (setq prev-indentation (current-indentation))
        (let ((ctx-str (buffer-substring (line-beginning-position) (line-end-position))))
          (cl-pushnew ctx-str ctx))
        )
      )
    ctx))

(setq code-context-window nil)
(defun code-context-window-create ()
  (when (not (eq (window-start) prev-window-start))
    (let* ((ctx (save-excursion (get-context-from--window (window-start))))
           (buf (get-buffer-create " *Code Context*"))
           (win (or (get-buffer-window " *Code Context*") (split-window (selected-window) (- (max (length ctx) window-min-height)) 'above))))
      (when win
        (set-window-buffer win buf)
        (ignore-errors (window-resize win (- (+ 1 (length ctx)) (window-body-height win))))
        (with-current-buffer buf
          (erase-buffer)
          (setq mode-line-format "---------------context---------------")
          (dolist (c ctx) (insert c "\n"))
          (when ctx (delete-char -1))
          ))
      ))
  )

;; call windmove up/down again if we switch into the code context window
;; to basically switch "past" it if we have two "real" windows on top of each other
;; and rebalance
(defun code-context-windmove-up-advice ()
  (when (string= (buffer-name) " *Code Context*")
    (windmove-up)
    )
  )
(defun code-context-windmove-down-advice ()
  (when (string= (buffer-name) " *Code Context*")
    (windmove-down))
  )

(defun code-context-split-window-advice (&optional _ _ _ _)
  (let ((win (get-buffer-window " *Code Context*")))
    (when win (delete-window win)))
  )

;; rebalance windows after deleting the code context one
(defun code-context-window-delete (_)
  (let ((win (get-buffer-window " *Code Context*")))
    (when win
      (delete-window win)
      (balance-windows)
      ))
  )


(defun advise-window-functions ()
  (advice-add 'windmove-up :after #'code-context-windmove-up-advice)
  (advice-add 'windmove-down :after #'code-context-windmove-down-advice)
  (advice-add 'split-window :before #'code-context-split-window-advice)
  ;; only allow the code context window to be active in the current window
  (add-to-list 'window-selection-change-functions #'code-context-window-delete)
  )

(defun remove-window-function-advice ()
  (advice-remove 'windmove-up #'code-context-windmove-up-advice)
  (advice-remove 'windmove-down #'code-context-windmove-down-advice)
  (advice-remove 'split-window #'code-context-split-window-advice)
  (setq window-selection-change-functions (remove #'code-context-window-delete window-selection-change-functions))
  )

(provide 'code-context-window)
;;; code-context-window.el ends here

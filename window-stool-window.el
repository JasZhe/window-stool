;;; window-stool-window.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Jason Zhen
;;
;; Author: Jason Zhen
;; Maintainer: Jason Zhen
;; Created: December 19, 2023
;; Modified: December 19, 2023
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: context window
;; Homepage: https://github.com/jasonzhen/window-stool
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Sort of experimental, I much prefer the overlays but I played around with having a "dedicated" window
;; for this in my head.
;;
;;; Description:
;; Window stool using an actual window instead of overlays with some convenience advice
;; to various window switching functions to allow "skipping" the window stool
;; if, for example, we have two real windows on top of each other.
;;
;;; Code:


(defun window-stool-window--get-context (pos)
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

(setq window-stool-window nil)
(setq window-stool-window--buffer-name " *Window Stool*")

(defun window-stool-window--create ()
  (when (not (eq (window-start) prev-window-start))
    (let* ((ctx (save-excursion (window-stool-window--get-context (window-start))))
           (buf (get-buffer-create window-stool-window--buffer-name))
           (win (or (get-buffer-window window-stool-window--buffer-name) (split-window (selected-window) (- (max (length ctx) window-min-height)) 'above))))
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
(defun window-stool-window--windmove-up-advice ()
  (when (string= (buffer-name) window-stool-window--buffer-name)
    (windmove-up)
    )
  )
(defun window-stool-window--windmove-down-advice ()
  (when (string= (buffer-name) window-stool-window--buffer-name)
    (windmove-down))
  )

(defun window-stool-window--split-window-advice (&optional _ _ _ _)
  (let ((win (get-buffer-window window-stool-window--buffer-name)))
    (when win (delete-window win)))
  )

;; rebalance windows after deleting the code context one
(defun window-stool-window--delete (_)
  (let ((win (get-buffer-window window-stool-window--buffer-name)))
    (when win
      (delete-window win)
      (balance-windows)
      ))
  )


(defun window-stool-window--advise-window-functions ()
  (advice-add 'windmove-up :after #'window-stool-window--windmove-up-advice)
  (advice-add 'windmove-down :after #'window-stool-window--windmove-down-advice)
  (advice-add 'split-window :before #'window-stool-window--split-window-advice)
  ;; only allow the code context window to be active in the current window
  (add-to-list 'window-selection-change-functions #'window-stool-window--delete)
  )

(defun window-stool-window--remove-window-function-advice ()
  (advice-remove 'windmove-up #'window-stool-window--windmove-up-advice)
  (advice-remove 'windmove-down #'window-stool-window--windmove-down-advice)
  (advice-remove 'split-window #'window-stool-window--split-window-advice)
  (setq window-selection-change-functions (remove #'window-stool-window--delete window-selection-change-functions))
  )

(provide 'window-stool-window)
;;; window-stool-window.el ends here

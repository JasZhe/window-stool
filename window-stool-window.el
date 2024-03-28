;;; window-stool-window.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Jason Zhen
;;
;; Author: Jason Zhen
;; Maintainer: Jason Zhen
;; Created: December 19, 2023
;; Modified: December 19, 2023
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: context window
;; Homepage: https://github.com/jasonzhen/window-stool
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

(setq window-stool-window nil)
(setq window-stool-window--buffer-name " *Window Stool*")

(defun window-stool-window--create ()
  "Create a code context window above the currently selected window."

  ;; Fixes issue with window stool window sometimes sticking around after #'delete-window
  ;; if the next selected window was a non file visiting buffer.
  ;; Needs post-command-hook attached to this to be non-local
  (when (overlayp window-stool-overlay) (delete-overlay window-stool-overlay))
  (window-stool-window--delete)
  ;; only in file visiting buffers
  (when (and (buffer-file-name) (not (eq window-stool-fn #'ignore)))
    (let* ((ctx-1 (window-stool--truncate-context (save-excursion (funcall window-stool-fn (window-start)))))
           (ctx (if (not (eq major-mode 'org-mode)) (cl-subseq ctx-1 0 (1- (length ctx-1))) ctx-1))
           (buf-name window-stool-window--buffer-name)
           (buf (get-buffer-create buf-name))
           (win (display-buffer-in-direction buf '((direction . above)))))

      (set-window-dedicated-p win t)
      (with-current-buffer buf
        (setq mode-line-format nil)
        (set-window-parameter win 'no-other-window t)
        (erase-buffer)
        (when ctx (insert (cl-reduce (lambda (acc str) (concat acc str)) ctx)))
        (add-face-text-property (point-min) (point-max) '(:inherit window-stool-face) t)
        (fit-window-to-buffer win)))))


(defun window-stool-window--create-ad (&rest _) (window-stool-window--create))

(defun window-stool-window--windmove-up-advice ()
  "Advice to move 'past the context window if there's another window above.
If there isn't (when \"windmove-up\" signals an error), move back down to the original window."
  (when (string= (buffer-name) window-stool-window--buffer-name)
    (condition-case nil
        (windmove-up)
      (error (windmove-down)))))

(defun window-stool-window--windmove-down-advice ()
  "Similar to \"window-stool-window--windmove-up-advice\".
Doesn't need to move back to the original window on error,since context window is always on top."
  (when (string= (buffer-name) window-stool-window--buffer-name)
    (windmove-down)))

(defun window-stool-window--delete (&rest _)
  "Find the window containing the context buffer, and delete it."
  (let ((win (get-buffer-window window-stool-window--buffer-name)))
    (when win (delete-window win))))

(defun window-stool-window--fit-to-buffer ()
  "Find the window containing the context buffer, and fit the window to its contents."
  (let ((win (get-buffer-window window-stool-window--buffer-name)))
    (when win (fit-window-to-buffer win))))

(defun window-stool-window--balance-advice-after (&rest _)
  (window-stool-window--fit-to-buffer))

(defun window-stool-window--balance-advice-before (&rest _)
  (window-stool-window--fit-to-buffer))

(defun window-stool-window--advise-window-functions ()
  "Advise a variety of window and buffer functions for interoperability with the context window."

  ;; When quitting magit on a split frame in doom, the window rebalancing
  ;; would be thrown off because of the window-stool-window.
  ;;
  ;; Killing the window, prior to showing the magit buffer via an advice to
  ;; display-buffer seemed to fix it and should hopefully fix it for other
  ;; issues as well.
  (advice-add 'display-buffer :before #'window-stool-window--delete)

  ;; ensure window stool window doesn't get resized from rebalancing
  ;; odd case where balancing windows on a window with a non window-stool
  ;; buffer displayed, will equalize the other window that does have the
  ;; window-stool-window.
  (advice-add 'balance-windows :before #'window-stool-window--balance-advice-before)
  (advice-add 'balance-windows :after #'window-stool-window--balance-advice-after)

  ;; delete the window and then re-add so we don't affect window enlargement/shrinkage
  (advice-add 'shrink-window :before #'window-stool-window--delete)
  (advice-add 'shrink-window :after #'window-stool-window--create-ad)
  (advice-add 'enlarge-window :before #'window-stool-window--delete)
  (advice-add 'enlarge-window :after #'window-stool-window--create-ad)

  (advice-add 'windmove-up :after #'window-stool-window--windmove-up-advice)
  (advice-add 'windmove-down :after #'window-stool-window--windmove-down-advice)
  (advice-add 'split-window :before #'window-stool-window--delete))

(defun window-stool-window--remove-window-function-advice ()
  (advice-remove 'display-buffer #'window-stool-window--delete)

  (advice-remove 'balance-windows #'window-stool-window--balance-advice-before)
  (advice-remove 'balance-windows #'window-stool-window--balance-advice-after)

  (advice-remove 'shrink-window #'window-stool-window--delete)
  (advice-remove 'shrink-window #'window-stool-window--create)
  (advice-remove 'enlarge-window #'window-stool-window--delete)
  (advice-remove 'enlarge-window #'window-stool-window--create-ad)

  (advice-remove 'windmove-up #'window-stool-window--windmove-up-advice)
  (advice-remove 'windmove-down #'window-stool-window--windmove-down-advice)
  (advice-remove 'split-window #'window-stool-window--delete))

(provide 'window-stool-window)
;;; window-stool-window.el ends here

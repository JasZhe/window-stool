;;; code-context.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Jason Zhen
;;
;; Author: Jason Zhen
;; Maintainer: Jason Zhen
;; Created: December 16, 2023
;; Modified: December 16, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jasonzhen/code-context
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
;;;

;; TODO: something weird when trying to scroll past this function from below
(defun find-non-empty-line-indentation ()
  "Find the first non-empty line to get current \"scope's\" indentation.
Limit it to 2 lines back, since it's unlikely there'll be more than 2 consequtive lines of whitespace"
  (let ((count 0))
    (while (and (= (current-indentation) 0) (< count 2) (= (line-beginning-position) (line-end-position)))
      (setq count (+ count 1))
      (forward-line -1)
      ))
  (current-indentation))

;; TODO: fix this up so we don't get context when we're already at 0 indentation (root level
;; TODO: maybe if we're in prog mode we can be smarter, otherwise we use this dumb way in non-prog mode
(defun code-context--update-1 ()
  (goto-char (window-start))
  (if (= (window-start) (point-min))
      (setq my-context-alist nil)
    (let ((curr-context '())
          (prev-indent (find-non-empty-line-indentation)))
      (if (> prev-indent 0)
          (progn
            (cl-pushnew (buffer-substring-no-properties (line-beginning-position) (line-end-position)) curr-context)
            (while (> (current-indentation) 0)
              (when (< (current-indentation) prev-indent)
                (setq prev-indent (current-indentation))
                (cl-pushnew (buffer-substring-no-properties (line-beginning-position) (line-end-position)) curr-context))
              (forward-line -1)
              (find-non-empty-line-indentation)
              )
            (cl-pushnew (buffer-substring-no-properties (line-beginning-position) (line-end-position)) curr-context)
            (setq my-context-alist curr-context))
        (remove-overlays (point-min) (point-max) 'name 'jason)))))

;; TODO: my context alist rules need to be set better
(setq my-context-alist '())
(setq my-overlay nil)

;; TODO: is it better to keep removing and remaking overlays or have a single one that we move?
(defun code-context-single-overlay (display-start)
  (save-excursion
    (code-context--update-1))
  (remove-overlays (point-min) (point-max) 'name 'jason)
  (when my-context-alist
    (let* ((ol (make-overlay display-start (save-excursion (goto-char display-start) (forward-line) (line-end-position))))
           (context (cl-reduce (lambda (acc str) (concat acc "\n" str)) my-context-alist)))
      (setq my-overlay ol)
      (overlay-put my-overlay 'name 'jason)
      (overlay-put my-overlay 'display (concat context "\n------------context------------\n")))))

(defun code-context-window-scroll-function (_ display-start)
  (when (buffer-file-name) (code-context-single-overlay display-start)))

;;;###autoload
(define-minor-mode code-context-mode
  "Minor mode to show code context based on indentation level within the buffer via overlays"
  :init-value nil
  :lighter " CodeCtx"
  (if code-context-mode
      (add-to-list 'window-scroll-functions #'code-context-window-scroll-function)
    (progn
      (setq window-scroll-functions (remove 'code-context-window-scroll-function window-scroll-functions))
      (remove-overlays (point-min) (point-max) 'name 'jason))))

(provide 'code-context)
;;; code-context.el ends here







































;;;asdsad

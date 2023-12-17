;;; code-context.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Jason Zhen
;;
;; Author: Jason Zhen <jaszhe@gmail.com>
;; Maintainer: Jason Zhen <jaszhe@gmail.com>
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

(setq my-overlay (make-overlay 1 8))
(overlay-put my-overlay 'display "asdad\nadsadsasd\nasasdsadasd\nasddsadsasa\n")

(setq my-overlay (make-overlay (window-start) (save-excursion (goto-char (window-start)) (line-end-position))))
(move-overlay my-overlay (save-excursion (goto-char (window-start)) (line-end-position)) (save-excursion (goto-char (window-start)) (line-end-position)))
;; (delete-overlay my-overlay)
;; (overlay-put my-overlay 'cursor-intangible t)
;; (overlay-put my-overlay 'intangible t)
;;
;;


(setq my-context-alist '())

(defun code-context--update (display-start)
  (save-excursion
    (let ((curr-context '())
          (prev-indent (current-indentation)))
      (goto-char (window-start))
      (while (> (current-indentation) 0)
        (when (< (current-indentation) prev-indent)
          (setq prev-indent (current-indentation))
          (cl-pushnew (buffer-substring-no-properties (line-beginning-position) (line-end-position)) curr-context))
        (previous-line)
        )
      (setq my-context-alist curr-context)
      )
    )
  ;; (move-overlay my-overlay display-start (+ 1 display-start))
  ;; (move-overlay my-overlay (save-excursion (goto-char (window-start)) (forward-line) (line-beginning-position)) (save-excursion (goto-char (window-start)) (forward-line) (line-end-position)))
  ;; (move-overlay my-overlay (window-start) (+ 1 (window-start)))
  (move-overlay my-overlay display-start (save-excursion (goto-char display-start) (line-end-position)))
  ;; (setq-local scroll-margin (length my-context-alist))
  (let ((context (cl-reduce (lambda (acc str) (concat acc "\n" str)) my-context-alist)))
    (message context)
    (if my-context-alist
        (progn
          ;; (setq last-line (last my-context-alist))
          (ignore-errors (overlay-put my-overlay 'display context))
          ;; (overlay-put my-overlay 'display last-line)
          )
      (delete-overlay my-overlay)
      ))
  )

(add-to-list 'window-scroll-functions (lambda (_ display-start) (message "scroll %s" display-start) (ignore-errors (code-context--update display-start))))

(provide 'code-context)
;;; code-context.el ends here

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
;;;

(defun find-previous-non-empty-line ()
  ;; empty body cause we basically just do the re-search-backward as part of the loop
  (while (and (looking-at-p (rx-to-string `(: (* blank) eol)))
              (re-search-backward (rx-to-string `(: (+ any))) nil t))))

(defun get-context-from (pos)
  (goto-char pos)
  (find-previous-non-empty-line)
  (let ((ctx '())
        (prev-indentation (current-indentation)))
    (while (> (current-indentation) 0)
      (forward-line -1)
      (find-previous-non-empty-line)
      (when (< (current-indentation) prev-indentation)
        (setq prev-indentation (current-indentation))
        (cl-pushnew (buffer-substring-no-properties (line-beginning-position) (line-end-position)) ctx)
        )
      )
    ctx))

;; TODO: move the save excursions into one parent save excursion maybe
(defun code-context-single-overlay (display-start)
  (remove-overlays (point-min) (point-max) 'name 'jason)

  (let ((ctx (save-excursion (get-context-from display-start))))
    (when ctx
      (let* (;; when window start is empty line ov-beg is display start
             (display-start-empty-line-p (save-excursion (goto-char display-start) (looking-at-p "^$")))
             (ol-beg-pos display-start ;;(if display-start-empty-line-p (save-excursion (goto-char display-start) (forward-line -1) (line-beginning-position)) display-start)
             )
             ;; when window start is empty line, ov-end is end of line after display-start
             (ol-end-pos (if display-start-empty-line-p (save-excursion (goto-char display-start) (forward-line) (line-end-position)) (save-excursion (goto-char display-start) (line-end-position))))
             (ol (make-overlay ol-beg-pos ol-end-pos))
             ;; we need to add the last line into the overlay cause we cover it with the overlay only if the window start is not empty
             (last-line (if display-start-empty-line-p
                            (save-excursion (goto-char display-start) (forward-line) (buffer-substring (line-beginning-position) (line-end-position)))
                          (save-excursion (goto-char display-start) (buffer-substring (line-beginning-position) (line-end-position)))))
             (context-str-1 (when ctx (cl-reduce (lambda (acc str) (concat acc "\n" str)) ctx :initial-value "")))
             (context-str (if display-start-empty-line-p context-str-1 context-str-1)))
        (setq-local buffer-overlay ol)
        (overlay-put buffer-overlay 'name 'jason)
        (overlay-put buffer-overlay 'display (concat context-str "\n------------context------------\n" last-line))))))

(defun code-context-window-scroll-function (_ display-start)
  (when (buffer-file-name) (code-context-single-overlay (+ 1 display-start))))

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

(defun test-function ()
  (when (buffer-file-name)
    (progn
      (let ((x (buffer-name)))
        (let ((y (buffer-name)))
          (let ((z (buffer-name)))
            (message z)))))))





































;;;asdsad

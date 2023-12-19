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
    (cl-pushnew (buffer-substring-no-properties (line-beginning-position) (line-end-position)) ctx)
    (while (> (current-indentation) 0)
      (forward-line -1)
      (find-previous-non-empty-line)
      (when (< (current-indentation) prev-indentation)
        (setq prev-indentation (current-indentation))
        (cl-pushnew (buffer-substring-no-properties (line-beginning-position) (line-end-position)) ctx)
        )
      )
    ctx))

;x TODO: move the save excursions into one parent save excursion maybe
;; (let ((replaced-char (buffer-substring 1473 1474)))
;;   (setq test-ov (make-overlay 1473 1474))
;;   (overlay-put test-ov 'display (propertize "hello\ngoodbye\nsdad"))
;;   (overlay-put test-ov 'after-string replaced-char)
;;   )
;; (remove-overlays)


(setq prev-ctx nil)
;; issue where we can't keep scrolling if overlay would move cursor outside of scroll margin
;; really only an issue when scrolling up
(defun code-context-single-overlay (display-start)
  (remove-overlays (point-min) (point-max) 'name 'jason)
  (let ((ctx (save-excursion (get-context-from display-start))))
    (let* ((display-start-empty-line-p (save-excursion (goto-char display-start) (looking-at-p "^$")))
           (ol-beg-pos display-start)
           (ol-end-pos (save-excursion (goto-char display-start) (if display-start-empty-line-p (+ 1 display-start) (line-end-position))))
           (ol (make-overlay ol-beg-pos ol-end-pos))
           ;; we need to add the last line into the overlay cause we cover it with the overlay only if the window start is not empty
           (replaced-char (char-to-string (char-after display-start)))
           (context-str-1 (when ctx (cl-reduce (lambda (acc str) (concat acc "\n" str)) ctx)))
           (context-str (if display-start-empty-line-p context-str-1 context-str-1)))

      ;; this only seems to work with post-command-hook
      (when (and ctx (or (eq last-command 'evil-scroll-line-up)
                         (eq last-command 'scroll-down-line)))
        (forward-line (- (+ (min (- (length ctx) (length prev-ctx)) 0) 1))))

      (setq-local buffer-overlay ol)
      (overlay-put buffer-overlay 'name 'jason)
      (overlay-put buffer-overlay 'display context-str)
      )
    (setq prev-ctx ctx)))

(defun code-context-window-scroll-function (_ display-start)
  (let ((display-start-empty-line-p (save-excursion (goto-char display-start) (looking-at-p "^$"))))
    (when (buffer-file-name)
      (code-context-single-overlay display-start)
      )
    )
  )

(advice-add 'redisplay :before (lambda () (code-context-window-scroll-function nil (window-start))))
(advice-remove 'redisplay (lambda () (code-context-window-scroll-function nil (window-start))))

(add-hook 'post-command-hook (lambda () (code-context-window-scroll-function nil (window-start))))

;;;###autoload
(define-minor-mode code-context-mode
  "Minor mode to show code context based on indentation level within the buffer via overlays"
  :init-value nil
  :lighter " CodeCtx"
  (if code-context-mode
      (progn
        (setq previous-window-start (window-start))
        (add-to-list 'window-scroll-functions #'code-context-window-scroll-function))
    (progn
      (setq window-scroll-functions (remove 'code-context-window-scroll-function window-scroll-functions))
      (remove-overlays (point-min) (point-max) 'name 'jason))))

(let ((x 1))
  (progn
    (progn
      (progn
        (progn
          (progn ))))))

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

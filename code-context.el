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

(defun code-context-find-prev-non-empty-line ()
  ;; empty body cause we basically just do the re-search-backward as part of the loop
  (while (and (looking-at-p (rx-to-string `(: (* blank) eol)))
              (re-search-backward (rx-to-string `(: (+ any))) nil t))))

(defun code-context-get-indentation-context-from (pos)
  (goto-char pos)
  (code-context-find-prev-non-empty-line)
  (let ((ctx '())
        (prev-indentation (current-indentation)))
    (let ((ctx-str (concat (buffer-substring (line-beginning-position) (line-end-position)) "\n")))
          (add-face-text-property 0 (length ctx-str) '(:inherit hl-line) t ctx-str)
          (cl-pushnew ctx-str ctx))
    (while (> (current-indentation) 0)
      (forward-line -1)
      (code-context-find-prev-non-empty-line)
      (when (< (current-indentation) prev-indentation)
        (setq prev-indentation (current-indentation))
        (let ((ctx-str (concat (buffer-substring (line-beginning-position) (line-end-position)) "\n")))
          (add-face-text-property 0 (length ctx-str) '(:inherit hl-line) t ctx-str)
          (cl-pushnew ctx-str ctx))
        )
      )
    ctx))

;; issue where we can't keep scrolling if overlay would move cursor outside of scroll margin
;; really only an issue when scrolling up
(defun code-context-single-overlay (display-start)
  (let* ((window-bufs (reduce (lambda (acc win) (push (window-buffer win) acc)) (window-list) :initial-value '()))
        (window-bufs-unique (reduce (lambda (acc win) (cl-pushnew (window-buffer win) acc)) (window-list) :initial-value '()))
        ;; having the same buffer shown in multiple windows gets kinda buggy
        (same-buffer-multiple-windows-p (not (= (length window-bufs) (length window-bufs-unique)))))
    (unless (boundp 'buffer-overlay) (setq-local buffer-overlay (make-overlay 1 1)))
    (if (or (eq display-start (point-min)) same-buffer-multiple-windows-p)
        (delete-overlay buffer-overlay)
      (progn
        (when (and (buffer-file-name) (not (string-match "\\.git" (buffer-file-name))))
          (let ((ctx (save-excursion (funcall code-context-fn display-start))))
            (when (and ctx (> code-context-max-context 0))
              (let* ((end-pos (min (length ctx) code-context-max-context)))
                (if code-context-truncate-from-bottom
                    (setq ctx (cl-subseq ctx 0 end-pos))
                  (setq ctx (cl-subseq ctx (- end-pos))))))
            (let* ((display-start-empty-line-p (save-excursion (goto-char display-start) (or (looking-at-p "^$") (looking-at-p "[[:blank:]]*$"))))
                   (ol-beg-pos display-start)
                   (ol-end-pos (save-excursion (goto-char display-start) (forward-line) (line-end-position)))
                   (covered-line (save-excursion (goto-char display-start) (forward-line) (buffer-substring (line-beginning-position) (line-end-position))))
                   (context-str-1 (when ctx (cl-reduce (lambda (acc str) (concat acc str)) ctx)))
                   (context-str (concat context-str-1 "-------------context-------------\n" covered-line)))

              (when buffer-overlay
                (move-overlay buffer-overlay ol-beg-pos ol-end-pos)
                (overlay-put buffer-overlay 'type 'code-context-buffer-overlay)
                (overlay-put buffer-overlay 'display context-str))
              )
            (setq prev-ctx ctx))))))
  (setq-local prev-window-start (window-start)))

;; this only seems to work with post-command-hook
(defun code-context-scroll-overlay-into-position ()

  (let ((ctx (save-excursion (funcall code-context-fn (window-start)))))
    (when (not (eq (window-start) prev-window-start))
      (when (and ctx (or (eq last-command 'evil-scroll-line-up)
                         (eq last-command 'scroll-down-line)))
        (forward-line (- (+ (min (- (length ctx) (length prev-ctx)) 0) 1)))))))

(defun code-context-window-scroll-function (_ display-start)
  (let ((display-start-empty-line-p (save-excursion (goto-char display-start) (looking-at-p "^$"))))
    (when (and (buffer-file-name) (not git-commit-mode))
      (code-context-single-overlay display-start)
      )
    )
  )

(defcustom code-context-use-overlays t
  "whether or not to use overlays or dedicated window"
  :type '(boolean))

(defcustom code-context-truncate-from-bottom t
  "If code-context-max-context is not 0, choose whether to truncate context from the top or the bottom"
  :type '(boolean))

(defcustom code-context-max-context 5
  "Max context lines to display so we don't take up too much space or 0 to show all context"
  :type '(natnum))


(defcustom code-context-major-mode-functions-alist '((nil . code-context-get-indentation-context-from))
  "A list of (major-mode . function) for specialized context functions to use in major modes.
Each function should take one argument, the point to search from.
Otherwise defautls to the indentation based context function."
  :type '(alist :key-type symbol :value-type function))


(defvar code-context-fn nil
  "Function that returns the context in a buffer from point")

;;;###autoload
(define-minor-mode code-context-mode
  "Minor mode to show code context based on indentation level within the buffer via overlays"
  :lighter " CodeCtx"
  (if code-context-mode
      (progn (setq-local prev-ctx nil)
             (setq-local prev-window-start (window-start))
             (remove-overlays (point-min) (point-max) 'type 'code-context-buffer-overlay)

             (setq code-context-fn (cdr (or (assq major-mode code-context-major-mode-functions-alist)
                                            (assq nil code-context-major-mode-functions-alist))))
             (if code-context-use-overlays
                 (progn
                   (code-context-window-delete nil)
                   (add-hook 'post-command-hook (lambda () (code-context-scroll-overlay-into-position)) nil t)
                   (add-to-list 'window-scroll-functions #'code-context-window-scroll-function))
               (progn
                 (add-hook 'post-command-hook #'code-context-window-create nil t)
                 (advise-window-functions)))
             )
    (progn
      (remove-hook 'post-command-hook (lambda () (code-context-scroll-overlay-into-position)) t)
      (setq window-scroll-functions (remove #'code-context-window-scroll-function window-scroll-functions))
      (remove-hook 'post-command-hook #'code-context-window-create t)
      (code-context-window-delete nil)
      (remove-window-function-advice)
      (remove-overlays (point-min) (point-max) 'type 'code-context-buffer-overlay))))

(provide 'code-context)
(add-hook 'prog-mode-hook #'code-context-mode)
;;; code-context.el ends here

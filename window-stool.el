;;; window-stool.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Jason Zhen
;;
;; Author: Jason Zhen
;; Maintainer: Jason Zhen
;; Created: December 16, 2023
;; Modified: December 16, 2023
;; Version: 0.0.1
;; Keywords: context overlay
;; Homepage: https://github.com/jasonzhen/window-stool
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary
;; Like a little "stool" for your window viewing needs
;;
;;  Description
;; Uses an overlay that moves when the window is scrolled to take over the first
;; two lines (one line gets a little buggy when it comes to scrolling) to show
;; indentation based (default but can define other methods) buffer context
;;
;; Works best if you have some sort of scroll margin, cause editing the overlay text
;; can get wonky.
;;
;;; Code:
;;;

(defun window-stool-find-prev-non-empty-line ()
  ;; empty body cause we basically just do the re-search-backward as part of the loop
  (while (and (looking-at-p (rx-to-string `(: (* blank) eol)))
              (re-search-backward (rx-to-string `(: (+ any))) nil t))))

(defun window-stool-get-indentation-context-from (pos)
  (goto-char pos)
  (window-stool-find-prev-non-empty-line)
  (let ((ctx '())
        (prev-indentation (current-indentation)))
    (let ((ctx-str (concat (buffer-substring (line-beginning-position) (line-end-position)) "\n")))
          (add-face-text-property 0 (length ctx-str) '(:inherit hl-line) t ctx-str)
          (cl-pushnew ctx-str ctx))
    (while (> (current-indentation) 0)
      (forward-line -1)
      (window-stool-find-prev-non-empty-line)
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
(defun window-stool-single-overlay (display-start)
  (let* ((window-bufs (cl-reduce (lambda (acc win) (push (window-buffer win) acc)) (window-list) :initial-value '()))
        (window-bufs-unique (cl-reduce (lambda (acc win) (cl-pushnew (window-buffer win) acc)) (window-list) :initial-value '()))
        ;; having the same buffer shown in multiple windows gets kinda buggy
        (same-buffer-multiple-windows-p (not (= (length window-bufs) (length window-bufs-unique)))))
    (unless (boundp 'window-stool-overlay) (setq-local window-stool-overlay (make-overlay 1 1)))
    (if (or (eq display-start (point-min)) same-buffer-multiple-windows-p)
        (delete-overlay window-stool-overlay)
      (progn
        (when (and (buffer-file-name) (not (string-match "\\.git" (buffer-file-name))))
          (let ((ctx (save-excursion (funcall window-stool-fn display-start))))
            (when (and ctx (> window-stool-max-context 0))
              (let* ((end-pos (min (length ctx) window-stool-max-context)))
                (if window-stool-truncate-from-bottom
                    (setq ctx (cl-subseq ctx 0 end-pos))
                  (setq ctx (cl-subseq ctx (- end-pos))))))
            (let* ((display-start-empty-line-p (save-excursion (goto-char display-start) (or (looking-at-p "^$") (looking-at-p "[[:blank:]]*$"))))
                   (ol-beg-pos display-start)
                   (ol-end-pos (save-excursion (goto-char display-start) (forward-line) (line-end-position)))
                   (covered-line (save-excursion (goto-char display-start) (forward-line) (buffer-substring (line-beginning-position) (line-end-position))))
                   (context-str-1 (when ctx (cl-reduce (lambda (acc str) (concat acc str)) ctx)))
                   (context-str (concat context-str-1 covered-line)))

              (when window-stool-overlay
                (move-overlay window-stool-overlay ol-beg-pos ol-end-pos)
                (overlay-put window-stool-overlay 'type 'window-stool-window-stool-overlay)
                (overlay-put window-stool-overlay 'display context-str))
              )
            (setq prev-ctx ctx))))))
  (setq-local prev-window-start (window-start)))

;; this only seems to work with post-command-hook
(defun window-stool-scroll-overlay-into-position ()

  (let ((ctx (save-excursion (funcall window-stool-fn (window-start)))))
    (when (not (eq (window-start) prev-window-start))
      (when (and ctx (or (eq last-command 'evil-scroll-line-up)
                         (eq last-command 'scroll-down-line)))
        (forward-line (- (+ (min (- (length ctx) (length prev-ctx)) 0) 1)))))))

(defun window-stool-window-scroll-function (_ display-start)
  (let ((display-start-empty-line-p (save-excursion (goto-char display-start) (looking-at-p "^$"))))
    (when (and (buffer-file-name) (or (not (boundp 'git-commit-mode)) (not git-commit-mode)))
      (window-stool-single-overlay display-start)
      )
    )
  )

(defcustom window-stool-use-overlays t
  "whether or not to use overlays or dedicated window (EXPERIMENTAL)"
  :type '(boolean))

(defcustom window-stool-truncate-from-bottom t
  "If window-stool-max-context is not 0, choose whether to truncate context from the top or the bottom"
  :type '(boolean))

;; TODO ability to specify x lines of "top" context and y lines of context from the "bottom"
;; This way we can always have the defun showing, but showing the inner most other context
(defcustom window-stool-max-context 5
  "Max context lines to display so we don't take up too much space or 0 to show all context"
  :type '(natnum))


(defcustom window-stool-major-mode-functions-alist '((nil . window-stool-get-indentation-context-from))
  "A list of (major-mode . function) for specialized context functions to use in major modes.
Each function should take one argument, the point to search from.
Otherwise defautls to the indentation based context function."
  :type '(alist :key-type symbol :value-type function))


(defvar window-stool-fn nil
  "Function that returns the context in a buffer from point")

;;;###autoload
(define-minor-mode window-stool-mode
  "Minor mode to show code context based on indentation level within the buffer via overlays"
  :lighter " WinStool"
  (if window-stool-mode
      (progn (setq-local prev-ctx nil)
             (setq-local prev-window-start (window-start))
             (remove-overlays (point-min) (point-max) 'type 'window-stool-buffer-overlay)
	     
             (setq window-stool-fn (cdr (or (assq major-mode window-stool-major-mode-functions-alist)
                                            (assq nil window-stool-major-mode-functions-alist))))

	     ;; TODO set scroll margin to be at least max-context if curr scroll margin less
             (if window-stool-use-overlays
                 (progn
                   (window-stool-window-delete nil)
                   (add-hook 'post-command-hook (lambda () (window-stool-scroll-overlay-into-position)) nil t)
                   (add-to-list 'window-scroll-functions #'window-stool-window-scroll-function))
               (progn
                 (add-hook 'post-command-hook #'window-stool-window-create nil t)
                 (advise-window-functions)))
             )
    (progn
      (remove-hook 'post-command-hook (lambda () (window-stool-scroll-overlay-into-position)) t)
      (setq window-scroll-functions (remove #'window-stool-window-scroll-function window-scroll-functions))
      (remove-hook 'post-command-hook #'window-stool-window-create t)
      (window-stool-window-delete nil)
      (remove-window-function-advice)
      (remove-overlays (point-min) (point-max) 'type 'window-stool-buffer-overlay))))

(provide 'window-stool)
(add-hook 'prog-mode-hook #'window-stool-mode)
;;; window-stool.el ends here

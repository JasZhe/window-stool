;;; window-stool.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Jason Zhen
;;
;; Author: Jason Zhen <jaszhe@gmail.com>
;; Maintainer: Jason Zhen <jaszhe@gmail.com>
;; Created: December 16, 2023
;; Modified: December 16, 2023
;; Version: 0.0.1
;; Keywords: context overlay
;; Homepage: https://github.com/jasonzhen/window-stool
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
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
;; Will definitely affect scrolling performance.
;;
;;; Code:
;;;
(require 'cl-lib)
(require 'window-stool-window)

(defgroup window-stool nil
  "A minor mode for providing some additional buffer context via overlays."
  :group 'tools)

(defface window-stool-face
  '((t (:inherit fringe :extend t)))
  "Face for window stool overlay background.
This will be ADDED to the context string's existing buffer font locking."
  :group window-stool-mode)

(defcustom window-stool-use-overlays t
  "Whether or not to use overlays or dedicated window (EXPERIMENTAL)."
  :type '(boolean))

(defcustom window-stool-n-from-top 1
  "Number of lines of context to keep from the outermost context list.
i.e. suppose we have
\(defun foo \(\)
  \(progn ;; one
     \(progn ;; two
        \(progn ;; three\)\)\)\)

Setting this to 2, would take the defun and the first progn.
Similar behavior for \"window-stool-n-from-bottom\".
Except we take from the end (the second and third progn).
Setting both of these to zero keeps all context."
  :type '(natnum))

(defcustom window-stool-n-from-bottom 2
  "Number of lines of context to keep from the innermost context list.
See: \"window-stool-n-from-top\"."
  :type '(natnum))

(defcustom window-stool-major-mode-functions-alist '((nil . window-stool-get-indentation-context-from))
  "A list of (major-mode . function).
Each function should take one argument, the point to search from.
Each function should return a list of strings.
Each string will be displayed in the overlay from top to bottom.
Strings should end with newlines.
Defaults to indentation based context function."
  :type '(alist :key-type symbol :value-type function))

(defvar window-stool-fn nil
  "Function that returns the context in a buffer from point.")

(defun window-stool-find-prev-non-empty-line ()
  "Find non-empty line above from point."
  ;; empty body cause we basically just do the re-search-backward as part of the loop
  (while (and (looking-at-p (rx-to-string `(: (* blank) eol)))
              (re-search-backward (rx-to-string `(: (+ any))) nil t))))

(defun window-stool-get-indentation-context-from (pos)
  "Get indentation based context from POS."
  (goto-char pos)
  (window-stool-find-prev-non-empty-line)
  (let ((ctx '())
        (prev-indentation (current-indentation)))
    (let ((ctx-str (concat (buffer-substring (line-beginning-position) (line-end-position)) "\n")))
      ;; Need to add the current line that pos is on as well cause there's some weird issues
      ;; if we have an empty context
      (add-face-text-property 0 (length ctx-str) '(:inherit window-stool-face) t ctx-str)
      (cl-pushnew ctx-str ctx))
    (while (> (current-indentation) 0)
      (forward-line -1)
      (window-stool-find-prev-non-empty-line)
      (when (< (current-indentation) prev-indentation)
        (setq prev-indentation (current-indentation))
        (let ((ctx-str (concat (buffer-substring (line-beginning-position) (line-end-position)) "\n")))
          (add-face-text-property 0 (length ctx-str) '(:inherit window-stool-face) t ctx-str)
          (cl-pushnew ctx-str ctx))
        )
      )
    ctx))

(defun window-stool--truncate-context (ctx)
  "Truncates CTX.
See: doc for \"window-stool-n-from-top\".
Just returns CTX if both are 0."
  (if (and (> window-stool-n-from-top 0) (> window-stool-n-from-bottom 0))
      (let* ((from-top (min (length ctx) window-stool-n-from-top))
	     (top-ctx (cl-subseq ctx 0 from-top))
	     (ctx-1 (nthcdr from-top ctx))
	     (from-bottom (min (length ctx-1) window-stool-n-from-bottom))
	     (bottom-ctx (cl-subseq ctx-1 (- from-bottom))))
	(append top-ctx bottom-ctx))
    ctx
    )
  )

(defun window-stool-single-overlay (display-start)
  "Create/move an overlay to show buffer context above DISPLAY-START.
Single overlay per buffer.
Contents of the overlay is based on the results of \"window-stool-fn\"."

  ;; Issue with having multiple windows displaying the same buffer since now there's multiple "window starts" which make it difficult to deal with. Simpler to temporarily delete the overlays until only a single window shows the buffer for now.
  (let* ((window-bufs (cl-reduce (lambda (acc win) (push (window-buffer win) acc)) (window-list) :initial-value '()))
        (window-bufs-unique (cl-reduce (lambda (acc win) (cl-pushnew (window-buffer win) acc)) (window-list) :initial-value '()))
        (same-buffer-multiple-windows-p (not (= (length window-bufs) (length window-bufs-unique)))))
    (unless (boundp 'window-stool-overlay) (setq-local window-stool-overlay (make-overlay 1 1)))
    (if (or (eq display-start (point-min)) same-buffer-multiple-windows-p)
        (delete-overlay window-stool-overlay)
      (progn
	;; Some git operations i.e. commit/rebase open up a buffer that we can edit which is based a temporary file in the .git directory. Most of the time I don't really want the overlay in those buffers so I've opted to disable them here via this simple heuristic.
        (when (and (buffer-file-name) (not (string-match "\\.git" (buffer-file-name))))
          (let* ((ctx-1 (save-excursion (funcall window-stool-fn display-start)))
		 (ctx (window-stool--truncate-context ctx-1)))
            (let* ((ol-beg-pos display-start)
                   (ol-end-pos (save-excursion (goto-char display-start) (forward-line) (line-end-position)))
		   ;; There's some bugginess if we don't have end-pos be on the next line, cause depending on the order of operations we might scroll past our overlay after redisplay.
		   ;; The solution here is to make the overlay 2 lines and just show the "covered" second line as part of the overlay
                   (covered-line (save-excursion (goto-char display-start) (forward-line) (buffer-substring (line-beginning-position) (line-end-position))))
                   (context-str-1 (when ctx (cl-reduce (lambda (acc str) (concat acc str)) ctx)))
                   (context-str (concat context-str-1 covered-line)))

              (when window-stool-overlay
                (move-overlay window-stool-overlay ol-beg-pos ol-end-pos)
                (overlay-put window-stool-overlay 'type 'window-stool--buffer-overlay)
                (overlay-put window-stool-overlay 'display context-str))
              )
            (setq window-stool--prev-ctx ctx))))))
  (setq-local window-stool--prev-window-start (window-start)))

(defun window-stool--scroll-overlay-into-position ()
  "Fixes some bugginess with scrolling getting stuck when the overlay large."
  (let* ((ctx-1 (save-excursion (funcall window-stool-fn (window-start))))
	(ctx (window-stool--truncate-context ctx-1)))
    (when (not (eq (window-start) window-stool--prev-window-start))
      (when (and ctx (or (eq last-command 'evil-scroll-line-up)
			 (eq last-command 'viper-scroll-down-one)
                         (eq last-command 'scroll-down-line)))
        (forward-line (- (+ (min (- (length ctx) (length window-stool--prev-ctx)) 0) 1)))))))

(defun window-stool--scroll-function (_ display-start)
  "Convenience wrapper for \"window-scroll-functions\".
Only requires use of DISPLAY-START.
See: \"window-stool-single-overlay\"."
  (when (and (buffer-file-name)
	     (or (not (boundp 'git-commit-mode))
		 (not git-commit-mode)))
    (window-stool-single-overlay display-start))
  )

;;;###autoload
(define-minor-mode window-stool-mode
  "Minor mode to show buffer context.
Get a glimpse of the buffer contents above your current window psoition.
Like a stool to peek a little higher than you could normally.

Uses overlays by default and attaches to \"post-command-hook\".
CAUTION: This can have some major performance impact on scrolling.

EXPERIMENTAL: alternative option to use a pseudo-dedicated window.
See: \"window-stool-use-overlays\""
  :lighter " WinStool"
  (if window-stool-mode
      (progn (setq-local window-stool--prev-ctx nil)
             (setq-local window-stool--prev-window-start (window-start))
             (remove-overlays (point-min) (point-max) 'type 'window-stool--buffer-overlay)

	     
             (setq window-stool-fn (cdr (or (assq major-mode window-stool-major-mode-functions-alist)
                                            (assq nil window-stool-major-mode-functions-alist))))

	     ;; set buffer local scroll margin to avoid strange behavior when putting point into the overlay itself
	     ;; since the overlay encompasses a lot less real text than the virtual text it actually shows
	     (when (< scroll-margin (+ window-stool-n-from-top window-stool-n-from-bottom))
	       (setq-local scroll-margin (+ 1 window-stool-n-from-top window-stool-n-from-bottom)))
	     
             (if window-stool-use-overlays
                 (progn
                   (window-stool-window--delete nil)
                   (add-hook 'post-command-hook (lambda () (window-stool--scroll-overlay-into-position)) nil t)
                   (add-to-list 'window-scroll-functions #'window-stool--scroll-function))
               (progn
                 (add-hook 'post-command-hook #'window-stool-window--create nil t)
                 (window-stool-window--advise-window-functions)))
             )
    (progn (remove-hook 'post-command-hook (lambda () (window-stool--scroll-overlay-into-position)) t)
	   (kill-local-variable 'scroll-margin)
	   (setq window-scroll-functions (remove #'window-stool--scroll-function window-scroll-functions))
	   (remove-hook 'post-command-hook #'window-stool-window--create t)
	   (window-stool-window--delete nil)
	   (window-stool-window--remove-window-function-advice)
	   (remove-overlays (point-min) (point-max) 'type 'window-stool-buffer-overlay))))

(provide 'window-stool)
;;; window-stool.el ends here

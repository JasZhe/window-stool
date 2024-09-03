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
;; Package-Requires: ((emacs "27.1"))
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
(require 'org)
(require 'timer)

(defgroup window-stool nil
  "A minor mode for providing some additional buffer context via overlays."
  :group 'tools)

(defface window-stool-face
  '((t (:inherit fringe :extend t)))
  "Face for window stool overlay background.
This will be ADDED to the context string's existing buffer font locking."
  :group 'window-stool)

(defcustom window-stool-use-overlays t
  "Whether or not to use overlays or dedicated window.
It's recommended to disable \"window-divider-mode\" if this is nil."
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
Setting both of these to zero keeps all context.

This needs to be increased to one more than what you would have
normally for the non overlay version."
  :type '(natnum))

(defcustom window-stool-n-from-bottom 2
  "Number of lines of context to keep from the innermost context list.
See: \"window-stool-n-from-top\"."
  :type '(natnum))

(defcustom window-stool-major-mode-functions-alist
  '((org-mode . window-stool-get-org-header-context)
    (nil . window-stool-get-indentation-context-from))
  "A list of (major-mode . function).
Each function should take one argument, the point to search from.
Each function should return a list of strings.
Each string will be displayed in the overlay from top to bottom.
Strings should end with newlines.
Defaults to indentation based context function."
  :type '(alist :key-type symbol :value-type function))

(defcustom window-stool-major-mode-valid-indentation-ctx-regex '((nil .".*[[:alnum:]][[:alnum:]].*"))
  "A regex for valid context lines for the default indentation based context defun.
The default is any string with at least two alphanumeric characters.
This way, we avoid showing lines of only symbols like parentheses.
Example in SQL:

CREATE TABLE xyz
(
  blah...

We want to show \"CREATE TABLE xyz\" instead of ( as the upper context here."
  :type '(alist :key-type symbol :value-type regexp))

(defvar window-stool-fn #'ignore
  "Function that returns the context in a buffer from point.")

(defconst window-stool--min-height 20
  "Minimum height (arbitrarily chosen) that a window needs to have for context to be displayed.
This is a hack to prevent some issues with resizing a window causing Emacs to freeze in the redisplay code.")

(defconst window-stool--min-width 50
  "Minimum width (arbitrarily chosen) that a window needs to have for context to be displayed.
This is a hack to prevent some issues with resizing a window causing Emacs to freeze in the redisplay code.")

(defun window-stool-get-org-header-context (pos)
  "Get org header contexts from POS.
Will move point so caller should call \"save-excursion\"."
  (goto-char pos)
  (when (not (org-before-first-heading-p))

    (outline-back-to-heading)

    ;; When indent mode is on, display-start for the overlay is indented. The issue is then
    ;; the first line of the context will match the indent level of org, but the rest of the
    ;; headings will have their normal indents.
    ;; Instead we propertize manually by grabbing the org-level-faces manually
    (let* ((ctx '())
           (ctx-fn (lambda ()
                     (concat
                      (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))
                      "\n")))
           (ctx-str (propertize
                     (funcall ctx-fn)
                     'face
                     (nth (1- (nth 0 (org-heading-components))) org-level-faces))))
      (cl-pushnew ctx-str ctx)
      (while (> (org-current-level) 1)
        (outline-up-heading 1)
        (let ((ctx-str (propertize
                        (funcall ctx-fn)
                        'face
                        (nth (1- (nth 0 (org-heading-components))) org-level-faces))))
          (cl-pushnew ctx-str ctx)
          )
        )
      ctx)))

(defun window-stool-find-prev-non-empty-line ()
  "Find non-empty line above from point."
  ;; keep going back even further until we hit a "useful" line for context: at least 2 alphanumeric characters
  ;; empty body cause we basically just do the re-search-backward as part of the loop
  (while (and (or (looking-at-p (rx-to-string `(: (* blank) eol)))
                  (not (looking-at-p window-stool-valid-indentation-ctx-regex)))
              (re-search-backward (rx-to-string `(: bol (+ any))) nil t))))

(defun window-stool-get-indentation-context-from (pos)
  "Get indentation based context from POS.
Returns a list of fontified strings with newlines at the end.
Strings will also inherit props of \"window-stool-face\".
Will move point so caller should call \"save-excursion\"."
  (goto-char pos)
  (window-stool-find-prev-non-empty-line)
  (let* ((ctx '())
         (prev-indentation (current-indentation))
         (ctx-fn (lambda ()
                   (concat
                    (buffer-substring
                     (line-beginning-position)
                     (line-end-position))
                    "\n")))
         (ctx-str (funcall ctx-fn)))
    ;; Need to add the current line that pos is on as well cause there's some weird issues
    ;; if we have an empty context
    (cl-pushnew ctx-str ctx)
    (while (> (current-indentation) 0)
      (forward-line -1)
      (window-stool-find-prev-non-empty-line)
      (when (< (current-indentation) prev-indentation)
        (setq prev-indentation (current-indentation))
        (let ((ctx-str (funcall ctx-fn)))
          (cl-pushnew ctx-str ctx))
        )
      )
    ;; need the forward char so we ensure we go back to the beg of current defun
    ;; and not previous defun
    (when (fboundp 'beginning-of-defun)
      (forward-line)
      (beginning-of-defun)
      (let ((ctx-str (funcall ctx-fn)))
        (when (not (string-equal ctx-str (cl-first ctx)))
          (cl-pushnew ctx-str ctx))))
    ctx))

(defun window-stool--truncate-context (ctx)
  "Truncates CTX.
See: doc for \"window-stool-n-from-top\".
Just returns CTX if both are 0."
  (if (or (> window-stool-n-from-top 0) (> window-stool-n-from-bottom 0))
      (let* ((from-top (min (length ctx) window-stool-n-from-top))
             (top-ctx (cl-subseq ctx 0 from-top))
             (ctx-1 (nthcdr from-top ctx))
             (from-bottom (min (length ctx-1) window-stool-n-from-bottom))
             (bottom-ctx (when (> from-bottom 0) (cl-subseq ctx-1 (- from-bottom)))))
        (append top-ctx bottom-ctx))
    ctx))

(defun window-stool--windows-displaying-buf (buf)
  (cl-reduce
   (lambda (acc win)
     (if (eq (window-buffer win) buf)
         (push win acc)
       acc))
   (window-list)
   :initial-value '()))

(defun window-stool--upper-window-displaying-buf (buf)
  "Find the window showing \"buf\" with the smallest display-start.
In other words, the window that shows earlier contents of the buffer.
Return a cons cell of the window with its \"window-start\" value."
  (cl-reduce
   (lambda (acc win)
     (if (eq (window-buffer win) buf)
         (let ((win-start (window-start win)))
           (if (< win-start (cdr acc))
               (cons win win-start)
             acc))
       acc))
   (window-stool--windows-displaying-buf buf)
   :initial-value (cons nil most-positive-fixnum)))

(defvar-local window-stool-overlays '()
  "List of overlays, each representing one context line.")

(defvar-local window-stool--prev-window-start nil
  "The previous window-start. So we don't run the overlay creation unnecessarily.")

(defvar-local window-stool--prev-ctx nil)
(defvar-local window-stool--prev-indentation 0)

(defcustom window-stool-ignore-buffer-regexps nil
  "List of buffer regexps to disable window-stool overlay on.
Different from `window-stool-ignore-file-regexps'"
  :type '(repeat regexp))

;; Some git operations i.e. commit/rebase open up a buffer that we can edit
;; which is based a temporary file in the .git directory.
;; Most of the time I don't really want the overlay in those buffers so
;; I've opted to disable them here via regexps.
(defcustom window-stool-ignore-file-regexps '("\\.git")
  "List of file name regexps to disable window-stool overlay on.
Different from `window-stool-ignore-buffer-regexps'"
  :type '(repeat regexp))

(defvar-local fix-scrolling-past-visually-split-lines nil)

(defun window-stool-display-context (window display-start)
  "Get the code from the current window position.
Then display each line as a separate overlay at the top of the window.
Should be used as a window-scroll-function taking WINDOW and DISPLAY-START."
    (unless (= (length window-stool-overlays) (+ window-stool-n-from-top window-stool-n-from-bottom))
      (setq-local window-stool-overlays
                  (cl-loop repeat (+ window-stool-n-from-top window-stool-n-from-bottom)
                           collect (make-overlay 1 1))))
    ;; Some git operations i.e. commit/rebase open up a buffer that we can edit which is based a temporary file in the .git directory.
    ;; Most of the time I don't really want the overlay in those buffers so I've opted to disable them here via this simple heuristic.
    (unless (or
             (cl-find-if (lambda (r) (and buffer-file-name (string-match r buffer-file-name)))
                         window-stool-ignore-file-regexps)
             (cl-find-if (lambda (r) (string-match r (buffer-name)))
                         window-stool-ignore-file-regexps))
      (let* ((ctx-1 (save-excursion (funcall window-stool-fn display-start)))
             (ctx (window-stool--truncate-context ctx-1))
             (line 0))
        (when ctx
          (dolist (c ctx)
            (setq c (string-trim-right c))
            (let* ((truncated (truncate-string-to-width c (1- (window-size window t)) 0 nil ""))
                   (truncated-respecting-word-boundaries
                    (if (string= truncated c) ;; this means we didn't need to truncate
                        truncated
                      (truncate-string-to-width truncated
                                                (- (length truncated) (1+ (string-match "[[:space:]]" (reverse truncated))))
                                                0 nil ""))))

              (setq c truncated-respecting-word-boundaries)
              (let* ((beg (save-excursion
                            (goto-char display-start)
                            (line-move line)
                            (line-beginning-position)))
                     (add-newline nil)
                     (end (save-excursion (goto-char beg) (if (= beg (line-end-position))
                                                              (progn (setq add-newline t) (1+ beg))
                                                            (line-end-position)
                                                            )))
                     (ov (nth line window-stool-overlays)))
                ;; this fixes odd behavior when there's empty lines
                ;; on an empty line, end would equal to beginning, an overlay with beg=end doesn't show
                ;; so we have to go to the next character, which overwrites a real "\n" so we have to manaully
                ;; add it into the overlay. Easy to test the behavior with a dummy overlay
                (when add-newline (setq c (concat c "\n")))

                (add-face-text-property 0 (length c) '(:inherit window-stool-face) t c)
                (move-overlay ov beg end)
                (overlay-put ov 'type 'window-stool--buffer-overlay)
                (overlay-put ov 'priority 0)
                (overlay-put ov 'display c)
                )
                (setq line (1+ line))
              ))
          (cl-loop for i from line to (length window-stool-overlays)
                   do
                   (let ((ov (nth i window-stool-overlays)))
                     (when (overlayp ov) (delete-overlay ov))))
          )
        (setq-local fix-scrolling-past-visually-split-lines (save-excursion
                 (goto-char display-start)
                 (line-move line)
                 (let ((st (buffer-substring (line-beginning-position) (line-end-position))))
                   ;; (message "%d/%d: %s" (length st) (window-width window) st)
                   )
                 ;; TODO: need to figure out a better heuristic for this i.e. doom's truncation stuff
                 (>= (- (line-end-position) (line-beginning-position)) (- (window-width window) 10))
                 ))
        (setq window-stool--prev-ctx ctx)
        ))
  (setq-local window-stool--prev-window-start (window-start))
  )

(defun window-stool--scroll-overlay-into-position ()
  "Fixes some bugginess with scrolling getting stuck when the overlay large."
  (when fix-scrolling-past-visually-split-lines (forward-line 2))

  ;; don't need this stuff
  (when (and nil (> (window-size (selected-window)) window-stool--min-height)
             (> (window-size (selected-window) t) window-stool--min-width)
             (not (eq (window-start) window-stool--prev-window-start)) (buffer-file-name))
    (let* ((ctx-1 (save-excursion (funcall window-stool-fn (window-start))))
           (ctx (window-stool--truncate-context ctx-1)))
      (ignore-errors
        (when (and ctx (or (eq last-command 'evil-scroll-line-up)
                           (eq last-command 'viper-scroll-down-one)
                           (eq last-command 'scroll-down-line)))
          (forward-visible-line (- (+ (min (- (length ctx) (length window-stool--prev-ctx)) 0) 1)))

          ;; So we don't need to double scroll when window start is in the middle of a visual line split
          (when (= (save-excursion
                     (goto-char (window-start))
                     (line-beginning-position))
                   (save-excursion
                     (goto-char (window-start))
                     (line-move-visual -1 t)
                     (line-beginning-position)))
            (scroll-down-line)))))))

(defun window-stool--scroll-function (window display-start)
  "Convenience wrapper for \"window-scroll-functions\".
Only requires use of DISPLAY-START.
See: `window-stool-display-context'."
  (when (and (buffer-file-name)
             (or (not (boundp 'git-commit-mode))
                 (not git-commit-mode)))

    ;; for org mode, if we hide the font decoration symbols for instance:
    ;; *This is bold* then display-start would point to the "T" instead of
    ;; the first "*" and (scroll-down 1) would complain about beginning of buffer
    (window-stool-display-context window (save-excursion (goto-char display-start) (line-beginning-position)))))

(defun window-stool--selection-change-function (frame-or-window)
  (if (and (windowp frame-or-window) (window-live-p frame-or-window))
      (save-window-excursion
        (select-window frame-or-window)
        (window-stool--scroll-function frame-or-window (window-start frame-or-window)))
    (let ((prev-window (and (framep frame-or-window) (frame-old-selected-window frame-or-window))))
      (when (and prev-window (window-live-p prev-window))
        (save-window-excursion
          (select-window prev-window)
          (when (not (eq window-stool-fn #'ignore))
            (window-stool--scroll-function prev-window (window-start prev-window))))))))

(defvar window-stool-timer nil
  "Idle timer used to reset the window-stool overlay if for some reason, it gets out of position
and the normal mechanism for repositioning it doesn't run (via \"window-scroll-functions\").")

(defvar window-stool-buffer-list '()
  "List of buffers, window-stool is enabled in.")

(defun window-stool-idle-fn ()
  "Function to be used in an idle timer.
Re-positions the window-stool overlay if it gets out of position.

Cancels \"window-stool-timer\" if \"window-stool-buffer-list\" is empty."
  (save-window-excursion
    (dolist (win (window-list))
      (select-window win t)
      (when (and (boundp 'window-stool-mode)
                 window-stool-mode
                 (not (eq window-stool-fn #'ignore))
                 (not (cl-remove-if-not
                       (lambda (o) (eq (overlay-get o 'type) 'window-stool--buffer-overlay))
                       (overlays-at (window-start)))))
        (window-stool--scroll-function (selected-window) (window-start)))))
  (when (= (length window-stool-buffer-list) 0)
    (cancel-timer window-stool-timer)
    (setq window-stool-timer nil)))

(defun window-stool--window-resize-before-advice (&rest _)
  "Advice to prevent a Emacs hanging when windows are resized with the window stool overlay."
  (dolist (win (window-list))
    (with-current-buffer (window-buffer win)
      (ignore-errors (delete-overlay window-stool-overlay)))))

(defun window-stool--window-resize-after-advice (&rest _)
  "Advice to rebuild the overlays after window resizing."
  (dolist (window (window-list))
    (with-current-buffer (window-buffer window)
      (when (and (boundp 'window-stool-mode)
                 window-stool-mode
                 (not (eq window-stool-fn #'ignore)))
                 (window-stool-display-context window (save-excursion (goto-char (window-start window)) (line-beginning-position)))))))

;;;###autoload
(define-minor-mode window-stool-mode
  "Minor mode to show buffer context.
Get a glimpse of the buffer contents above your current window psoition.
Like a stool to peek a little higher than you could normally.

Uses overlays by default and attaches to \"post-command-hook\".
CAUTION: This can have some major performance impact on scrolling.

Alternative option to use a pseudo-dedicated window.
See: \"window-stool-use-overlays\""
  :lighter " WinStool"
  :group 'window-stool
  (if window-stool-mode
      (progn (setq-local window-stool--prev-ctx nil)
             (setq-local window-stool--prev-window-start (window-start))
             (remove-overlays (point-min) (point-max) 'type 'window-stool--buffer-overlay)

             (setq-local window-stool-fn
                         (cdr (or (assq major-mode window-stool-major-mode-functions-alist)
                                  (assq nil window-stool-major-mode-functions-alist))))
             (setq-local window-stool-valid-indentation-ctx-regex
                         (cdr (or (assq major-mode window-stool-major-mode-valid-indentation-ctx-regex)
                                  (assq nil window-stool-major-mode-valid-indentation-ctx-regex))))

             ;; set buffer local scroll margin to avoid strange behavior when putting point into the overlay itself
             ;; since the overlay encompasses a lot less real text than the virtual text it actually shows
             (when (< scroll-margin (+ window-stool-n-from-top window-stool-n-from-bottom))
               (setq-local scroll-margin (+ 1 window-stool-n-from-top window-stool-n-from-bottom)))

             (if window-stool-use-overlays
                 (progn
                   ;; turn off the non-overlay stuff if they're enabled
                   (window-stool-window--delete)
                   (remove-hook 'post-command-hook #'window-stool-window--create)
                   (window-stool-window--remove-window-function-advice)

                   (advice-add #'window-resize :before #'window-stool--window-resize-before-advice)
                   (advice-add #'window-resize :after #'window-stool--window-resize-after-advice)

                   (add-hook 'post-command-hook #'window-stool--scroll-overlay-into-position nil t)

                   ;; TODO: Temporarily remove until we fix the mutli window setup
                   ;; (unless (timerp window-stool-timer)
                   ;;   (setq window-stool-timer (run-with-idle-timer 0.5 t #'window-stool-idle-fn)))

                   (when (buffer-file-name)
                     (cl-pushnew (current-buffer) window-stool-buffer-list))

                   ;; remove from window-stool-buffer-list if buffer iskilled
                   (add-hook 'kill-buffer-hook (lambda () (cl-remove (current-buffer) window-stool-buffer-list)))

                   ;; prevents a (void-function: nil) error when we switch to a non-hooked mode i.e. in fundamental mode,
                   ;; which will break the global window-scroll-functions' window-stool--scroll-function
                   ;; therefore breaking window-stool for all other buffers
                   (make-local-variable 'window-scroll-functions)
                   (add-to-list 'window-scroll-functions #'window-stool--scroll-function))
               (progn
                 ;; cancel the timer if we're using the "window" version
                 (when (timerp window-stool-timer)
                   (cancel-timer window-stool-timer)
                   (setq window-stool-timer nil))

                 (setq window-stool--prev-window-min-height window-min-height)
                 (setq window-min-height 0)
                 (window-divider-mode -1)
                 (add-hook 'window-selection-change-functions #'window-stool--selection-change-function)
                 (add-hook 'post-command-hook #'window-stool-window--create)
                 (window-stool-window--advise-window-functions))))
    ;; clean up overlay stuff
    (progn (remove-overlays (point-min) (point-max) 'type 'window-stool--buffer-overlay)
           (remove-hook 'post-command-hook #'window-stool--scroll-overlay-into-position t)
           (setq window-scroll-functions
                 (remove #'window-stool--scroll-function window-scroll-functions))
           (setq window-stool-buffer-list (cl-remove (current-buffer) window-stool-buffer-list))
           (kill-local-variable 'scroll-margin)
           (advice-remove #'window-resize #'window-stool--window-resize-before-advice)
           (advice-remove #'window-resize #'window-stool--window-resize-after-advice)

           ;; cleanup window stuff
           (when (boundp 'window-stool--prev-window-min-height) (setq window-min-height window-stool--prev-window-min-height))
           (remove-hook 'post-command-hook #'window-stool-window--create)
           (window-stool-window--delete)
           (window-stool-window--remove-window-function-advice))))

(provide 'window-stool)
;;; window-stool.el ends here

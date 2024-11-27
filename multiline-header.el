;;; multiline-header.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jason Zhen
;;
;; Author: Jason Zhen <jaszhe@gmail.com>
;; Maintainer: Jason Zhen <jaszhe@gmail.com>
;; Created: November 12, 2024
;; Modified: November 12, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/jasonzhen/multiline-header
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;  Small library to support pseudo multiline headerlines using overlays.
;;; Code:

(defgroup multiline-header nil
  "A minor mode for simulating a multiline header via overlays."
  :group 'tools)

(defface multiline-header-face
  '((t (:inherit fringe :extend t)))
  "Face for window stool overlay background.
This will be ADDED to the context string's existing buffer font locking.")

(defcustom multiline-header-ignore-buffer-regexps nil
  "List of buffer regexps to disable window-stool overlay on.
Different from `window-stool-ignore-file-regexps'"
  :type '(repeat regexp))

(defcustom multiline-header-ignore-file-regexps '("\\.git")
  "List of file name regexps to disable window-stool overlay on.
Different from `window-stool-ignore-buffer-regexps'"
  :type '(repeat regexp))

(defvar-local multiline-header-overlay nil
  "The multiline header overlay.")

(defvar-local multiline-header-prev-window-start nil
  "The previous window-start. So we don't run the overlay creation unnecessarily.")

(defvar-local multiline-header--header-text nil
  "The header text.")

(defvar-local multiline-header--prev-header-text nil
  "The previous header text.")

(defvar-local multiline-header--prev-window-start nil
  "The previous window start. Used to fix scrolling issues.")

(defvar-local multiline-header--window-min-height 20
  "Minimum height (arbitrarily chosen) that a window needs to have for header to be displayed.
This is a hack to prevent some issues with resizing a window causing Emacs to freeze in the redisplay code.")

(defvar-local multiline-header--window-min-width 50
  "Minimum width (arbitrarily chosen) that a window needs to have for header to be displayed.
This is a hack to prevent some issues with resizing a window causing Emacs to freeze in the redisplay code.")

(defun multiline-header--set-text (text)
  (setq-local multiline-header--prev-header-text multiline-header--header-text)
  (setq-local multiline-header--header-text text))

(defun multiline-header--create (window display-start)
  "Create/move the `multiline-header-overlay' to DISPLAY-START of WINDOW.
Contents of overlay from `multiline-header--header-text'.
Intended to be used as in `window-scroll-functions'."
(when (eq window (selected-window))
    (unless multiline-header-overlay (setq-local multiline-header-overlay (make-overlay 1 1)))
    (if (and (or (<= (window-size window) multiline-header--window-min-height)
                 (<= (window-size window t) multiline-header--window-min-width)
                 (eq display-start (point-min))))
        (delete-overlay multiline-header-overlay)
      (progn
        ;; Some git operations i.e. commit/rebase open up a buffer that we can edit which is based a temporary file in the .git directory. Most of the time I don't really want the overlay in those buffers so I've opted to disable them here via this simple heuristic.
        (when (and (not (or
                         (cl-find-if (lambda (r) (and buffer-file-name (string-match r buffer-file-name)))
                                     window-stool-ignore-file-regexps)
                         (cl-find-if (lambda (r) (string-match r (buffer-name)))
                                     window-stool-ignore-file-regexps))))
          (let* ((ctx-1 (save-excursion (funcall window-stool-fn display-start)))
                 (ctx (window-stool--truncate-context ctx-1)))
            (let* ((ol-beg-pos display-start)
                   (ol-end-pos (save-excursion
                                 (goto-char display-start)
                                 (forward-visible-line 1)
                                 (line-end-position)))
                   ;; There's some bugginess if we don't have end-pos be on the next line,
                   ;; cause depending on the order of operations we might scroll past our overlay after redisplay.
                   ;; The solution here is to make the overlay 2 lines and just show
                   ;; the "covered" second line as part of the overlay
                   (covered-line (save-excursion
                                   (goto-char display-start)
                                   (forward-visible-line 1)
                                   (buffer-substring
                                    (line-beginning-position)
                                    (line-end-position))))
                   (context-str-1 (when ctx (cl-reduce (lambda (acc str)
                                                         (let* ((truncated (truncate-string-to-width str (1- (window-size window t)) 0 nil "\n"))
                                                                (truncated-respecting-word-boundaries
                                                                 (if (string= truncated str) ;; this means we didn't need to truncate
                                                                     truncated
                                                                   (truncate-string-to-width truncated
                                                                                             (- (length truncated) (1+ (string-match "[[:space:]]" (reverse truncated))))
                                                                                             0 nil "\n"))))
                                                           (concat acc truncated-respecting-word-boundaries)))
                                                       ctx)))

                   (context-str (progn
                                  (add-face-text-property 0 (length context-str-1) '(:inherit window-stool-face) t context-str-1)
                                  (concat context-str-1 covered-line))))

              (when multiline-header-overlay
                (move-overlay multiline-header-overlay ol-beg-pos ol-end-pos)
                (overlay-put multiline-header-overlay 'type 'window-stool--buffer-overlay)
                (overlay-put multiline-header-overlay 'priority 0)
                (overlay-put multiline-header-overlay 'display context-str))
              )
            (setq window-stool--prev-ctx ctx))))))
  )


(defun multiline-header--pre-command-hook ()
  "Fixes an issue with scrolling down a single line by simply deleting the overlay.
The idea is that the scrolling redisplay logic can then properly calculate where the
next scroll position should be without our multiline overlay getting in the way.
This fixes the issue with the overlay when multiple windows show the same buffer."
  (when (and (overlayp window-stool-overlay)
             (or (eq this-command 'evil-scroll-line-down)
                 (eq this-command 'viper-scroll-up-one)
                 (eq this-command 'scroll-up-line)))
    (delete-overlay multiline-header-overlay)))


(defun multiline-header--scroll ()
  "Fixes some bugginess with scrolling getting stuck when the overlay large.
Conditions:
- the multiline-header is enabled
- window size is above `multiline-header--window-min-width' and `multiline-header--window-min-height'
- we've actually scrolled
- file visiting buffers only"
  (when (and multiline-header
             (overlay-buffer window-stool-overlay)
             (> (window-size (selected-window)) multiline-header--window-min-height)
             (> (window-size (selected-window) t) multiline-header--window-min-width)
             (not (eq (window-start) multiline-header--prev-window-start))
             (buffer-file-name))
    (ignore-errors
      (when (and multiline-header--header-text
                 (memq last-command (list 'evil-scroll-line-up
                                          'viper-scroll-down-one
                                          'scroll-down-line)))
        (forward-visible-line (- (+ (min (- (length multiline-header--header-text)
                                            (length multiline-header--prev-header-text))
                                         0)
                                    1)))
        ;; So we don't need to double scroll when window start is in the middle of a visual line split
        (when (= (save-excursion
                   (goto-char (window-start))
                   (line-beginning-position))
                 (save-excursion
                   (goto-char (window-start))
                   (line-move-visual -1 t)
                   (line-beginning-position)))
          (scroll-down-line))))))

;;;###autoload
(define-minor-mode multiline-header-mode
  "Minor mode to enable a multiline header.
Uses overlays and hooks into post-command-hook which can hamper performance."
  :lighter "MLine-Header"
  :group 'multiline-header
  (when multiline-header-mode
    (add-hook 'pre-command-hook #'multiline-header--pre-command-hook nil t)
    (add-hook 'post-command-hook #'multiline-header--scroll nil t)
    )
  (unless multiline-header-mode
    (remove-hook 'post-command-hook #'multiline-header--scroll t)
    (remove-hook 'pre-command-hook #'multiline-header--pre-command-hook)
    )
  )

(provide 'multiline-header)
;;; multiline-header.el ends here

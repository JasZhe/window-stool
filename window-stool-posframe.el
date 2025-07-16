
;; general idea
;; create a posframe buffer for each window i.e. on configuration change
;; hook into window state functions to refresh the posframe
;; profit


(require 'window-stool)


(defun test-posframe-stuff (&rest args)
  (message "%S" (window-list))
  )

(defvar window-stool--posframe-buffers (make-hash-table :test 'equal))

(defun ws/-get-key-for-window (window)
  (let ((win-string (prin1-to-string window)))
    (if (string-match "#<window \\([0-9]+\\)" win-string)
        (format "#<window %s>" (match-string 1 win-string))
      win-string)))

(defun ws/-add-posframe-buffer-for-window (window)
  "Returns the buffer created."
  (let* ((win-string (window-stool--get-key-for-window window))
         (buf (get-buffer-create (concat "*window-stool-posframe" win-string))))
    (puthash win-string buf window-stool--posframe-buffers)
    buf))

(defun ws/-create-posframe-buffers ()
  (maphash (lambda (k v) (kill-buffer v)) window-stool--posframe-buffers)
  (setq window-stool--posframe-buffers (make-hash-table :test 'equal))

  (window-stool--create-posframe (selected-window) (point))
  (dolist (win (window-list))
    (window-stool--add-posframe-buffer-for-window win)))

(defun ws/-create-posframe (window display-start)
  (let* ((display-start (save-excursion (goto-char display-start) (line-beginning-position)))
         (posframe-buf? (gethash (window-stool--get-key-for-window window) window-stool--posframe-buffers))
         (posframe-buf (if posframe-buf? posframe-buf? (window-stool--add-posframe-buffer-for-window window)))
         (ctx-1 (save-excursion (funcall window-stool-fn display-start)))
         (ctx (window-stool--truncate-context ctx-1))
         (context-str (when ctx (cl-reduce (lambda (acc str) (concat acc str)) ctx)))
         )
    (with-current-buffer posframe-buf
      (erase-buffer)
      (insert context-str)
      )
    (posframe-show posframe-buf
               :poshandler #'window-stool-posframe--poshandler
               :border-width 0
               :right-fringe (cadr (window-fringes))
               :internal-border-width 0
               :min-width (window-width)
               :min-height 0
               )
    ))

(add-to-list 'window-scroll-functions #'window-stool--create-posframe)
(setq window-scroll-functions
                 (remove #'window-stool--create-posframe window-scroll-functions))

(with-current-buffer (get-buffer-create "random-text")
  (erase-buffer)
  (insert "ahhh\n")
  (insert "wee\n")
  (insert "\n")
  (insert "woo\n")
  )




(defun window-stool-posframe--poshandler (info)
    (cons (+ (plist-get info :parent-window-left)
             (car (window-fringes)))
          (plist-get info :parent-window-top)))

(posframe-show (get-buffer "random-text")
               :poshandler #'window-stool-posframe--poshandler
               :border-width 0
               :right-fringe (cadr (window-fringes))
               :internal-border-width 0
               :min-width (window-width)
               :min-height 0
               )

(posframe-hide-all)


;; window-stool-posframe.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("ws/" . "window-stool-") (":ws/" . ":window-stool-"))
;; package-lint--sane-prefixes: "^ws/"
;; End:

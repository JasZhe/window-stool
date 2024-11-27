(defconst window-stool-dummy-test-file "dummy.el")

(defun window-stool-test-setup ()
  (let ((buffer (find-file-noselect "dummy.el")))
    buffer
    )
  )


(require :sb-posix)
(require :asdf)
(require :gk-trees)

;;; sbcl
(sb-ext:save-lisp-and-die
 "lasso"
 :toplevel (lambda ()
             ;; asdf requires sbcl_home to be set, so set it to the value when
             ;; the image was built
             (sb-posix:putenv
              (format nil "SBCL_HOME=~A" #.(sb-ext:posix-getenv "SBCL_HOME")))
             (gk-trees:main sb-ext:*posix-argv*))
 :executable t
 :compression t)


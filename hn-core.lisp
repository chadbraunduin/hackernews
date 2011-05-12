(require :asdf)
(require :drakma)
(require :cl-json)
(require :cl-ppcre)

;; main program
(defun start-main (&key (debug nil))
  (declaim #+sbcl(sb-ext:muffle-conditions style-warning))
  (when debug
    (setf *uses-colored-text* nil))
  (load "user-settings.lisp")
  (load "utilities.lisp")
  (load "hn.lisp")
  (declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))
  (main (build-home-page #'hn-news-url)))

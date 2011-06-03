(require :asdf)
(require :drakma)
(require :cl-json)
(require :cl-ppcre)
(asdf:operate 'asdf:load-op :uffi)
(require 'uffi)
(asdf:oos 'asdf:load-op 'cl-ncurses)

;; main program
(defun start-main ()
  (load "ncurses.lisp"))

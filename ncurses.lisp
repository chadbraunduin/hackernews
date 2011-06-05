
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

(in-package :cl-ncurses)

;; load other files
(load "user-settings.lisp")
(load "utilities.lisp")
(load "hn.lisp")

;; initialize ncurses
(initscr)
(start-color)
(assume-default-colors color_white color_black) ;;these two lines set the background black
(cbreak)
(noecho)

;; colors
(defparameter *banner-color-number* 1)
(init-pair *banner-color-number* color_black color_white)

(defparameter *highlight-color-number* 2)
(init-pair *highlight-color-number* color_cyan color_black)

(defparameter *error-color-number* 3)
(init-pair *error-color-number* color_white color_red)

;; windows and pads
(defparameter bannerwin nil)
(defparameter mypad nil) ;;scrolling area where the data goes
(defparameter instrwin nil)
(defparameter cmdwin nil)
(defparameter hndlwin nil)

;; other globals
(defparameter curmaxx 0)
(defparameter curmaxy 0)

;; helper functions
(defun pad-visible-lines ()
  (- (getmaxy *stdscr*) 8))
(defun instrwiny ()
  (- (getmaxy *stdscr*) 5))
(defun cmdwiny ()
  (- (getmaxy *stdscr*) 2))
(defun hndlwiny ()
  (- (getmaxy *stdscr*) 1))

(defun lines-needed (char-count max-width)
  (if (zerop char-count)
      1
      (multiple-value-bind (q r) (truncate char-count max-width)
	(if (zerop r)
	    q
	    (1+ q)))))

(defun pad-lines-needed (items max-width)
  (reduce #'+
	  (mapcar (lambda (item)
		    (reduce #'+
			    (mapcar (lambda (str) (lines-needed (length str) max-width)) item)))
		  items)))

(defun build-screen (page &optional (text nil))
  (clear)
  (refresh)
  (erase) ;; start by blanking out the screen
  ;; set current screen values
  (setf curmaxy (getmaxy *stdscr*))
  (setf curmaxx (getmaxx *stdscr*))
  
  ;; print banner and subtitle
  (setf bannerwin (newwin 4 curmaxx 0 0))
  (wattron bannerwin (color-pair *banner-color-number*))
  (mvwprintw bannerwin 1 0 (format nil "Hacker news - ~a" (hn-page-title page)))
  (wattroff bannerwin (color-pair *banner-color-number*))
  (mvwprintw bannerwin 2 0 (subtitle-str page))

  ;; build pad
  (let* ((items (printable-items page))
	 (total-lines-needed (pad-lines-needed items curmaxx)))
    (setf (hn-page-total-lines-needed page) total-lines-needed)
    (setf mypad (newpad total-lines-needed curmaxx))
    (wattron mypad (color-pair *highlight-color-number*))
    (loop for item in items
       do
	 (loop for line in item
	    do
	      (loop for char across line
		 do
		   (waddch mypad (char-code char)))
	      (waddch mypad (char-code #\newline))
	      ))
    (wattroff mypad (color-pair *highlight-color-number*)))

  ;; print instructions
  (setf instrwin (newwin 2 curmaxx (instrwiny) 0))
  (wprintw instrwin (instructions-str page))

  ;; command window
  (mvprintw (cmdwiny) 0 ">")
  (setf cmdwin (newwin 1 curmaxx (cmdwiny) 2))
  (when text
    (wprintw cmdwin (text-to-str text))) ;; reposition the text back into the command bar

  ;; handle command window
  (setf hndlwin (newwin 1 curmaxx (hndlwiny) 0))

  ;; refresh all windows and pads
  (refresh)
  (wrefresh bannerwin)
  (prefresh mypad (hn-page-scroll-pos page) 0 4 0 (pad-visible-lines) curmaxx)
  (wrefresh instrwin)
  (wrefresh cmdwin)
  (wrefresh hndlwin)

  ;; position the cursor
  (wmove cmdwin (getcury cmdwin) (getcurx cmdwin)))

(defun print-error (text)
  (wclear hndlwin)
  (wattron hndlwin (color-pair *error-color-number*))
  (wprintw hndlwin text)
  (wattroff hndlwin (color-pair *error-color-number*))
  (wrefresh hndlwin))

(defun get-scroll-dir ()
  (let ((ch (wgetch cmdwin)))
    (cond
      ((eq ch (char-code #\O)) ;; home and end keys
       (let ((ch (wgetch cmdwin)))
	 (cond((eq ch (char-code #\H)) 'home)
	      ((eq ch (char-code #\F)) 'end))))
      ((eq ch (char-code #\[)) ;; arrow keys and whatnot
       (let ((ch (wgetch cmdwin)))
	 (cond ((eq ch (char-code #\B)) 'down)
	       ((eq ch (char-code #\A)) 'up)
	       ((eq ch (char-code #\6))
		(wgetch cmdwin) ;; clean off trailing ~
		'page-down)
	       ((eq ch (char-code #\5))
		(wgetch cmdwin) ;; clean off trailing ~
		'page-up)))))))

(defun pad-scroll (page dir text)
  (let* ((total-lines-needed (hn-page-total-lines-needed page))
	 (scroll-pos (hn-page-scroll-pos page))
	 (end-position (- total-lines-needed
			  (- (pad-visible-lines)
			     (length (car (reverse (printable-items page))))))))
    (when (or (eq dir 'home)
	      (eq dir 'end)
	      (and (< scroll-pos end-position)
		   (or (eq dir 'down)
		       (eq dir 'page-down)))
	      (and (> scroll-pos 0)
		   (or (eq dir 'up)
		       (eq dir 'page-up))))
      (let ((dir-func (cond ((eq dir 'down) (lambda () (1+ scroll-pos)))
			    ((eq dir 'page-down) (lambda ()
						   (let ((new-pos (+ scroll-pos (pad-visible-lines))))
						     (if (>= new-pos end-position)
							 end-position
							 new-pos))))
			    ((eq dir 'up) (lambda () (1- scroll-pos)))
			    ((eq dir 'page-up) (lambda ()
						 (let ((new-pos (- scroll-pos (pad-visible-lines))))
						   (if (< new-pos 0)
						       0
						       new-pos))))
			    ((eq dir 'home) (lambda () 0))
			    ((eq dir 'end) (lambda () end-position))
			    (t #'identity)
			    )))
	(setf (hn-page-scroll-pos page) (funcall dir-func)))
      ;; erase cruft at end of window by rebuilding the screen
      (let ((scroll-pos (hn-page-scroll-pos page)))
	(when (> (pad-visible-lines) (- total-lines-needed scroll-pos))
	  (build-screen page text))
	(prefresh mypad scroll-pos 0 4 0 (pad-visible-lines) curmaxx)))))

(defun main (page &optional (text nil))
  (if (not (and (eq curmaxx (getmaxx *stdscr*))
		(eq curmaxy (getmaxy *stdscr*))))
      (progn
	(build-screen page text)
	(main page text)) ;; rebuild the screen if the terminal width or height changed
      (let ((ch (wgetch cmdwin)))
	(cond ((eq ch 27) ;; 27 is the escape code for arrow keys, home, end, and page keys
	       (let ((y (getcury cmdwin))
		     (x (getcurx cmdwin))
		     (dir (get-scroll-dir)))
		 (when dir
		   (progn
		     (funcall #'pad-scroll page dir text)
		     (wmove cmdwin y x))))
	       (main page text)) ;; scrolling / paging
	      ((eq ch (char-code #\newline))
	       (wclear cmdwin)
	       (wclear hndlwin)
	       (let ((cmd (text-to-str text)))
		 (if (equal cmd "q")
		     (endwin) ;; end the program
		     (handler-case
			 (progn
			   (wclear cmdwin)
			   (wclear hndlwin)
			   (wprintw hndlwin "Loading...")
			   (wrefresh hndlwin)
			   (let* ((new-page (handle-cmd cmd page))
				  (message (hn-page-message page)))
			     (build-screen new-page)
			     (when message
			       (print-error message)
			       (setf (hn-page-message page) nil))
			     (main new-page)))
		       (error (e)
			 (progn
			   (print-error (format nil "~S" e))
			   (main page))
			 ))))) ;; <enter> was clicked
	      ((or (eq ch (char-code #\delete))
		   (eq ch (char-code #\backspace)))
	       (let ((y (getcury cmdwin))
		     (x (getcurx cmdwin)))
		 (mvwdelch cmdwin y (1- x)))
	       (wrefresh cmdwin)
	       (main page (cdr text))) ;; <backspace> was clicked
	      ((and (>= ch 32)
		    (<= ch 126))
	       (waddch cmdwin ch)
	       (wrefresh cmdwin)
	       (main page (cons ch text))) ;; printable characters
	      (t (main page text))	    ;; anything else was clicked
	      ))))

;; launch the application here
(let ((page (build-home-page #'hn-news-url)))
  (build-screen page)
  (main page))
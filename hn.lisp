
;; constants
(defparameter *api-url* "http://api.ihackernews.com")
(defparameter *front-page* "page")
(defparameter *newly-submitted* "new")
(defparameter *profile* "profile")
(defparameter *post* "post")
(defparameter *posts-per-page* 30)
(defparameter *comments-per-page* 3)
(defparameter *front-page-title* "Front Page")
(defparameter *newest-page-title* "Newest")

(defun hn-posts-url (page &optional next-id)
  (let ((base-url (format nil "~a/~a" *api-url* page)))
    (if next-id
	(format nil "~a/~a" base-url next-id)
	base-url)))

(defun hn-news-url (&optional next-id)
  (hn-posts-url "page" next-id))

(defun newest-url (&optional next-id)
  (hn-posts-url "new" next-id))

(defun user-url (username)
  (format nil "~a/~a/~a"
	  *api-url*
	  *profile*
	  username))

(defun comments-url (item-id)
  (format nil "~a/~a/~a"
	  *api-url*
	  *post*
	  item-id))

(defstruct hn-page
  url
  (title *front-page-title*)
  message)

(defstruct (home-page (:include hn-page))
  (items nil)
  (next-ids '())
  (index 0))

(defstruct (user-page (:include hn-page))
  user
  (back-page nil))

(defstruct (comments-page (:include hn-page))
  (comments nil)
  (index 0)
  (back-page nil))

(defstruct hn-item
  title
  url
  id
  comment-count
  points
  posted-ago
  posted-by)

(defstruct hn-user
  username
  created-ago
  karma
  about
  version
  cached-on-+utc+)

(defstruct hn-comment
  posted-by
  posted-ago
  comment
  id
  points
  parent-id
  post-id
  children
  (nesting-level 0))

(defun flatten-alist (alst)
  (mapcan (lambda (x) (list (car x) (cdr x))) alst))

(defun build-home-page (url &optional next-id page-index title)
  (let* ((page (make-home-page :url url))
	 (output (url-output (funcall (hn-page-url page) next-id)))
	 (next-id (cdar output))
	 (raw-items (cdr (cadr output))))
    (when (not (member next-id (home-page-next-ids page)))
      (setf (home-page-next-ids page)
	    (cons next-id (home-page-next-ids page))))
    (when page-index
      (setf (home-page-index page) page-index))
    (when title
      (setf (hn-page-title page) title))
    (setf (home-page-items page) (map 'vector
				      (lambda
					  (raw-item)
					(apply #'make-hn-item raw-item))
				      (mapcar
				       #'flatten-alist
				       raw-items)))
    page))

(defun poster (username items)
  (let ((posters (remove-if-not
		  (lambda (item)
		    (insensitive-equal username (hn-item-posted-by item)))
		  (coerce items 'list))))
    (when posters
      (car posters))))

(defun build-user-page (username back-page)
  (let* ((url (user-url username))
	 (output (url-output url))
	 (user (apply #'make-hn-user (flatten-alist output))))
    (make-user-page :url url :title username :user user :back-page back-page)))

(defun build-comments-page (item &optional page-index back-page)
  (let* ((item-id (hn-item-id item))
	 (url (comments-url item-id))
	 (page (make-comments-page :url url :title (hn-item-title item) :back-page back-page))
	 (raw-comments (cdr (car (cdr (url-output url))))))
    (when page-index
      (setf (comments-page-index page) page-index))
    (setf (comments-page-comments page) (build-comments raw-comments 0))
    page))

(defun build-comments (raw-comments nesting-level)
  (when raw-comments
    (let* ((comment (apply #'make-hn-comment
			   (append (list :nesting-level nesting-level)
				   (flatten-alist (car raw-comments)))))
	   (children (hn-comment-children comment)))
      (append
       (cons
	comment
	(when children
	  (let ((child-comments (build-comments children (1+ nesting-level))))
	    (setf (hn-comment-children comment) nil)
	    child-comments)))
       (build-comments (cdr raw-comments) nesting-level)))))

(defun more-comments (page)
  (let* ((new-index (1+ (comments-page-index page)))
	 (new-start (* new-index *comments-per-page*)))
    (when (< new-start (length (comments-page-comments page)))
      (setf (comments-page-index page) new-index))
    page))

(defun prev-comments (page)
  (let ((new-index (1- (comments-page-index page))))
    (when (>= new-index 0)
      (setf (comments-page-index page) new-index))
    page))

(defun commenter (username comments)
  (let ((commenters (remove-if-not
		     (lambda (comment)
		       (insensitive-equal username (hn-comment-posted-by comment)))
		     comments)))
    (when commenters
      (car commenters))))

(defun page-back (page)
  (let ((next-id (car (cdr (home-page-next-ids page))))
	(new-page-index (1- (home-page-index page))))
    (setf (home-page-next-ids page) (cdr (home-page-next-ids page)))
    (if (< new-page-index 0)
	page
	(build-home-page (hn-page-url page) next-id new-page-index))))

(defun page-forward (page)
  (let ((next-id (car (home-page-next-ids page)))
	(new-page-index (1+ (home-page-index page))))
    (build-home-page (hn-page-url page) next-id new-page-index)))

(defun validpostnumberp (post-number page)
  (let* ((page-index (home-page-index page))
	 (lower-post (* *posts-per-page* page-index))
	 (upper-post (+ *posts-per-page* lower-post)))
    (and (> post-number lower-post)
	 (<= post-number upper-post))))

(defun get-item (post-number-str page)
  (let* ((post-number (parse-integer post-number-str))
	 (mod-post-number (mod post-number *posts-per-page*))
	 (adj-post-index (1- (if (zerop mod-post-number)
				 *posts-per-page*
				 mod-post-number))))
    (aref (home-page-items page) adj-post-index)))

(defun post-number (post-index page-index posts-per-page)
  (+ (1+ post-index) (* page-index posts-per-page)))

(defmethod print-content (page))

(defmethod print-content ((page home-page))
  (let ((page-index (home-page-index page))
	(items (home-page-items page)))
    (loop for item across items
       for index from 0
       do (print-item page-index index item))))

(defun print-item (page-index index item)
  (fresh-line)
  (format t "~d. ~a \(~a\) | "
	  (post-number index page-index *posts-per-page*)
	  (hn-item-title item)
	  (url-text (short-url item)))
  (format t "~d points by ~a. ~a \| ~d comments~%"
	  (hn-item-points item)
	  (hn-item-posted-by item)
	  (hn-item-posted-ago item)
	  (hn-item-comment-count item)))

(defmethod print-content ((page user-page))
  (let ((user (user-page-user page)))
    (print-user user)))

(defun print-user (user)
  (format t "user: ~a~%" (hn-user-username user))
  (format t "created: ~a~%" (hn-user-created-ago user))
  (format t "karma: ~a~%" (hn-user-karma user))
  (format t "about: ~a~%" (clean-html-str (hn-user-about user))))

(defmethod print-content ((page comments-page))
  (let* ((comments (comments-page-comments page))
	 (start-index (* (comments-page-index page) *comments-per-page*))
	 (end-index (+ start-index *comments-per-page*))
	 (comment-count (length (comments-page-comments page)))
	 (end-index (if (> end-index comment-count) comment-count end-index)))
    (mapc #'print-comment (subseq comments start-index end-index))))

(defun print-comment (comment)
  (let ((nesting-level (hn-comment-nesting-level comment)))
    (when (> nesting-level 0)
      (princ (nesting-level-text (format nil "~d " nesting-level)))
      (princ
       (nesting-level-text
	(apply
	 #'concatenate
	 'string
	 (loop repeat nesting-level collect ">> "))))))
  (format t "~&~a | ~a~%" (hn-comment-posted-by comment) (hn-comment-posted-ago comment))
  (format t "~a~%~%" (clean-html-str (hn-comment-comment comment))))

(defmethod print-instructions (page))

(defmethod print-instructions ((page home-page))
  (format t "~%[h]acker-news; [n]ewest; [r]eload-page; [f]orward-page; [b]ack-page; [q]uit~%")
  (format t "<number> to read post; [c]+<number> to view comments; <username> to view user~%"))

(defmethod print-instructions ((page user-page))
  (format t "~%[b]ack; [q]uit~%"))

(defmethod print-instructions ((page comments-page))
  (format t "[m]ore comments; [p]rev comments; [b]ack; [q]uit~%")
  (format t "[p]+<number> to change comments per page; <username> to view user~%"))

(defmethod print-subtitle (page)
  (terpri))

(defmethod print-subtitle ((page comments-page))
  (let* ((comment-count (length (comments-page-comments page)))
	 (total-pages (truncate (/ comment-count *comments-per-page*)))
	 (total-pages (if (zerop (rem comment-count *comments-per-page*))
			  total-pages
			  (1+ total-pages))))
    (format t "~d total comments | ~d comments per page | page ~d of ~d~%~%"
	    comment-count
	    *comments-per-page*
	    (1+ (comments-page-index page))
	    total-pages)))

(defun print-page (page)
  (format t (banner-text
	     (format nil "~%Hacker News - ~a ~%" (hn-page-title page))))
  (print-subtitle page)
  (print-content page)
  (print-instructions page)
  (let ((message (hn-page-message page)))
    (when message
      (format t "~%~a~%" (error-text message))))
  (format t "~%> ")
  (finish-output))

(defun add-message (message page)
  (setf (hn-page-message page) message)
  page)

(defun main (page)
  (clear-terminal)
  (print-page page)
  (let ((cmd (string-downcase (string-trim " " (read-line)))))
    (if (equal cmd "q")
	(format t "Thank you for using Hacker News. Good-bye~%")
	(progn
	  (format t "Loading...")
	  (finish-output)
	  (main
	   (handler-case
	       (handle-cmd cmd page)
	     (error (e)
	       (add-message (princ e) page))))))))

(defmethod handle-cmd (cmd page))

(defmethod handle-cmd (cmd (page home-page))
  (let ((post (poster cmd (home-page-items page)))
	(cmd-lst (coerce cmd 'list)))
    (cond
      (post
       (build-user-page (hn-item-posted-by post) page))
      ((equal cmd "h")
       (build-home-page #'hn-news-url))
      ((equal cmd "n")
       (build-home-page #'newest-url nil nil *newest-page-title*))
      ((equal cmd "r")
       (build-home-page (hn-page-url page)))
      ((equal cmd "f")
       (page-forward page))
      ((equal cmd "b")
       (page-back page))
      ((and (integerlistp cmd-lst)
	    (validpostnumberp (parse-integer cmd) page))
       (let* ((item (get-item cmd page))
	      (url (hn-item-url item)))
	 (browse url)
	 page))
      ((and (equal #\c (car cmd-lst))
	    (integerlistp (cdr cmd-lst)))
       (build-comments-page (get-item (coerce (cdr cmd-lst) 'string) page) nil page)) 
      (t (handle-default cmd page)))))

(defmethod handle-cmd (cmd (page user-page))
  (cond
    ((equal cmd "b")
     (user-page-back-page page))
    (t (handle-default cmd page))))

(defmethod handle-cmd (cmd (page comments-page))
  (let ((comment (commenter cmd (comments-page-comments page)))
	(cmd-lst (coerce cmd 'list)))
    (cond
      (comment
       (build-user-page (hn-comment-posted-by comment) page))
      ((equal cmd "m")
       (more-comments page))
      ((equal cmd "p")
       (prev-comments page))
      ((equal cmd "b")
       (comments-page-back-page page))
      ((and (equal #\p (car cmd-lst))
	    (integerlistp (cdr cmd-lst)))
       (let* ((comments-per-page (parse-integer (coerce (cdr cmd-lst) 'string)))
	      (comment-count (length (comments-page-comments page)))
	      (comments-per-page (cond ((<= comments-per-page 0) 1)
				       ((> comments-per-page comment-count) comment-count)
				       (t comments-per-page))))
	 (setf *comments-per-page* comments-per-page)
	 (setf (comments-page-index page) 0))
       page)
      (t (handle-default cmd page)))))

(defun handle-default (cmd page)
  (cond
    ((equal cmd "")
     page)
    (t
     (add-message (format nil "~a is not a valid command" cmd) page))))


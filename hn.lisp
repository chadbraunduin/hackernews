
;; constants
(defparameter *api-url* "http://api.ihackernews.com")
(defparameter *front-page* "page")
(defparameter *newly-submitted* "new")
(defparameter *profile* "profile")
(defparameter *post* "post")
(defparameter *posts-per-page* 30)
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
  (message nil)
  (title *front-page-title*)
  (scroll-pos 0)
  (total-lines-needed 0))

(defstruct (home-page (:include hn-page))
  (items nil)
  (next-ids '())
  (index 0))

(defstruct (user-page (:include hn-page))
  user
  (back-page nil))

(defstruct (comments-page (:include hn-page))
  (comments nil)
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

;; news page methods
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
		    (string-equal username (hn-item-posted-by item)))
		  (coerce items 'list))))
    (when posters
      (car posters))))

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

;; user page methods
(defun build-user-page (username back-page)
  (let* ((url (user-url username))
	 (output (url-output url))
	 (user (apply #'make-hn-user (flatten-alist output))))
    (make-user-page :url url :title username :user user :back-page back-page)))

;; comment page methods
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

(defun build-comments-page (item &optional page-index back-page)
  (let* ((item-id (hn-item-id item))
	 (url (comments-url item-id))
	 (page (make-comments-page :url url :title (hn-item-title item) :back-page back-page))
	 (raw-comments (cdr (car (cdr (url-output url))))))
    (when page-index
      (setf (comments-page-index page) page-index))
    (setf (comments-page-comments page) (build-comments raw-comments 0))
    page))

(defun commenter (username comments)
  (let ((commenters (remove-if-not
		     (lambda (comment)
		       (string-equal username (hn-comment-posted-by comment)))
		     comments)))
    (when commenters
      (car commenters))))

;; prepare data for the screen
(defmethod printable-items (page))

(defmethod printable-items ((page home-page))
  (let ((items (home-page-items page)))
    (labels ((comment-count-str (count)
	       (if (eq count 1)
		   "1 comment"
		   (format nil "~d comments" count))))
      (loop for item across items
	 for n from 0
	 collect (list
		  (format nil "~d. ~a \(~a\)"
			  (post-number n
				       (home-page-index page)
				       *posts-per-page*)
			  (hn-item-title item)
			  (short-url item))
		  (format nil "~d points by ~a ~a | ~a"
			  (hn-item-points item)
			  (hn-item-posted-by item)
			  (hn-item-posted-ago item)
			  (comment-count-str (hn-item-comment-count item)))
		  "" ;; blank line for spacing
		  )))))

(defmethod printable-items ((page user-page))
  (let ((user (user-page-user page)))
    (list
     (list
      (format nil "user: ~a" (hn-user-username user))
      (format nil "created: ~a" (hn-user-created-ago user))
      (format nil "karma: ~d" (hn-user-karma user))
      (format nil "about: ~a" (clean-html-str (hn-user-about user)))
      "" ;; blank line for spacing
      ))))

(defmethod printable-items ((page comments-page))
  (let ((comments (comments-page-comments page)))
    (labels ((nesting-level-str (nesting-level)
	       (apply
		#'concatenate
		'string
		(loop repeat nesting-level collect " >>"))))
      (loop for comment in comments
	 for nesting-level = (hn-comment-nesting-level comment)
	 collect
	   (append (when (> nesting-level 0)
		     (list
		      (format nil "~d~a"
			      nesting-level
			      (nesting-level-str nesting-level))))
		   (list
		    (format nil "~a ~a"
			    (hn-comment-posted-by comment)
			    (hn-comment-posted-ago comment))
		    (clean-html-str (hn-comment-comment comment))
		    "" ;; blank line for spacing
		    ))))))

(defmethod instructions-str (page) "")

(defmethod instructions-str ((page home-page))
  (format nil "[h]acker-news; [n]ewest; [r]eload-page; [m]ore-posts; [b]ack-page; [q]uit~%<number> to read post; [c]+<number> to view comments; <username> to view user"))

(defmethod instructions-str ((page user-page))
  (format nil "[b]ack; [q]uit"))

(defmethod instructions-str ((page comments-page))
  (format nil "[b]ack; [q]uit~%<username>to view user"))

(defmethod subtitle-str (page) "")

(defmethod subtitle-str ((page comments-page))
  (let ((comment-count (length (comments-page-comments page))))
    (format nil "~d total comments" comment-count)))

;; handle valid user commands
(defun handle-default (cmd page)
  (cond
    ((equal cmd "") page)
    (t
     (setf (hn-page-message page) (format nil "~a is not a valid command" cmd))
     page)))

(defmethod handle-cmd (cmd page))

(defmethod handle-cmd (cmd (page home-page))
  (let ((post (poster cmd (home-page-items page))))
    (cond
      (post
       (build-user-page (hn-item-posted-by post) page)) ;; view user page
      ((equal cmd "h")
       (build-home-page #'hn-news-url)) ;; front page
      ((equal cmd "n")
       (build-home-page #'newest-url nil nil *newest-page-title*)) ;; newest posts
      ((equal cmd "r")
       (build-home-page (hn-page-url page))) ;; refresh items
      ((equal cmd "m")
       (page-forward page)) ;; more posts
      ((equal cmd "b")
       (page-back page)) ; back a page
      ((and (not (string-equal cmd ""))
	    (integerlistp cmd)
	    (validpostnumberp (parse-integer cmd) page))
       (let* ((item (get-item cmd page))
	      (url (hn-item-url item)))
	 (browse url)
	 page)) ;; open a link
      ((and (not (string-equal cmd ""))
	    (equal (char cmd 0) #\c)
	    (integerlistp (subseq cmd 1))
	    (validpostnumberp (parse-integer (subseq cmd 1)) page))
       (build-comments-page (get-item (subseq cmd 1) page) nil page)) ;; view comments
      (t (handle-default cmd page)))))

(defmethod handle-cmd (cmd (page user-page))
  (cond
    ((equal cmd "b") (user-page-back-page page)) ;; back
    (t (handle-default cmd page))))

(defmethod handle-cmd (cmd (page comments-page))
  (let ((comment (commenter cmd (comments-page-comments page))))
    (cond
      (comment (build-user-page (hn-comment-posted-by comment) page)) ;;view user page
      ((equal cmd "b") (comments-page-back-page page)) ;; back
      (t (handle-default cmd page)))))

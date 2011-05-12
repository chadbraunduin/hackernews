
(defun run-cmd (cmd &rest args)
  (run-program cmd args :input t :output t :wait t))

(defun clear-terminal ()
  (run-cmd *clear-cmd*))

(defun browse (url)
  (run-cmd *browser-cmd*
	   "-accept_all_cookies "
	   url))

(defun insensitive-equal (str1 str2)
  (equal (string-downcase str1)
	 (string-downcase str2)))

(defun url-output (url)
  (let ((s (drakma:http-request url :want-stream t)))
    (json:decode-json-from-string (read-line s))))

(defun short-url (item)
  (let* ((without-front (ppcre:regex-replace
			 "http[s]*://(www.)*"
			 (hn-item-url item)
			 ""))
	 (short-url (ppcre:regex-replace
		     "/.*"
		     without-front
		     "")))
    short-url))

(defun clean-html-str (comment-str)
  (let* ((clean (ppcre:regex-replace "<[a-zA-Z]+.*?>" comment-str " "))
	 (clean (ppcre:regex-replace "</[a-zA-Z]+>" clean " ")))
    (if (not (equal clean comment-str))
	(clean-html-str clean)
	(string-trim " " clean))))

(defun integerlistp (lst)
  (notany #'alpha-char-p lst))

(defun colored-text (attr bgcolor fgcolor text)
  (if *uses-colored-text*
      (format nil "~c[~d;~d;~dm~a~c[m" #\ attr bgcolor fgcolor text #\)
      text))

(defun error-text (text)
  (colored-text 1 41 37 text))

(defun banner-text (text)
  (colored-text 1 47 30 text))

(defun url-text (text)
  (colored-text 4 0 36 text))

(defun nesting-level-text (text)
  (colored-text 0 0 36 text))